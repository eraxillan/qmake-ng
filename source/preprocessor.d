/****************************************************************************
**
** Copyright (C) 2018 Alexander Kamyshnikov
** Contact: axill777@gmail.com
**
** This file is part of the qmake-ng application, replacement of the Qt Toolkit one.
**
** qmake-ng is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** qmake-ng is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with qmake-ng.  If not, see <http://www.gnu.org/licenses/>.
**
****************************************************************************/

module preprocessor;

import std.typecons;

import std.experimental.logger;

import std.uni;
import std.algorithm;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;
import std.range;
static import std.regex;

import common_const;
import common_utils;

private const auto STR_ELSE = "else";
private const auto STR_ELSE_SINGLELINE = "else:";

private const auto EXISTS_FUNCTION_STR = "exists";      // test function, argument 1
private const auto CONTAINS_FUNCTION_STR = "contains";  // test function, argument 2
private const auto QTCONFIG_FUNCTION_STR = "qtConfig";  // test function, argument 1
private const auto ERROR_FUNCTION_STR = "error";        // test function, argument 1
private const auto SYSTEM_FUNCTION_STR = "system";      // test/replace function, argument 1

private const auto FIND_FUNCTION_STR = "find";          // replace function, argument 2
private const auto REESCAPE_FUNCTION_STR = "re_escape"; // replace function, argument 1
private const auto REPLACE_FUNCTION_STR = "replace";    // replace function, arguments 2 and 3


private struct QuotesInfo
{
    long indexOpen  = -1;
    long indexClose = -1;
    bool success;
}

private enum PreprocessorModifications
{
    None,
    WhitespaceStripped       = 1 << 0,
    TrailingSemicolonRemoved = 1 << 1,
    CommentRemoved           = 1 << 2,
    MultilineMerged          = 1 << 3,
    SinglelineScopeFixed     = 1 << 4,
    MultilineScopeFixed      = 1 << 5,
    SinglelineScopeElseFixed = 1 << 6,
    FunctionArgumentEnquoted = 1 << 7
} 

private enum ParenhesisType
{
    None,
    Opening,
    Closing
}

// FIXME: use this function instead of direct while loop
private void skipWhitespaces(in string sourceLine, ref long index)
{
    while (isWhite(sourceLine[index]) && (index < sourceLine.length))
        index++;
}

private bool isInsideParenthesis(in string sourceLine, in long index, in ParenhesisType parType,
    out long parIndex, bool findClosest = false)
{
    immutable long startIndex = (parType == ParenhesisType.Opening) ? 0     : (index + 1);
    immutable long endIndex   = (parType == ParenhesisType.Opening) ? index : sourceLine.length;

    long[] parenthesisStack;
    for (long i = startIndex; i < endIndex; i++)
    {
        if ("" ~ sourceLine[cast(uint)i] == STR_OPENING_PARENTHESIS)
            parenthesisStack ~= i;
        else if ("" ~ sourceLine[cast(uint)i] == STR_CLOSING_PARENTHESIS)
        {
            if (!parenthesisStack.empty)
                parenthesisStack.popBack();
            else
            {
                if (findClosest)
                {
                    parenthesisStack = [i];
                    break;
                }
                else if (parType == ParenhesisType.Closing)
                    parenthesisStack ~= i;
            }
        }
    }
    if (parenthesisStack.length == 1)
    {
         parIndex = parenthesisStack[0];
         return true;
    }

    if (parType == ParenhesisType.Opening)
        trace("no open parenthesis found, stack.length = " ~ std.conv.to!string(parenthesisStack.length));
    else
        trace("no close parenthesis found, stack.length = " ~ std.conv.to!string(parenthesisStack.length));
    return false;
}

private QuotesInfo detectFunctionArgument(in string functionName, in string sourceLine, in long index)
{
    long indexOpen;
    if (!isInsideParenthesis(sourceLine, index, ParenhesisType.Opening, indexOpen))
        return QuotesInfo(-1, -1, false);

    long indexClose;
    if (!isInsideParenthesis(sourceLine, index, ParenhesisType.Closing, indexClose))
        return QuotesInfo(-1, -1, false);

    auto thisFunctionName = sourceLine[cast(uint)(cast(uint)indexOpen - functionName.length) .. cast(uint)indexOpen];
    if (thisFunctionName != functionName)
    {
        trace("non-ambiguous function call detected = " ~ thisFunctionName);
        return QuotesInfo(-1, -1, false);
    }

    return QuotesInfo(indexOpen, indexClose, true);
}

// TODO: rewrite code like in detectFunctionArgument
private QuotesInfo isInsideQuotes(in string strLine, in long index)
{    
    long[] stack;
    
    // Search for opening separator: skip paired characters until we find unpaired one
    for (auto i = 0; i < index; i++)
    {
        if ("" ~ strLine[i] == STR_DOUBLE_QUOTE)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
    {
        trace("no open char '" ~ STR_DOUBLE_QUOTE ~ "' before index " ~ std.conv.to!string(index)
                ~ ", stack.length = " ~ std.conv.to!string(stack.length));
        return QuotesInfo(-1, -1, false);
    }
    auto indexOpen = stack[0];
    stack = [];

    // Search for closing double quote (unpaired)
    for (auto i = index + 1; i < strLine.length; i++)
    {
        if ("" ~ strLine[cast(uint)i] == STR_DOUBLE_QUOTE)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
    {
        trace("no close char '" ~ STR_DOUBLE_QUOTE ~ "' before index " ~ std.conv.to!string(index)
                ~ ", stack.length = " ~ std.conv.to!string(stack.length));
        return QuotesInfo(-1, -1, false);
    }
    auto indexClose = stack[0];
    stack = [];

    return QuotesInfo(indexOpen, indexClose, true);
}

private string cutInlineComment(in string sourceLine)
{
    bool commentFound;
    return cutInlineComment(sourceLine, commentFound);
}

private string cutInlineComment(in string sourceLine, ref bool commentFound)
{
    string result = sourceLine;

    auto hashIndex = sourceLine.indexOf(STR_HASH);
    if (hashIndex >= 0)
    {
        if (hashIndex == 0)
        {
            commentFound = true;
            trace("skip comment line");
        }
        else
            trace("cutting off inline comment");

        result = sourceLine[0 .. hashIndex];
        result = result.strip();
    }

    return result;
}

private alias ExtractResult = Tuple!(string[], "arguments", long, "endIndex");

// NOTE: regex may contain paired parenthesis which don't handled by grammar now
private ExtractResult extractFunctionArguments(
    in string functionName, in long functionArgumentCount,
    in long startIndex, in string sourceLine)
in
{
    assert(!functionName.empty);
    assert(functionArgumentCount >= 1);
    assert((startIndex >= 0) && startIndex < (sourceLine.length));
    assert(sourceLine.length > functionName.length);
}
out(result)
{
    assert(result.arguments.length == functionArgumentCount);
    // NOTE: some of arguments can be empty, need further investigion
    //for (int i; i < result.arguments.length; i++) assert(!result.arguments[i].empty);
}
do
{
    trace("Extracting function '", functionName, "' arguments, expect ", functionArgumentCount, " items");

    ExtractResult result;
    int argumentIndex;
    long[] parenthesisStack;

    // Search for function call statement: function name + opening parenthehis
    // FIXME: allow whitespace* between function name and parenthesis
    immutable auto functionCallIndex = sourceLine.indexOf(functionName ~ STR_OPENING_PARENTHESIS, startIndex);
    if (functionCallIndex < 0)
    {
        trace("no function '", functionName, "' call found");
        return result;
    }
        
    // Save opening parenthesis in the stack
    immutable auto functionOpenParIndex = functionCallIndex + functionName.length;
    assert(sourceLine[functionOpenParIndex] == CHAR_OPENING_PARENTHESIS);
    // NOTE: we do not save function opening parenthesis intentionally:
    //       to recognize function argument list end correctly

    // Extract function arguments
    long i = functionOpenParIndex + 1;
    while (argumentIndex < functionArgumentCount)
    {
        // If it is not last arguments, just consume all chars until comma found
        immutable bool lastArgument = argumentIndex == functionArgumentCount - 1;
        if (!lastArgument)
        {
            long commaIndex = -1;
            long j = i;
            while (j < sourceLine.length)
            {
                if (sourceLine[j] == CHAR_COMMA)
                {
                    commaIndex = j;
                    break;
                }
                
                j++;
            }
            assert(j > i);
            assert(sourceLine[j] == CHAR_COMMA);
            assert(sourceLine[commaIndex] == CHAR_COMMA);

            string argument = sourceLine[i .. j];
            assert(!argument.empty);
            trace("Middle function argument ", argumentIndex, ": ", argument);

            // Skip possible whitespaces after comma
            j ++;
            while (j < sourceLine.length)
            {
                if (sourceLine[j] != CHAR_WS)
                    break;
                j++;
            }
            
            result.arguments ~= argument;
            i = j;
            argumentIndex++;
        }
        else
        {
            // Skip paired parenthesis until got unpaired close one
            long j = i;
            while (j < sourceLine.length)
            {
                if (sourceLine[j] == CHAR_OPENING_PARENTHESIS)
                    parenthesisStack ~= j;
                else if (sourceLine[j] == CHAR_CLOSING_PARENTHESIS)
                {
                    if (!parenthesisStack.empty)
                        parenthesisStack.popBack();
                    else
                        break;
                }

                j++;
            }
            // NOTE: argument can be empty, e.g. `VCLIBS = Microsoft.VCLibs.$$replace(MSVC_VER, \\., ).00`
            assert(j >= i);
            assert(sourceLine[j] == CHAR_CLOSING_PARENTHESIS);
            assert(parenthesisStack.length == 0);

            string argument = sourceLine[i .. j];
            //assert(!argument.empty);
            trace("Last function argument ", argumentIndex, ": ", argument);

            result.arguments ~= argument;
            i = j + 1;
            argumentIndex++;
        }
    }

    result.endIndex = i;
    return result;
}

private bool isMultiline(in string sourceLine)
{
    return sourceLine.endsWith(STR_BACKSLASH);
}

private struct MultilineInfo
{
    long startIndex = -1;
    long endIndex = -1;
    string line;
}

private bool mergeMultiline(in long lineNo, in string[] lines, out MultilineInfo result)
in
{
    //assert(isMultiline(lines[lineNo]));
}
out(r)
{
    if (r)
        assert(!result.line.empty);
}
do
{
    string currentLine = lines[lineNo];
    long currentLineIndentSize = detectIndentSize(currentLine);

    currentLine = currentLine.strip();
    currentLine = cutInlineComment(currentLine);
    trace("Current line: |", currentLine, "|, indent: ", currentLineIndentSize);

    if (!isMultiline(currentLine))
    {
        trace("not a multi-line at all, skipping");
        return false;
    }

    assert(isMultiline(currentLine));
    result.line = currentLine[0 .. $ - 1];
    assert(!isMultiline(result.line));

    auto j = lineNo + 1;
    if (j == lines.length)
    {
        trace("the last line, nothing to merge, just remove trailing backslash");

        result.startIndex = lineNo;
        result.endIndex = lineNo;
        return true;
    }

    string nextLine = lines[j];
    long nextLineIndentSize = detectIndentSize(nextLine);
    nextLine = nextLine.strip();
    nextLine = cutInlineComment(nextLine);
    trace(" Next line 1: |", nextLine,    "|, indent: ", nextLineIndentSize);

    if ((j + 1 == lines.length) || (!nextLine.empty && !isMultiline(nextLine)))
    {
        result.line ~= STR_WS ~ nextLine;
        trace("just two multi-lines");
        trace(result.line);

        result.startIndex = lineNo;
        result.endIndex = j;
        return true;
    }

    // Python-like syntax detection: backslashes may be skipped
    // if multi-line aligned by similar indent
    long lastMultilineBlockLine = -1;
    bool indentSizeChanged = false;
    long k;
    for (k = j + 1; k < lines.length; k++)
    {
        nextLine = lines[k];
        long newNextLineIndentSize = detectIndentSize(nextLine);

        if (nextLineIndentSize != newNextLineIndentSize)
        {
            trace("Indentation size changed line index: ", (k + 1));
            indentSizeChanged = true;
            break;
        }

        nextLine = nextLine.strip();
        trace(" Next line 2: |", nextLine,    "|, indent: ", newNextLineIndentSize);
    }
    if (indentSizeChanged)
    {
        lastMultilineBlockLine = k;
        trace(" Next line 3: |", nextLine, "|");
        trace("Last multi-line heuristic index: ", (lastMultilineBlockLine + 1));
    }

    long lastMultiline = -1;
    for (k = j + 1; k < lines.length; k++)
    {
        nextLine = lines[k];

        bool commentFound;
        nextLine = cutInlineComment(nextLine, commentFound);
        nextLine = nextLine.strip();
        // Just ignore empty/comment lines
        if (commentFound || nextLine.empty)
        {
            trace("Skip empty/comment line...");
            continue;
        }

        if (!isMultiline(nextLine))
        {
            lastMultiline = k;
            break;
        }
    }
    if (lastMultiline != -1)
    {
        // NOTE: k-th line is also multi-line part, stop-line is the next one
        lastMultiline ++;
        trace(" Next line 4: |", lines[lastMultiline], "|");
        trace("Last multi-line strict index: ", lastMultiline + 1);
    }

    //assert(lastMultilineBlockLine == lastMultiline);
    if (lastMultilineBlockLine > lastMultiline)
    {
        writeln("\nPARSER WARNING:\n");
        writeln("absent backslash detected\n");
        //lastMultiline = lastMultilineBlockLine;
    }

    // Merge multi-line
    for (k = lineNo + 1; k < lastMultiline; k++)
    {
        currentLine = lines[k];
        currentLine = currentLine.strip();

        bool commentFound;
        currentLine = cutInlineComment(currentLine, commentFound);

        if (!currentLine.empty)
            result.line ~= isMultiline(currentLine) ? STR_WS ~ strip(currentLine[0 .. $ - 1]) : STR_WS ~ currentLine;
    }

    trace("startIndex: " ~ std.conv.to!string(lineNo));
    trace("endIndex: " ~ std.conv.to!string(lastMultiline));
    trace("result: |" ~ result.line ~ "|");

    result.startIndex = lineNo;
    result.endIndex = lastMultiline - 1;
    return true;
}

private bool hasSinglelineScope(in string sourceLine, out long colonIndex)
{
    colonIndex = -1;

    if (sourceLine.empty || sourceLine.endsWith(STR_OPENING_CURLY_BRACE))
        return false;

    // testFunc(): x = y ... : ...
    auto lastEqIndex = sourceLine.lastIndexOf(STR_EQUALS);

    long i;
    if (lastEqIndex == -1)
    {
        i = cast(long)sourceLine.length - 1;
    }
    else if (isInsideQuotes(sourceLine, lastEqIndex).success)
    {
        trace("assignment operator detected inside quotes on position ", lastEqIndex);
        while (isInsideQuotes(sourceLine, lastEqIndex).success && (lastEqIndex != -1))
        {
            lastEqIndex = sourceLine.lastIndexOf(STR_EQUALS, lastEqIndex);
        }
        trace("new assignment operator index on position ", lastEqIndex);
        i = lastEqIndex - 1;
    }
    else
    {
        i = lastEqIndex - 1;
    }

    for ( ; i >= 0; i--)
    {
        if ("" ~ sourceLine[cast(uint)i] != STR_COLON)
            continue;

        trace("colon detected at index " ~ std.conv.to!string(i));

        auto info1 = isInsideQuotes(sourceLine, i);
        if (info1.success)
        {
            i = info1.indexOpen - 1;
            trace("quote begin = " ~ std.conv.to!string(info1.indexOpen));
            trace("quote end = " ~ std.conv.to!string(info1.indexClose));
            trace("colon inside quotes detected, go back to index " ~ std.conv.to!string(i));
            continue;
        }

        auto info2 = detectFunctionArgument("requires", sourceLine, i);
        if (info2.success)
        {
            i = info2.indexOpen - 1;
            trace("function begin = " ~ std.conv.to!string(info2.indexOpen));
            trace("function end = " ~ std.conv.to!string(info2.indexClose));
            continue;
        }

        colonIndex = i;
        trace("single-line block statement detected");
        return true;
    }
    return false;
}

private bool hasSinglelineScope(in string sourceLine)
{
    long colonIndex;
    return hasSinglelineScope(sourceLine, colonIndex);
}

private bool fixSinglelineScope(in string sourceLine, out string resultLine)
{
    resultLine = sourceLine;

    long colonIndex;
    if (!hasSinglelineScope(sourceLine, colonIndex))
        return false;

    trace("single-line scope statement detected and fixed");
    resultLine.replaceInPlace(cast(uint)colonIndex, cast(uint)(colonIndex + 1), STR_DOG);
    return true;
}

private bool hasMultilineScope(in string sourceLine, out long colonIndex)
{
    if (!sourceLine.endsWith(STR_OPENING_CURLY_BRACE))
        return false;

    // Search for reduntant colon
    // E.g.: contains(TEMPLATE, ".*app"):!build_pass: { ... }
    colonIndex = sourceLine.lastIndexOf(STR_COLON);
    if (colonIndex == -1)
        return false;

    bool reduntant = true;
    for (int i = cast(int)colonIndex + 1; i < cast(int)sourceLine.length - 1; i++)
    {
        if (!isWhite(sourceLine[i]))
        {
            reduntant = false;
            break;
        }
    }

    return reduntant;
}

private bool hasMultilineScope(in string sourceLine)
{
    long colonIndex;
    return hasMultilineScope(sourceLine, colonIndex);
}

private bool fixMultilineScope(in string sourceLine, out string resultLine)
{
    resultLine = sourceLine;

    long colonIndex;
    if (!hasMultilineScope(sourceLine, colonIndex))
        return false;

    trace("single-line scope statement detected and fixed");
    resultLine.replaceInPlace(cast(uint)colonIndex, cast(uint)(colonIndex + 1), "" ~ STR_DOG);
    return true;
}

private bool hasSinglelineScopeElse(in string sourceLine)
{
    return sourceLine.indexOf(STR_ELSE_SINGLELINE) != -1;
}

private bool fixSinglelineScopeElse(in string sourceLine, out string resultLine)
{
    resultLine = sourceLine.replace(STR_ELSE_SINGLELINE, STR_ELSE ~ STR_DOG);
    return true;
}

private void prettifyLine(ref LineInfo li)
{
    if (li.line.endsWith(STR_SEMICOLON))
    {
        li.mods |= PreprocessorModifications.TrailingSemicolonRemoved;
        li.line = li.line[0 .. $-1];
        assert(!li.line.endsWith(STR_SEMICOLON));
    }

    auto temp = li.line.strip();
    if (li.line != temp)
    {
        li.mods |= PreprocessorModifications.WhitespaceStripped;
        li.line = temp;
    }

    temp = cutInlineComment(li.line);
    if (li.line != temp)
    {
        li.mods |= PreprocessorModifications.CommentRemoved;
        li.line = temp;
    }
}

private void fixMultiline(ref LineInfo li, ref MultilineInfo mresult, ref long lineIndex, in string[] strLinesArray)
{
    if (isMultiline(li.line))
    {
        if (mergeMultiline(lineIndex, strLinesArray, mresult))
        {
            li.mods |= PreprocessorModifications.MultilineMerged;
            li.line = mresult.line;

            lineIndex = mresult.endIndex;
        }
    }
}

private void fixScope(ref LineInfo li)
{
    // FIXME: workaround for grammar ambiguity - cannot distingush AND-colon and scope statement expression end colon
    // Replace last colon (":") with "@";
    // Exceptions:
    // - there is an "{" on this line
    // - the colon is inside quotes/doublequotes
    // - the colon is a part of assignment operator
    if (hasSinglelineScope(li.line))
    {
        li.mods |= PreprocessorModifications.SinglelineScopeFixed;

        fixSinglelineScope(li.line, li.line);
    }
    if (hasMultilineScope(li.line))
    {
        li.mods |= PreprocessorModifications.MultilineScopeFixed;

        fixMultilineScope(li.line, li.line);
    }

    // Replace "else:" with "else@"
    if (hasSinglelineScopeElse(li.line))
    {
        li.mods |= PreprocessorModifications.SinglelineScopeElseFixed;

        fixSinglelineScopeElse(li.line, li.line);
    }
}

private bool isEnquotedString(in string str)
{
    if (str.length < 2)
        return false;
    
    return (str.front == CHAR_DOUBLE_QUOTE) && (str.back == CHAR_DOUBLE_QUOTE);
}

private string enquoteString(in string str)
{
    assert(!isEnquotedString(str));

    return CHAR_DOUBLE_QUOTE ~ str ~ CHAR_DOUBLE_QUOTE;
}

/**
 * Enquotes (add quotes around) specified functionName arguments
 * with indeces targetArgumentIndeces
 * Params:
 *      functionName          = the name of function to search for
 *      argumentCount         = expected function argument count
 *      targetArgumentIndeces = one-based argument indeces to enquote
 *      li                    = reference to current line information structure to modify
 */
private void enquoteAmbiguousFunctionArguments(in string functionName, in long argumentCount,
    in long[] targetArgumentIndeces, ref LineInfo li)
{
    long functionCallIndex;
    while (functionCallIndex < li.line.length)
    {
        functionCallIndex = li.line.indexOf(functionName ~ STR_OPENING_PARENTHESIS, functionCallIndex);
        if (functionCallIndex == -1)
            break;

        auto replaceArguments = extractFunctionArguments(functionName, argumentCount, functionCallIndex, li.line);
        assert(!replaceArguments.arguments.empty);
        assert(replaceArguments.endIndex > functionCallIndex);

        foreach (i; targetArgumentIndeces)
        {
            string arg = strip(replaceArguments.arguments[i - 1]);

            // NOTE: function argument indeces are one-based, not zero-ones
            if (!isEnquotedString(arg))
            {
                li.mods |= PreprocessorModifications.FunctionArgumentEnquoted;
                replaceArguments.arguments[i - 1] = enquoteString(arg);
            }
        }

        string lineBefore = li.line[0 .. functionCallIndex];
        string lineAfter = li.line[replaceArguments.endIndex .. $];
        string finalLine =
              lineBefore
            ~ functionName
            ~ STR_OPENING_PARENTHESIS
            ~ replaceArguments.arguments.join(STR_COMMA ~ STR_WS)
            ~ STR_CLOSING_PARENTHESIS
            ~ lineAfter;
        trace("lineBefore: ", lineBefore);
        trace("lineAfter: ", lineAfter);
        trace("finalLine: ", finalLine);

        li.line = finalLine;
        functionCallIndex++;
    }

    // FIXME: add error handling above, e.g. exception generation
}

private void fixAmbiguousFunctionCalls(ref LineInfo li)
{
    // E.g.:
    // replace(string, old_string, new_string)
    // qtConfig(opengl(es1|es2)?)
    // contains(var, regex)
    // system(/OUT:$$MYSCRIPT_VOIM_LIB_PATH)

    // Test functions

    // Enquote `exists` test function second argument
    enquoteAmbiguousFunctionArguments(EXISTS_FUNCTION_STR, 1, [1], li);

    // Enquote `contains` test function second argument
    enquoteAmbiguousFunctionArguments(CONTAINS_FUNCTION_STR, 2, [2], li);

    // Enquote `qtConfig` test function first argument
    enquoteAmbiguousFunctionArguments(QTCONFIG_FUNCTION_STR, 1, [1], li);

    // Enquote `error` test function first argument
    enquoteAmbiguousFunctionArguments(ERROR_FUNCTION_STR, 1, [1], li);

    // Replace functions

    // Enquote `find` replace function second and third arguments
    enquoteAmbiguousFunctionArguments(FIND_FUNCTION_STR, 2, [2], li);

    // Enquote `re_escape` replace function second and third arguments
    enquoteAmbiguousFunctionArguments(REESCAPE_FUNCTION_STR, 1, [1], li);

    // Enquote `replace` replace function second and third arguments
    enquoteAmbiguousFunctionArguments(REPLACE_FUNCTION_STR, 3, [2, 3], li);

    // Enquote `system` replace/test function first argument
    enquoteAmbiguousFunctionArguments(SYSTEM_FUNCTION_STR, 1, [1], li);
}

//---------------------------------------------------------------------------------------------------------------------

struct LineInfo
{
    alias Mods = PreprocessorModifications;

    string line;
    long index;
    Mods mods;
}

string preprocessLines(in string[] strLinesArray, out LineInfo[] resultLines)
{
    string[] result;
    MultilineInfo mresult;
    for (long lineIndex; lineIndex < strLinesArray.length; lineIndex++)
    {
        LineInfo li;
        li.index = lineIndex;
        li.line = strLinesArray[cast(uint)lineIndex];

        prettifyLine(li);
        fixMultiline(li, mresult, lineIndex, strLinesArray);
        fixAmbiguousFunctionCalls(li);
        fixScope(li);

        if (li.mods & PreprocessorModifications.MultilineMerged)
            trace("Multi-line " ~ std.conv.to!string(mresult.startIndex + 1) ~
                " - " ~ std.conv.to!string(mresult.endIndex + 1) ~ ": |" ~ li.line ~ "|");
        else
            trace("Line " ~ std.conv.to!string(lineIndex + 1) ~ ": |" ~ li.line ~ "|");

        result ~= li.line;
        resultLines ~= li;
    }

    return result.join("\n");
}

unittest
{
    // FIXME: implement
    writeln("<<< 1 >>>");
    // detectFunctionArgument
    // detectPairedCharacter
    // cutInlineComment
    // preprocessLines
    writeln("<<< >>>");
}
