/****************************************************************************
**
** Copyright (C) 2018 Alexander Kamyshnikov
** Contact: axill777@gmail.com
**
** This file is part of the qmake-ng application, replacement of the Qt Toolkit one.
**
** Foobar is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** Foobar is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with qmake-ng.  If not, see <http://www.gnu.org/licenses/>.
**
****************************************************************************/

module preprocessor;

import std.typecons : BitFlags;

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

private const auto STR_HASH = '#';
private const auto STR_QUOTE = '\'';
private const auto STR_DQUOTE = '"';
private const auto STR_OPEN_CURLY_BRACE = '{';
private const auto STR_OPENING_PARENTHESIS = '(';
private const auto STR_CLOSING_PARENTHESIS = ')';
private const auto STR_COLON = ':';
private const auto STR_COMMA = ',';
private const auto STR_DOG = '@';
private const auto STR_BACKSLASH = '\\';
private const auto STR_WS = ' ';
private const auto STR_EQ = '=';

private const auto STR_ELSE = "else";
private const auto STR_ELSE_SINGLELINE = "else:";

private const auto CONTAINS_STR = "contains(";
private const auto QTCONFIG_STR = "qtConfig(";

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
    CommentRemoved           = 1 << 1,
    MultilineMerged          = 1 << 2,
    SinglelineScopeFixed     = 1 << 3,
    MultilineScopeFixed      = 1 << 4,
    SinglelineScopeElseFixed = 1 << 5,
    FunctionArgumentEnquoted = 1 << 6
} 

private enum ParenhesisType
{
    None,
    Opening,
    Closing
}

private bool isInsideParenthesis(in string sourceLine, in long index, in ParenhesisType parType, out long parIndex)
{
    immutable long startIndex = (parType == ParenhesisType.Opening) ? 0     : (index + 1);
    immutable long endIndex   = (parType == ParenhesisType.Opening) ? index : sourceLine.length;

    long[] parenthesisStack;
    for (long i = startIndex; i < endIndex; i++)
    {
        if (sourceLine[i] == STR_OPENING_PARENTHESIS)
            parenthesisStack ~= i;
        else if (sourceLine[i] == STR_CLOSING_PARENTHESIS)
        {
            if (!parenthesisStack.empty)
                parenthesisStack.popBack();

            if (parType == ParenhesisType.Closing)
                parenthesisStack ~= i;
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

    auto thisFunctionName = sourceLine[indexOpen - functionName.length .. indexOpen];
    if (thisFunctionName != functionName)
    {
        trace("non-ambiguous function call detected = " ~ thisFunctionName);
        return QuotesInfo(-1, -1, false);
    }

    return QuotesInfo(indexOpen, indexClose, true);
}

private QuotesInfo detectPairedCharacter(in char openChar, in char closeChar, in string strLine, in long index)
{    
    long[] stack;
    
    // Search for opening separator: skip paired characters until we find unpaired one
    for (auto i = index - 1; i >= 0; i--)
    {
        if (strLine[i] == openChar)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
    {
        trace("no open char '" ~ openChar ~ "' before index " ~ std.conv.to!string(index)
                ~ ", stack.length = " ~ std.conv.to!string(stack.length));
        return QuotesInfo(-1, -1, false);
    }
    auto indexOpen = stack[0];
    stack = [];

    // Search for closing double quote (unpaired)
    for (auto i = index + 1; i < strLine.length; i++)
    {
        if (strLine[i] == closeChar)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
    {
        trace("no close char '" ~ closeChar ~ "' before index " ~ std.conv.to!string(index)
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

    auto hashIndex = indexOf(sourceLine, STR_HASH);
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
        result = strip(result);
    }

    return result;
}

private bool hasFunctionCall(in string functionName, in string sourceLine)
{
    return sourceLine.indexOf(functionName) != -1;
}

// qtConfig(opengl(es1|es2)?)
// contains(var, regex)
private string enquoteFunctionArgument(in string functionName, in int argumentIndex, in string strLine)
{
    string result;

    // contains(id, regex)
    // NOTE: regex may contain paired parenthesis
    long functionIndex, newFunctionEndIndex = -1;
    while (true)
    {
        // FIXME: implement regex search using "contains\\s*\\("
        functionIndex = strLine.indexOf(functionName, functionIndex);
        if (functionIndex == -1)
        {
            result ~= strLine[newFunctionEndIndex == -1 ? 0 : newFunctionEndIndex .. $];
            break;
        }

        trace("ambiguous function detected at index " ~ std.conv.to!string(functionIndex));

        functionIndex += functionName.length;

        // Skip previous arguments
        auto commaIndex = functionIndex;
        auto currentArgumentIndex = 1;
        while (currentArgumentIndex < argumentIndex)
        {
            commaIndex = strLine.indexOf(STR_COMMA, commaIndex);
            if (commaIndex == -1)
            {
                trace("ambiguous function '" ~ functionName ~ "' argument count is "
                    ~ std.conv.to!string(currentArgumentIndex));
                break;
            }

            currentArgumentIndex++;
        }
        assert(commaIndex >= 0);

        trace("strLine[commaIndex] = '" ~ strLine[commaIndex] ~ "'");
        if (argumentIndex >= 2)
            commaIndex++;

        // Skip whitespaces between comma and next argument value
        // FIXME: move this code to separate function
        auto secondArgumentBeginIndex = commaIndex;
        while (isWhite(strLine[secondArgumentBeginIndex]) && (secondArgumentBeginIndex < strLine.length))
            secondArgumentBeginIndex++;

        // Search for second argument end - skip paired parenthesis
        auto secondArgumentEndIndex = secondArgumentBeginIndex;
        trace("strLine[secondArgumentEndIndex] = '" ~ strLine[secondArgumentEndIndex] ~ "'");
        long[] parenthesisStack;
        for (auto i = secondArgumentBeginIndex; i < strLine.length; i++)
        {
            if (strLine[i] == STR_OPENING_PARENTHESIS)
                parenthesisStack ~= i;
            else if (strLine[i] == STR_CLOSING_PARENTHESIS)
            {
                if (parenthesisStack.empty)
                {
                    secondArgumentEndIndex = i;
                    break;
                }
                
                parenthesisStack.popBack();
            }
        }

        string wsSuffix;
        if (argumentIndex >= 2)
            wsSuffix ~= STR_WS;

        // Enquote specified argument value
        auto secondArgument = strLine[secondArgumentBeginIndex .. secondArgumentEndIndex];
        auto secondArgumentQuoted = secondArgument;
        if ((secondArgument[0] != STR_DQUOTE) && (secondArgument[$ - 1] != STR_DQUOTE))
            secondArgumentQuoted = STR_DQUOTE ~ secondArgument ~ STR_DQUOTE;
        result ~= strLine[newFunctionEndIndex == -1 ? 0 : newFunctionEndIndex .. commaIndex] ~ wsSuffix;
        result ~= secondArgumentQuoted ~ STR_CLOSING_PARENTHESIS;

        trace("secondArgument = '" ~ secondArgument ~ "'");
        trace("secondArgumentQuoted = '" ~ secondArgumentQuoted ~ "'");

        newFunctionEndIndex = secondArgumentEndIndex + 1;
    }
    
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
// FIXME: in-out-do contracts
{
    string currentLine = lines[lineNo];
    currentLine = currentLine.strip();
    currentLine = cutInlineComment(currentLine);

    if (!isMultiline(currentLine))
    {
        trace("not a multi-line, skipping");
        return false;
    }

    result.line = strip(currentLine[0 .. $ - 1]);
    auto j = lineNo + 1;
    if (j == lines.length)
    {
        trace("the last line, nothing to merge");
        return false;
    }
    
    for (; j < lines.length; j++)
    {
        currentLine = lines[j];
        currentLine = currentLine.strip();

        bool commentFound;
        currentLine = cutInlineComment(currentLine, commentFound);
        
        result.line ~= isMultiline(currentLine) ? STR_WS ~ strip(currentLine[0 .. $ - 1]) : STR_WS ~ currentLine;

        // NOTE: comment line will be replaced with empty one by cutInlineComment() function call
        if (!commentFound)
        {
            if (!isMultiline(currentLine) || currentLine.empty)
                break;
        }
    }

    trace("startIndex: " ~ std.conv.to!string(lineNo));
    trace("endIndex: " ~ std.conv.to!string(j));

    result.startIndex = lineNo;
    result.endIndex = j;
    return true;
}

private bool hasSinglelineScope(in string sourceLine, out long colonIndex)
{
    colonIndex = -1;

    if (sourceLine.empty || sourceLine.endsWith(STR_OPEN_CURLY_BRACE))
        return false;

    // testFunc(): x = y ... : ...
    immutable auto lastEqIndex = sourceLine.lastIndexOf(STR_EQ);
    long i = (lastEqIndex == -1) ? (cast(long)sourceLine.length - 1) : (lastEqIndex - 1);
    for ( ; i >= 0; i--)
    {
        if (sourceLine[i] != STR_COLON)
            continue;

        trace("colon detected at index " ~ std.conv.to!string(i));

        auto info1 = detectPairedCharacter(STR_DQUOTE, STR_DQUOTE, sourceLine, i);
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
    resultLine.replaceInPlace(colonIndex, colonIndex + 1, "" ~ STR_DOG);
    return true;
}

private bool hasMultilineScope(in string sourceLine, out long colonIndex)
{
    if (!sourceLine.endsWith(STR_OPEN_CURLY_BRACE))
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
    resultLine.replaceInPlace(colonIndex, colonIndex + 1, "" ~ STR_DOG);
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
        mergeMultiline(lineIndex, strLinesArray, mresult);

        li.mods |= PreprocessorModifications.MultilineMerged;
        li.line = mresult.line;

        lineIndex = mresult.endIndex;
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

private void fixAmbiguousFunctionCalls(ref LineInfo li)
{
    // Enquote contains test function second argument
    if (hasFunctionCall(CONTAINS_STR, li.line))
    {
        li.mods |= PreprocessorModifications.FunctionArgumentEnquoted;
        li.line = enquoteFunctionArgument(CONTAINS_STR, 2, li.line);
    }

    // Enquote qtConfig test function first argument
    if (hasFunctionCall(QTCONFIG_STR, li.line))
    {
        li.mods |= PreprocessorModifications.FunctionArgumentEnquoted;
        li.line = enquoteFunctionArgument(QTCONFIG_STR, 1, li.line);
    }
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
        li.line = strLinesArray[lineIndex];

        prettifyLine(li);
        fixMultiline(li, mresult, lineIndex, strLinesArray);
        fixScope(li);
        fixAmbiguousFunctionCalls(li);

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
    assert(isWhite(' '));
    assert(isWhite('\t'));
    // detectFunctionArgument
    // detectPairedCharacter
    // cutInlineComment
    // enquoteFunctionArgument
    // preprocessLines
    writeln("<<< >>>");
}
