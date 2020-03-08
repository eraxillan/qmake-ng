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

module source.preprocessor;

import std.experimental.logger;

static import std.regex;

import std.typecons;
import std.uni;
import std.algorithm;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;
import std.range;

import source.common_const;
import source.common_utils;

//---------------------------------------------------------------------------------------------------------------------
private:

const auto STR_ELSE = "else";
const auto STR_ELSE_SINGLELINE = "else:";

package:
// ambiguous test functions
const auto EXISTS_FUNCTION_STR = "exists";      // argument 1
const auto CONTAINS_FUNCTION_STR = "contains";  // argument 2
const auto QTCONFIG_FUNCTION_STR = "qtConfig";  // argument 1
const auto ERROR_FUNCTION_STR = "error";        // argument 1
const auto REQUIRES_FUNCTION_STR = "requires";  // argument 1

// ambiguous test/replace functions
const auto SYSTEM_FUNCTION_STR = "system";      // argument 1

// ambiguous replace functions
const auto FIND_FUNCTION_STR = "find";          // argument 2
const auto REESCAPE_FUNCTION_STR = "re_escape"; // argument 1
const auto REPLACE_FUNCTION_STR = "replace";    // arguments 2 and 3

//---------------------------------------------------------------------------------------------------------------------
private:

enum PreprocessorModifications
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

enum ParenhesisType
{
    None,
    Opening,
    Closing
}

struct MultilineInfo
{
    long startIndex = INVALID_INDEX;
    long endIndex = INVALID_INDEX;
    string line;
}

//---------------------------------------------------------------------------------------------------------------------

public struct LineInfo
{
    alias Mods = PreprocessorModifications;

    string line;
    long index;
    Mods mods;
}

//---------------------------------------------------------------------------------------------------------------------

TextSearchResult isInsideQuotes(const string strLine, const long index)
in
{
    assert(index >= 0 && index < strLine.length);
}
out (result)
{
    assert((result.success && (result.indexOpen >= 0) && (result.indexClose < strLine.length)
        && (strLine[result.indexOpen] == CHAR_DOUBLE_QUOTE) && (strLine[result.indexClose] == CHAR_DOUBLE_QUOTE))
        || (!result.success) && (result.indexOpen == INVALID_INDEX) && (result.indexClose == INVALID_INDEX));
}
do
{
    // Naive optimization: enquoted char length must have length 3 or more
    if (strLine.length < 3)
    {
        trace("input string is too short to contain quotes");
        return TextSearchResult();
    }

    long[] stack;
    
    // Search for opening separator: skip paired characters until we find unpaired one
    for (auto i = 0; i < index; i++)
    {
        if (strLine[i] == CHAR_DOUBLE_QUOTE)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
    {
        trace("no opening double quote before index ", index,
              ", stack.length = ", stack.length);
        return TextSearchResult();
    }

    auto indexOpen = stack[0];
    stack = [];

    // Search for closing double quote (unpaired)
    for (auto i = index + 1; i < strLine.length; i++)
    {
        if (strLine[i] == CHAR_DOUBLE_QUOTE)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
    {
        trace("no closing double quote before index ", index,
              ", stack.length = ", stack.length);
        return TextSearchResult();
    }
    auto indexClose = stack[0];
    stack = [];

    return TextSearchResult(indexOpen, indexClose, true);
}

string cutInlineComment(const string sourceLine)
{
    bool commentFound;
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

    // TODO: can be commentFound var useful outside?
    return result;
}

long detectFunctionArgumentsStartIndex(const string functionName, const string sourceStr, const long fromIndex)
in
{
    assert(!functionName.empty);
    assert(fromIndex >= 0 && fromIndex < sourceStr.length - 1);
}
out (result)
{
    assert(result >= 0 && result < sourceStr.length);
    assert(sourceStr[result] == CHAR_OPENING_PARENTHESIS);
}
do
{
    // Search for function call statement: function name + optional whitespaces + opening parenthehis
    immutable auto functionCallPos = findFunctionCall(sourceStr, functionName, fromIndex);
    assert(functionCallPos.isValid() && functionCallPos.success);

    immutable auto functionOpenParIndex = functionCallPos.indexClose;
    trace("Arguments start index = ", functionOpenParIndex);

    return functionOpenParIndex;
}

long detectFunctionArgumentsEndIndex(const string sourceStr, const long functionCallStartIndex)
in
{
    assert(functionCallStartIndex < sourceStr.length);

    // Does given string contains function call at all?
    long indexBegin = sourceStr.indexOf(STR_OPENING_PARENTHESIS);
    assert(indexBegin > 0);
    long indexEnd = sourceStr.indexOf(STR_CLOSING_PARENTHESIS, indexBegin + 1);
    assert(indexEnd > 0);
    assert(indexEnd > indexBegin);

    assert(sourceStr[functionCallStartIndex] == CHAR_OPENING_PARENTHESIS);
}
out (result)
{
    assert(result > functionCallStartIndex && result < sourceStr.length);
    assert(sourceStr[result] == CHAR_CLOSING_PARENTHESIS);
}
do
{
    long result;
    long[] parenthesisStack;

    bool isPrependedWithBackslash(const long index)
    {
        assert(index < sourceStr.length);
        if (index < 1)
            return false;
        
        return (sourceStr[index - 1] == CHAR_BACKSLASH);
    }

    // Determine where function ends, i.e. final closing parenthesis, using stack collection:
    // * skip enquoted function arguments (ignoring espaced quotes)
    // * skip paired parenthesis until got unpaired close one
    result = functionCallStartIndex;
    while (result < sourceStr.length)
    {
        if ((sourceStr[result] == CHAR_DOUBLE_QUOTE) && !isPrependedWithBackslash(result))
        {
            trace("open quote detected at index ", result);

            // Skip enquoted argument
            result++;
            while (result < sourceStr.length)
            {
                if ((sourceStr[result] == CHAR_DOUBLE_QUOTE) && !isPrependedWithBackslash(result))
                    break;

                result++;
            }
            
            assert(result < sourceStr.length);
            assert(sourceStr[result] == CHAR_DOUBLE_QUOTE);
            trace("close quote detected at index ", result);
        }
        else if (sourceStr[result] == CHAR_OPENING_PARENTHESIS)
        {
            trace("Open par at ", result);
            parenthesisStack ~= result;
        }
        else if (sourceStr[result] == CHAR_CLOSING_PARENTHESIS)
        {
            if (!parenthesisStack.empty)
            {
                trace("Close par at ", result, " corresponding opening at ", parenthesisStack[$ - 1]);
                parenthesisStack.popBack();
            }
            
            if (parenthesisStack.empty)
            {
                trace("Stop at ", result);
                break;
            }
        }

        result++;
    }

    trace("Arguments end index = ", result);
    return result;
}

alias ParIndex = Tuple!(long, "startIndex", long, "endIndex");

ParIndex[] detectInnerFunctions(const string sourceStr)
{
    if (sourceStr.length < 3)
        return [];

    ParIndex[] result;
    long[] parenthesisStack;

    for (int i = 0; i < sourceStr.length; i++)
    {
        if (sourceStr[i] == CHAR_OPENING_PARENTHESIS)
        {
            parenthesisStack ~= i;
        }
        else if (sourceStr[i] == CHAR_CLOSING_PARENTHESIS)
        {
            if (!parenthesisStack.empty)
            {
                result ~= ParIndex(parenthesisStack[$ - 1], i);
                parenthesisStack.popBack();
            }
        }
    }

    return result;
}

TextSearchResult insideInnerFunctionCall(const long index, const ParIndex[] indeces)
{
    foreach (indexPair; indeces)
    {
        if (index > indexPair.startIndex && index < indexPair.endIndex)
            return TextSearchResult(indexPair.startIndex, indexPair.endIndex, true);
    }

    return TextSearchResult();
}

package alias ExtractResult = Tuple!(string[], "arguments", long, "endIndex");
// NOTE: regex may contain paired parenthesis which don't handled by grammar now
package ExtractResult extractFunctionArguments(
    const string functionName,
    const long requiredArgumentCount, const long optionalArgumentCount,
    const long startIndex, const string sourceLine, const int srcLine)
in
{
    assert(!functionName.empty);
    // NOTE: no arguments ==> nothing to extract
    assert(requiredArgumentCount >= 0);
    assert(optionalArgumentCount >= 0);
    assert(requiredArgumentCount + optionalArgumentCount >= 1);
    assert((startIndex >= 0) && startIndex < (sourceLine.length));
    assert(sourceLine.length > functionName.length);
}
out (result)
{
    assert(result.arguments.length >= requiredArgumentCount);
    assert(result.arguments.length <= requiredArgumentCount + optionalArgumentCount);
}
do
{
    trace("");
    trace("Caller source code line number: ", srcLine);
    trace("Source code line: " ~ sourceLine);
    trace("Source code line length: ", sourceLine.length);
    trace("Extracting function '" ~ functionName ~ "' arguments...");
    trace("Expect from ", requiredArgumentCount, " to ", requiredArgumentCount + optionalArgumentCount, " items");
    trace("");

    ExtractResult result;

    // Search for function call statement: function name + opening parenthehis
    immutable auto functionOpenParIndex = detectFunctionArgumentsStartIndex(functionName, sourceLine, startIndex);

    // Determine where function ends, i.e. final closing parenthesis, using stack collection
    immutable auto functionCloseParIndex = detectFunctionArgumentsEndIndex(sourceLine, functionOpenParIndex);

    string argumentString = sourceLine[functionOpenParIndex + 1 .. functionCloseParIndex];
    trace("Argument raw string: " ~ "'" ~ argumentString ~ "'");

    ParIndex[] innerParenthesisIndeces = detectInnerFunctions(argumentString);
    trace("Internal function calls indeces: ", innerParenthesisIndeces);

    // Find all of commas between opening and closing parenthesisis satisfying two requirements:
    // *  they must be outside enquoted arguments
    // *  they must be outside inner functions arguments
    long[] commaIndeces;
    for (int i = 0; i < argumentString.length; i++)
    {
        if ((argumentString[i] == CHAR_COMMA)
            && !isInsideQuotes(argumentString, i).success
            && !insideInnerFunctionCall(i, innerParenthesisIndeces).success)
        {
            tracef("comma at %d", i);
            commaIndeces ~= i;
        }
    }

    // Extract arguments using comma positions
    string[] arguments;
    long prevIndex = 0;
    foreach(commaIndex; commaIndeces)
    {
        arguments ~= joinTokens(argumentString, prevIndex, commaIndex - prevIndex).strip();
        prevIndex = commaIndex + 1;
    }
    // Leftover
    if (!commaIndeces.empty)
        arguments ~= joinTokens(argumentString, commaIndeces[$ - 1] + 1, argumentString.length - commaIndeces[$ - 1]).strip();
    else
        arguments ~= argumentString;

    trace("Argument array: ", arguments);

    result.arguments = arguments;
    result.endIndex = functionCloseParIndex + 1;
    return result;
}

bool isMultiline(const string sourceLine)
{
    return sourceLine.endsWith(STR_BACKSLASH);
}

bool mergeMultiline(const long startIndex, const string[] strLinesArray, out MultilineInfo result)
in
{
    //assert(isMultiline(lines[lineNo]));

    assert(startIndex >= 0 && startIndex < strLinesArray.length);
}
out(r)
{
    if (r)
        assert(!result.line.empty);
}
do
{
    LineInfo li, liNext;

    li.line = strLinesArray[startIndex];
    li.index = startIndex;
    li.mods = PreprocessorModifications.None;

    // 1) Corner case: one line
    prettifyLine(li);
    tracef("First line [%d]: |%s|", (startIndex + 1), li.line);
    if (!isMultiline(li.line))
    {
        trace("not a multi-line, skipping");
        return false;
    }

    // Remove trailing backslash
    result.line = li.line[0 .. $ - 1];
    assert(!isMultiline(result.line));

    // 2) Corner case: two lines at the end of file
    auto nextIndex = startIndex + 1;
    if (nextIndex == strLinesArray.length)
    {
        trace("Multi-line of two lines at the end of the file");

        result.startIndex = startIndex;
        result.endIndex = startIndex;
        return true;
    }

    // 3) Common case
    // Detect the multi-line end: the last one which don't contain backslash at the end
    long endIndex = startIndex;
    for (; nextIndex < strLinesArray.length; nextIndex++)
    {
        liNext.line = strLinesArray[nextIndex];
        liNext.index = nextIndex;
        liNext.mods = PreprocessorModifications.None;
        prettifyLine(liNext);
        tracef("Next line [%d]: |%s|", (nextIndex + 1), liNext.line);

        // Vanilla qmake don't consider empty lines as multi-line end
        if (liNext.line.empty)
        {
            trace("Skip empty line");
            continue;
        }

        if (isMultiline(liNext.line))
        {
            // Remove trailing backslash
            result.line ~= liNext.line[0 .. $ - 1];
            assert(!isMultiline(liNext.line[0 .. $ - 1]));
        }
        else
        {
            tracef("Last multi-line number: %d", (nextIndex + 1));
            result.line ~= liNext.line;

            endIndex = nextIndex;
            break;
        }
    }

    trace("startIndex: " ~ std.conv.to!string(startIndex + 1));
    trace("endIndex: " ~ std.conv.to!string(endIndex + 1));
    trace("result: |" ~ result.line ~ "|");

    result.startIndex = startIndex;
    result.endIndex = endIndex;
    return true;
}

bool hasSinglelineScope(const string sourceLine, out long colonIndex)
{
    colonIndex = INVALID_INDEX;

    if (sourceLine.empty || sourceLine.endsWith(STR_OPENING_CURLY_BRACE))
        return false;

    // testFunc(): x = y ... : ...
    auto lastEqIndex = sourceLine.lastIndexOf(STR_EQUALS);

    long i;
    if (lastEqIndex == INVALID_INDEX)
    {
        i = sourceLine.length - 1;
    }
    else if (isInsideQuotes(sourceLine, lastEqIndex).success)
    {
        trace("assignment operator detected inside quotes on position ", lastEqIndex);
        while ((lastEqIndex != INVALID_INDEX) && isInsideQuotes(sourceLine, lastEqIndex).success)
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

    ParIndex[] innerParenthesisIndeces = detectInnerFunctions(sourceLine);
    trace("Internal function calls indeces: ", innerParenthesisIndeces);

    for ( ; i >= 0; i--)
    {
        if (sourceLine[i] != CHAR_COLON)
            continue;

        trace("colon detected at index ", i);

        auto info1 = isInsideQuotes(sourceLine, i);
        if (info1.success)
        {
            i = info1.indexOpen - 1;
            trace("quote begin = ", info1.indexOpen);
            trace("quote end = ", info1.indexClose);
            trace("colon inside quotes detected, go back to index = ", i);
            continue;
        }

        // Special case: `requires` test function call;
        // it may contain colon in it's single parameter,
        // and cause false code block detection.
        // Examples:
        // requires(qtConfig(opengl):!wasm)
        // requires(linux:!android|macos)
        auto info2 = insideInnerFunctionCall(i, innerParenthesisIndeces);
        if (info2.success)
        {
            i = info2.indexOpen - 1;
            trace("function begin = ", info2.indexOpen);
            trace("function end = ", info2.indexClose);
            trace("colon inside " ~ REQUIRES_FUNCTION_STR ~ " function call detected, go back to index = ", i);
            continue;
        }

        colonIndex = i;
        trace("single-line block statement detected");
        return true;
    }
    return false;
}

bool hasSinglelineScope(const string sourceLine)
{
    long colonIndex;
    return hasSinglelineScope(sourceLine, colonIndex);
}

bool fixSinglelineScope(const string sourceLine, out string resultLine)
{
    resultLine = sourceLine;

    long colonIndex;
    if (!hasSinglelineScope(sourceLine, colonIndex))
        return false;

    trace("single-line scope statement detected and fixed");
    resultLine.replaceInPlace(colonIndex, colonIndex + 1, STR_DOG);
    return true;
}

bool hasMultilineScope(const string sourceLine, out long colonIndex)
{
    if (!sourceLine.endsWith(STR_OPENING_CURLY_BRACE))
        return false;

    // Search for reduntant colon
    // E.g.: contains(TEMPLATE, ".*app"):!build_pass: { ... }
    colonIndex = sourceLine.lastIndexOf(STR_COLON);
    if (colonIndex == INVALID_INDEX)
        return false;

    bool reduntant = true;
    for (auto i = colonIndex + 1; i < sourceLine.length - 1; i++)
    {
        if (!isWhite(sourceLine[i]))
        {
            reduntant = false;
            break;
        }
    }

    return reduntant;
}

bool hasMultilineScope(const string sourceLine)
{
    long colonIndex;
    return hasMultilineScope(sourceLine, colonIndex);
}

bool fixMultilineScope(const string sourceLine, out string resultLine)
{
    resultLine = sourceLine;

    long colonIndex;
    if (!hasMultilineScope(sourceLine, colonIndex))
        return false;

    assert(colonIndex >= 0);
    trace("single-line scope statement detected and fixed");

    resultLine.replaceInPlace(colonIndex, colonIndex + 1, STR_DOG);
    return true;
}

bool hasSinglelineScopeElse(const string sourceLine)
{
    return (sourceLine.indexOf(STR_ELSE_SINGLELINE) != INVALID_INDEX);
}

bool fixSinglelineScopeElse(const string sourceLine, out string resultLine)
{
    resultLine = sourceLine.replace(STR_ELSE_SINGLELINE, STR_ELSE ~ STR_DOG);
    return true;
}

void prettifyLine(ref LineInfo li)
{
    if (li.line.endsWith(STR_SEMICOLON))
    {
        li.mods |= PreprocessorModifications.TrailingSemicolonRemoved;
        li.line = li.line[0 .. $ - 1];
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

void fixMultiline(ref LineInfo li, ref MultilineInfo mresult, ref long lineIndex, const string[] strLinesArray)
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

void fixScope(ref LineInfo li)
{
    // NOTE: workaround for grammar ambiguity: cannot distingush AND-colon and single-line code block colon
    //
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

bool isEnquotedString(const string str)
{
    if (str.length < 2)
        return false;
    
    return (str.front == CHAR_DOUBLE_QUOTE) && (str.back == CHAR_DOUBLE_QUOTE);
}

string enquoteString(const string str)
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
void enquoteAmbiguousFunctionArguments(const string functionName,
    const long requiredArgumentCount, const long optionalArgumentCount,
    const long[] targetArgumentIndeces, ref LineInfo li)
{
    long functionCallIndex = 0;
    while (functionCallIndex < li.line.length)
    {
        auto functionCallPos = findFunctionCall(li.line, functionName, functionCallIndex);
        if (!functionCallPos)
            break;

        functionCallIndex = functionCallPos.indexOpen;

        auto replaceArguments = extractFunctionArguments(functionName,
            requiredArgumentCount, optionalArgumentCount,
            functionCallIndex, li.line, __LINE__);
        assert(replaceArguments.arguments.length >= requiredArgumentCount);
        assert(replaceArguments.arguments.length <= requiredArgumentCount + optionalArgumentCount);
        assert(replaceArguments.endIndex > functionCallIndex);

        // NOTE: function argument indeces are one-based, not zero-ones
        foreach (i; targetArgumentIndeces)
        {
            // Optional arguments may be absent
            if (i > replaceArguments.arguments.length)
                continue;

            string arg = strip(replaceArguments.arguments[i - 1]);
    
            if (!isEnquotedString(arg))
            {
                li.mods |= PreprocessorModifications.FunctionArgumentEnquoted;
                replaceArguments.arguments[i - 1] = enquoteString(arg);
            }
        }

        if (li.mods & PreprocessorModifications.FunctionArgumentEnquoted)
        {
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
        }

        functionCallIndex++;
    }
}

void fixAmbiguousFunctionCalls(ref LineInfo li)
{
    // E.g.:
    // replace(string, old_string, new_string)
    // qtConfig(opengl(es1|es2)?)
    // contains(var, regex)
    // system(/OUT:$$MYSCRIPT_VOIM_LIB_PATH)

    // Test functions

    // Enquote `exists` test function second argument
    enquoteAmbiguousFunctionArguments(EXISTS_FUNCTION_STR, 1, 0, [1], li);

    // Enquote `contains` test function second argument
    enquoteAmbiguousFunctionArguments(CONTAINS_FUNCTION_STR, 2, 1, [2, 3], li);

    // Enquote `qtConfig` test function first argument
    enquoteAmbiguousFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0, [1], li);

    // Enquote `error` test function first argument
    enquoteAmbiguousFunctionArguments(ERROR_FUNCTION_STR, 0, 1, [1], li);

    // Replace functions

    // Enquote `find` replace function second and third arguments
    enquoteAmbiguousFunctionArguments(FIND_FUNCTION_STR, 2, 0, [2], li);

    // Enquote `re_escape` replace function second and third arguments
    enquoteAmbiguousFunctionArguments(REESCAPE_FUNCTION_STR, 1, 0, [1], li);

    // Enquote `replace` replace function second and third arguments
    enquoteAmbiguousFunctionArguments(REPLACE_FUNCTION_STR, 3, 0, [2, 3], li);

    // Enquote `system` replace/test function first argument
    enquoteAmbiguousFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2, [1], li);
}

//---------------------------------------------------------------------------------------------------------------------

public string preprocessLines(const string[] strLinesArray, out LineInfo[] resultLines)
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

package string mergeMultilineHelper(const string[] strLinesArray)
{
    LineInfo li;
    MultilineInfo mresult;
    long lineIndex;

    li.index = 0;
    li.line = strLinesArray[0];
    li.mods = PreprocessorModifications.None;

    prettifyLine(li);
    fixMultiline(li, mresult, lineIndex, strLinesArray);
    if (li.mods & PreprocessorModifications.MultilineMerged)
        trace("Multi-line " ~ std.conv.to!string(mresult.startIndex + 1) ~
            " - " ~ std.conv.to!string(mresult.endIndex + 1) ~ ": |" ~ li.line ~ "|");
    else
        trace("Line " ~ std.conv.to!string(lineIndex + 1) ~ ": |" ~ li.line ~ "|");

    return li.line;
}
