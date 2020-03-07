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

//---------------------------------------------------------------------------------------------------------------------
private:

const auto STR_ELSE = "else";
const auto STR_ELSE_SINGLELINE = "else:";

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
    long startIndex = -1;
    long endIndex = -1;
    string line;
}

struct QuotesInfo
{
    long indexOpen  = -1;
    long indexClose = -1;
    bool success;
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

// FIXME: use this function instead of direct while loop
void skipWhitespaces(const string sourceLine, ref long index)
{
    while (isWhite(sourceLine[index]) && (index < sourceLine.length))
        index++;
}

bool isInsideParenthesis(const string sourceLine, const long index, const ParenhesisType parType,
    out long parIndex, bool findClosest = false)
{
    immutable long startIndex = (parType == ParenhesisType.Opening) ? 0     : (index + 1);
    immutable long endIndex   = (parType == ParenhesisType.Opening) ? index : sourceLine.length;

    long[] parenthesisStack;
    for (long i = startIndex; i < endIndex; i++)
    {
        if (sourceLine[i] == CHAR_OPENING_PARENTHESIS)
            parenthesisStack ~= i;
        else if (sourceLine[i] == CHAR_CLOSING_PARENTHESIS)
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
        trace("no open parenthesis found, stack.length = ", parenthesisStack.length);
    else
        trace("no close parenthesis found, stack.length = ", parenthesisStack.length);
    return false;
}

QuotesInfo detectFunctionArgument(const string functionName, const string sourceLine, const long index)
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

// TODO: rewrite code like in detectFunctionArgument
QuotesInfo isInsideQuotes(const string strLine, const long index)
in
{
    assert(index >= 0 && index < strLine.length);
}
out (result)
{
    assert((result.success && (result.indexOpen >= 0) && (result.indexClose < strLine.length)
        && (strLine[result.indexOpen] == CHAR_DOUBLE_QUOTE) && (strLine[result.indexClose] == CHAR_DOUBLE_QUOTE))
        || (!result.success) && (result.indexOpen == -1) && (result.indexClose == -1));
}
do
{
    // Naive optimization: enquoted char length must have length 3 or more
    if (strLine.length < 3)
    {
        trace("input string is too short to contain quotes");
        return QuotesInfo(-1, -1, false);
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
        return QuotesInfo(-1, -1, false);
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
        return QuotesInfo(-1, -1, false);
    }
    auto indexClose = stack[0];
    stack = [];

    return QuotesInfo(indexOpen, indexClose, true);
}

string cutInlineComment(const string sourceLine)
{
    bool commentFound;
    return cutInlineComment(sourceLine, commentFound);
}

string cutInlineComment(const string sourceLine, ref bool commentFound)
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

long detectFunctionArgumentsStartIndex(const string functionName, const string sourceStr, const long fromIndex)
in
{
    assert(sourceStr.indexOf(functionName ~ STR_OPENING_PARENTHESIS, fromIndex) >= 0);
}
out (result)
{
    assert(result >= 0 && result < sourceStr.length);
    assert(sourceStr[result] == CHAR_OPENING_PARENTHESIS);
}
do
{
    // Search for function call statement: function name + opening parenthehis
    //
    // FIXME: allow whitespace* between function name and parenthesis (use regex)
    immutable auto functionCallIndex = sourceStr.indexOf(functionName ~ STR_OPENING_PARENTHESIS, fromIndex);
    assert(functionCallIndex >= 0);

    immutable auto functionOpenParIndex = functionCallIndex + functionName.length;
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
// FIXME: in/out contracts
{
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

QuotesInfo insideInnerFunctionCall(const long index, const ParIndex[] indeces)
{
    foreach (indexPair; indeces)
    {
        if (index > indexPair.startIndex && index < indexPair.endIndex)
            return QuotesInfo(indexPair.startIndex, indexPair.endIndex, true);
    }

    return QuotesInfo(-1, -1, false);
}

alias ExtractResult = Tuple!(string[], "arguments", long, "endIndex");
// NOTE: regex may contain paired parenthesis which don't handled by grammar now
ExtractResult extractFunctionArguments(
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
    colonIndex = -1;

    if (sourceLine.empty || sourceLine.endsWith(STR_OPENING_CURLY_BRACE))
        return false;

    // testFunc(): x = y ... : ...
    auto lastEqIndex = sourceLine.lastIndexOf(STR_EQUALS);

    long i;
    if (lastEqIndex == -1)
    {
        i = sourceLine.length - 1;
    }
    else if (isInsideQuotes(sourceLine, lastEqIndex).success)
    {
        trace("assignment operator detected inside quotes on position ", lastEqIndex);
        while ((lastEqIndex != -1) && isInsideQuotes(sourceLine, lastEqIndex).success)
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
    if (colonIndex == -1)
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
    return (sourceLine.indexOf(STR_ELSE_SINGLELINE) != -1);
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
        // FIXME: replace with regex to allow whitespaces between function name and opening parenthesis
        functionCallIndex = li.line.indexOf(functionName ~ STR_OPENING_PARENTHESIS, functionCallIndex);
        if (functionCallIndex == -1)
            break;

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

    // FIXME: add error handling above, e.g. exception generation
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

//---------------------------------------------------------------------------------------------------------------------

unittest
{
    string mergeMultilineHelper(const string[] strLinesArray)
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

    // NOTE: These examples were extracted from clang_64/mkspecs directory

    // Function name         | Function type | Ambiguous argument indeces
    // EXISTS_FUNCTION_STR   | test          | argument 1
    // CONTAINS_FUNCTION_STR | test          | argument 2
    // QTCONFIG_FUNCTION_STR | test          | argument 1
    // ERROR_FUNCTION_STR    | test          | argument 1
    // SYSTEM_FUNCTION_STR   | test/replace  | argument 1
    // FIND_FUNCTION_STR     | replace       | argument 2
    // REESCAPE_FUNCTION_STR | replace       | argument 1
    // REPLACE_FUNCTION_STR  | replace       | arguments 2 and 3

    writeln("Starting preprocessor unit tests...");
 
    ExtractResult result;
    string[] strLinesArray;

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 1) exists

    // Single-line test snippets
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$NDK_ROOT): error(\"You need to set the ANDROID_NDK_ROOT environment variable to point to your Android NDK.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$NDK_ROOT");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$absolute_path($$bundle_file/AppIcon.appiconset, $$_PRO_FILE_PWD_)): next()", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$absolute_path($$bundle_file/AppIcon.appiconset, $$_PRO_FILE_PWD_)");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$test_out_dir/Makefile):qtRunLoggedCommand(\"$$test_cmd_base $$QMAKE_MAKE distclean\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$test_out_dir/Makefile");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$cmake_extras_file.input)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$cmake_extras_file.input");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$DEVICE_PRI):include($$DEVICE_PRI)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$DEVICE_PRI");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$QMLTYPEFILE): AUX_QML_FILES += $$QMLTYPEFILE", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMLTYPEFILE");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$qmod/qml): importpath.value += $$shell_path($$qmod/qml)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$qmod/qml");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$QMAKE_DOCS): error(\"Cannot find documentation specification file $$QMAKE_DOCS\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_DOCS");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$cmd): QT_TOOL.repc.binary = $$cmd", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$cmd");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$UUID_CACHE): include($$UUID_CACHE)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$UUID_CACHE");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$QMAKE_QT_MODULE)|!include($$QMAKE_QT_MODULE, \"\", true):debug(1, \"Cannot load qmodule.pri!\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_QT_MODULE");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$OUT_PWD/qt$${MODULE}-config.pri): include($$OUT_PWD/qt$${MODULE}-config.pri)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$OUT_PWD/qt$${MODULE}-config.pri");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$pfx/.qmake.cache): logn(\"Once everything is built, Qt is installed.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$pfx/.qmake.cache");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$QMAKE_QT_CONFIG)|!include($$QMAKE_QT_CONFIG, \"\", true): debug(1, \"Cannot load qconfig.pri!\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_QT_CONFIG");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$OUT_PWD/config.opt): qtConfAddError(\"No config.opt present - cannot redo configuration.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$OUT_PWD/config.opt");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$lp/.): qtLog(\"Library path $$val_escape(lp) is invalid.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$lp/.");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$libdir/.): qtLog(\"Library path $$val_escape(libdir) is invalid.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$libdir/.");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$incdir/.): qtLog(\"Include path $$val_escape(incdir) is invalid.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$incdir/.");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($${cmd}.pl): $${1}_EXE = $${cmd}.pl", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$${cmd}.pl");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$BUNDLENAME): cmd = $$BUNDLENAME", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$BUNDLENAME");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists(\"$$sysroot/usr/lib/pkgconfig\"): return()", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$sysroot/usr/lib/pkgconfig\"");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$OUT_PWD/$${MODULE_CFG_FILE}.pri): include($$OUT_PWD/$${MODULE_CFG_FILE}.pri)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$OUT_PWD/$${MODULE_CFG_FILE}.pri");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$OUT_PWD/$${MODULE_CFG_FILE}.h): fwd_rel = $$relative_path($$OUT_PWD, $$REAL_MODULE_BASE_OUTDIR)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$OUT_PWD/$${MODULE_CFG_FILE}.h");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$_PRO_FILE_PWD_/src/src.pro): sub_src.subdir = src", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$_PRO_FILE_PWD_/src/src.pro");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$_PRO_FILE_PWD_/examples/examples.pro): sub_examples.subdir = examples", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$_PRO_FILE_PWD_/examples/examples.pro");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($${include}/parser.g):msvc:QMAKE_CXXFLAGS += /wd4129", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$${include}/parser.g");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$ANDROID_JAR_FILE): ANDROID_API_VERSION = $$section(API_VERSION_TO_USE, -, 1, 1)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$ANDROID_JAR_FILE");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$dir/sh.exe): QMAKE_SH = $$dir/sh.exe", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$dir/sh.exe");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         1, "!exists($$VXWORKS_MUNCH_TOOL): error(\"Could not find VxWorks Munch tool: '$${VXWORKS_MUNCH_TOOL}'\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$VXWORKS_MUNCH_TOOL");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
         0, "exists($$[QT_INSTALL_PLUGINS]/platforms/wasm_shell.html): WASM_PLUGIN_PATH = $$[QT_INSTALL_PLUGINS]/platforms", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$[QT_INSTALL_PLUGINS]/platforms/wasm_shell.html");
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        18, "watchos:simulator:!exists($$simulator_system_frameworks/CoreText.framework/Headers/CoreText.h): device_system_frameworks = $$xcodeSDKInfo(Path, $${device.sdk})/System/Library/Frameworks", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$simulator_system_frameworks/CoreText.framework/Headers/CoreText.h");

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= "exists($$ANDROID_SOURCES_CXX_STL_LIBDIR/libc++.so): \\";
    strLinesArray ~= "    ANDROID_CXX_STL_LIBS = -lc++";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$ANDROID_SOURCES_CXX_STL_LIBDIR/libc++.so");

    strLinesArray = [];
    strLinesArray ~= "!exists($$CMAKE_MODULE_TESTS): \\";
    strLinesArray ~= "    error(\"Missing CMake tests. Either create tests in tests/auto/cmake,\" \\";
    strLinesArray ~= "          \"or disable cmake config file creation with CONFIG-=create_cmake.\")";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$CMAKE_MODULE_TESTS");

    strLinesArray = [];
    strLinesArray ~= "!exists($$QMAKE_XCODE_DEVELOPER_PATH): \\";
    strLinesArray ~= "    error(\"Xcode is not installed in $${QMAKE_XCODE_DEVELOPER_PATH}. Please use xcode-select to choose Xcode installation path.\")";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_XCODE_DEVELOPER_PATH");

    strLinesArray = [];
    strLinesArray ~= "exists($$QMAKE_XCODE_PREFERENCES_FILE): \\";
    strLinesArray ~= "   QMAKE_TARGET_BUNDLE_PREFIX = $$system(\"/usr/libexec/PlistBuddy -c 'print IDETemplateOptions:bundleIdentifierPrefix' $$QMAKE_XCODE_PREFERENCES_FILE 2>/dev/null\")";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_XCODE_PREFERENCES_FILE");

    strLinesArray = [];
    strLinesArray ~= "exists($$NDK_ROOT/sysroot/usr/include): \\";
    strLinesArray ~= "        QMAKE_CFLAGS += --sysroot=$$NDK_ROOT/sysroot \\";
    strLinesArray ~= "                        -isystem $$NDK_ROOT/sysroot/usr/include/$$NDK_TOOLS_PREFIX";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$NDK_ROOT/sysroot/usr/include");

    strLinesArray = [];
    strLinesArray ~= "isEmpty(QMAKE_PLUGINDUMP_DEPENDENCIES_FILE):exists($$_PRO_FILE_PWD_/dependencies.json): \\";
    strLinesArray ~= "    QMAKE_PLUGINDUMP_DEPENDENCIES_FILE = $$_PRO_FILE_PWD_/dependencies.json";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
       44, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$_PRO_FILE_PWD_/dependencies.json");
    
    strLinesArray = [];
    strLinesArray ~= "exists($$[QT_INSTALL_QML/get]): \\";
    strLinesArray ~= "    QMLPATHS *= $$[QT_INSTALL_QML/get]";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$[QT_INSTALL_QML/get]");

    strLinesArray = [];
    strLinesArray ~= "!exists($$[QT_HOST_DATA]/.qmake.cache): \\";
    strLinesArray ~= "    CONFIG += prefix_build force_independent";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$[QT_HOST_DATA]/.qmake.cache");
    
    strLinesArray = [];
    strLinesArray ~= "exists($$qrep/qml): \\";
    strLinesArray ~= "   QMLPATHS += $$qrep/qml";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$qrep/qml");

    strLinesArray = [];
    strLinesArray ~= "exists($$MODULE_BASE_INDIR/.git): \\";
    strLinesArray ~= "    CONFIG += git_build";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$MODULE_BASE_INDIR/.git");

    strLinesArray = [];
    strLinesArray ~= "exists(\"$$dir/$$file\"): \\";
    strLinesArray ~= "    return(\"$$dir/$$file\")";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$dir/$$file\"");

    strLinesArray = [];
    strLinesArray ~= "exists($$test_out_dir/Makefile): \\";
    strLinesArray ~= "    QMAKE_MAKE = \"$$QMAKE_MAKE clean && $$QMAKE_MAKE\"";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$test_out_dir/Makefile");

    strLinesArray = [];
    strLinesArray ~= "exists($$s/configure.json): \\";
    strLinesArray ~= "    configsToProcess += $$c";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$s/configure.json");
    
    strLinesArray = [];
    strLinesArray ~= "exists($$priFile): \\";
    strLinesArray ~= "    !include($$priFile): error()";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$priFile");

    strLinesArray = [];
    strLinesArray ~= "exists($$extra): \\";
    strLinesArray ~= "    sourcefiles += $$extra";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$extra");

    strLinesArray = [];
    strLinesArray ~= "exists($$BLACKLISTPATH): \\";
    strLinesArray ~= "    testdata.files *= $$BLACKLISTPATH";
    result = extractFunctionArguments(EXISTS_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$BLACKLISTPATH");

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 2) contains

    // Single-line test snippets
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         1, `!contains(TARGET, ".so"): TARGET = lib$${TARGET}.so`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TARGET`);
    assert(result.arguments[1] == `".so"`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         1, `!contains(bundle_file, .*\\.xcassets$): next()`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `bundle_file`);
    assert(result.arguments[1] == `.*\\.xcassets$`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
        27, `if(!equals(v, -framework):!contains(v, -L.*)): v ~= s,^-l,,`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `v`);
    assert(result.arguments[1] == `-L.*`);
    
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(TEMPLATE, ".*app"): QMAKE_LFLAGS += $$QMAKE_LFLAGS_EXE`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `".*app"`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(CMAKE_INSTALL_LIBS_DIR, ^(/usr)?/lib(64)?.*): CMAKE_USR_MOVE_WORKAROUND = $$CMAKE_INSTALL_LIBS_DIR`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `CMAKE_INSTALL_LIBS_DIR`);
    assert(result.arguments[1] == `^(/usr)?/lib(64)?.*`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(CMAKE_INCLUDE_DIR, "^\\.\\./.*"): CMAKE_INCLUDE_DIR = $$[QT_INSTALL_HEADERS]/`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `CMAKE_INCLUDE_DIR`);
    assert(result.arguments[1] == `"^\\.\\./.*"`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(CONFIG, plugin): debug("test")`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `CONFIG`);
    assert(result.arguments[1] == `plugin`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(QT.$${mod}.plugin_types, $$PLUGIN_TYPE): debug("test")`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT.$${mod}.plugin_types`);
    assert(result.arguments[1] == `$$PLUGIN_TYPE`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         1, `!contains(QT.$${dep}.module_config, no_link): mod_deps += $$cdep`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT.$${dep}.module_config`);
    assert(result.arguments[1] == `no_link`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         1, `!contains(subent, .*\\w\\.xml$): next()`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `subent`);
    assert(result.arguments[1] == `.*\\w\\.xml$`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(TEMPLATE, .*app): QMAKE_EXTRA_INCLUDES += $$shell_quote($$PWD/sdk.mk)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `.*app`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(VALID_SIMULATOR_ARCHS, $$arch): sdk = $$simulator.sdk`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `VALID_SIMULATOR_ARCHS`);
    assert(result.arguments[1] == `$$arch`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(TEMPLATE, ".*(lib|app)"): CONFIG += have_target`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `".*(lib|app)"`);
    
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         1, `!contains(TEMPLATE, vc.*): hdr = $$basename(tlb)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `vc.*`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains($$dir, .*$${exclusive_affix}.*): $$dir ~= s/$${exclusive_affix}/$${build_affix}/gi`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `$$dir`);
    assert(result.arguments[1] == `.*$${exclusive_affix}.*`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(TEMPLATE, "vc.*"): copycommand = $$QMAKE_QMAKE -install qinstall`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `"vc.*"`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(QMAKE_LEX, .*flex): lex.commands = $$QMAKE_LEX`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QMAKE_LEX`);
    assert(result.arguments[1] == `.*flex`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         1, `!contains(DISTRO_OPTS, aarch64): COMPILER_FLAGS += -mfloat-abi=softfp`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `DISTRO_OPTS`);
    assert(result.arguments[1] == `aarch64`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(DISTRO_OPTS, boot2qt): QMAKE_PLATFORM += boot2qt`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `DISTRO_OPTS`);
    assert(result.arguments[1] == `boot2qt`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(QMAKE_TARGET.arch, x86_64): DEFINES += WIN64`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QMAKE_TARGET.arch`);
    assert(result.arguments[1] == `x86_64`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         1, `!contains(WINRT_MANIFEST.CONFIG, "verbatim"):equals(TEMPLATE, "app"): VCLIBS = $${VCLIBS}`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `WINRT_MANIFEST.CONFIG`);
    assert(result.arguments[1] == `"verbatim"`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(MSVC_VER, "15.0"): VCLIBS = $$replace(VCLIBS, 150, 140)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `MSVC_VER`);
    assert(result.arguments[1] == `"15.0"`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(WINRT_MANIFEST.capabilities, defaults): WINRT_MANIFEST.capabilities -= defaults`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `WINRT_MANIFEST.capabilities`);
    assert(result.arguments[1] == `defaults`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(DISTRO, squeeze): QMAKE_LIBS_OPENGL_ES2 = -lGLESv2 -lEGL`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `DISTRO`);
    assert(result.arguments[1] == `squeeze`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(B_REFSW_DEBUG, [Nn]): BRCM_BUILD_TYPE = release`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `B_REFSW_DEBUG`);
    assert(result.arguments[1] == `[Nn]`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         1, `!contains(use, linkonly): CC_USES += $$nu`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `use`);
    assert(result.arguments[1] == `linkonly`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(qmlf,.*\\.js$)|contains(qmlf,.*\\.qml$): CACHEGEN_FILES += $$absolute_path($$qmlf, $$_PRO_FILE_PWD_)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `qmlf`);
    assert(result.arguments[1] == `.*\\.js$`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(qt_depends, gui(-private)?): LIBS *= -L$$[QT_INSTALL_PLUGINS/get]/platforms`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `qt_depends`);
    assert(result.arguments[1] == `gui(-private)?`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(name, ^\\.\\..*): name = $$relative_path($$1, $$OUT_PWD)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `^\\.\\..*`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(rccContents,.*\\.js$)|contains(rccContents,.*\\.qml$)|contains(rccContents,.*\\.mjs$): new_resource = $$qmlCacheResourceFileOutputName($$res)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `rccContents`);
    assert(result.arguments[1] == `.*\\.js$`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(QT, core(-private)?|xml): QT -= core core-private xml`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT`);
    assert(result.arguments[1] == `core(-private)?|xml`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         1, `!contains(QMAKE_INTERNAL_INCLUDED_FILES, .*qmodule\\.pri): QMAKE_QT_MODULE = $$[QT_HOST_DATA/get]/mkspecs/qmodule.pri`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QMAKE_INTERNAL_INCLUDED_FILES`);
    assert(result.arguments[1] == `.*qmodule\\.pri`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(apple_ver, "4\\.[012]|5\\.[01]")|contains(reg_ver, "[345]\\.|6\\.0"): QMAKE_CXXFLAGS_WARN_ON += -Werror`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `apple_ver`);
    assert(result.arguments[1] == `"4\\.[012]|5\\.[01]"`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         6, `linux:contains(ver, "(1[345678]\\.|19\\.0)"): QMAKE_CXXFLAGS_WARN_ON += -Werror`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `ver`);
    assert(result.arguments[1] == `"(1[345678]\\.|19\\.0)"`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(ver, "(4\\.[6789]|[5-9]\\..)"): QMAKE_CXXFLAGS_WARN_ON += -Werror`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `ver`);
    assert(result.arguments[1] == `"(4\\.[6789]|[5-9]\\..)"`);

    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
         0, `contains(MSVC_VER, "1[124].0"): QMAKE_CXXFLAGS_WARN_ON += -WX`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `MSVC_VER`);
    assert(result.arguments[1] == `"1[124].0"`);

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= `!contains(QT.$${dep}.module_config, no_link): \`;
    strLinesArray ~= `    mod_deps += $$cmakeModuleName($$dep)`;
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT.$${dep}.module_config`);
    assert(result.arguments[1] == `no_link`);

    strLinesArray = [];
    strLinesArray ~= `!contains(TARGET, .*phony_target.*): \   # monster hack, you don't really see this here, right? ;)`;
    strLinesArray ~= `    system($$QT_BREAKPAD_ROOT_PATH/qtbreakpadsymbols`;
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TARGET`);
    assert(result.arguments[1] == `.*phony_target.*`);

    strLinesArray = [];
    strLinesArray ~= `$$sim_and_dev|contains(QMAKE_MAC_SDK, ^$${device.sdk}.*): \`;
    strLinesArray ~= `    CONFIG += device $${device.sdk}`;
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
       14, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QMAKE_MAC_SDK`);
    assert(result.arguments[1] == `^$${device.sdk}.*`);

    strLinesArray = [];
    strLinesArray ~= `contains(DISTRO_OPTS, deb-multi-arch): \`;
    strLinesArray ~= `   QMAKE_PKG_CONFIG = $${CROSS_COMPILE}pkg-config`;
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `DISTRO_OPTS`);
    assert(result.arguments[1] == `deb-multi-arch`);

    strLinesArray = [];
    strLinesArray ~= `contains(UAP_CAPABILITIES, $$CAPABILITY): \`;
    strLinesArray ~= `   MANIFEST_CAPABILITIES += \"  <uap:Capability Name=\"$$CAPABILITY\" />\"`;
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `UAP_CAPABILITIES`);
    assert(result.arguments[1] == `$$CAPABILITY`);

    strLinesArray = [];
    strLinesArray ~= `contains(flag, ^-.*): \`;
    strLinesArray ~= `   $$1 -= $$replace(flag, ^-, )`;
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `flag`);
    assert(result.arguments[1] == `^-.*`);

    strLinesArray = [];
    strLinesArray ~= `contains(QT.$${module}.enabled_features, $$1): \`;
    strLinesArray ~= `   return(true)`;
    result = extractFunctionArguments(CONTAINS_FUNCTION_STR, 2, 0,
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT.$${module}.enabled_features`);
    assert(result.arguments[1] == `$$1`);

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 3) qtConfig

    // Single-line test snippets
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        19, `qtHaveModule(gui):!qtConfig(egl): CMAKE_GL_DEFINES += -DNO_EGL=True`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `egl`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         0, `qtConfig(c++11):CONFIG += c++11`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `c++11`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         0, `qtConfig(opengles2): INCLUDEPATH += $$QMAKE_INCDIR_OPENGL_ES2`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `opengles2`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         0, `qtConfig(opengles2):qtConfig(combined-angle-lib):QMAKE_LIBS_OPENGL_ES2 = -l$${LIBQTANGLE_NAME} $$QMAKE_LIBS_OPENGL_ES2`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `opengles2`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         0, `qtConfig(debug_and_release): CONFIG += debug_and_release`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `debug_and_release`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         6, `win32:qtConfig(shared):QMAKE_DLL_PATHS += $$[QT_INSTALL_BINS/get]`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `shared`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         0, `qtConfig(force_asserts): DEFINES += QT_FORCE_ASSERTS`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `force_asserts`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        15, `import_plugins:qtConfig(static):manualplugs = $$QTPLUGIN`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        18, `isEmpty(QTPLUGIN):qtConfig(static):plug_type = $$eval(QT_PLUGIN.$${plug}.TYPE)`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        39, `host_build:force_bootstrap:!build_pass:qtConfig(release_tools): CONFIG += release`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `release_tools`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        12, `!build_pass:qtConfig(debug_and_release): CONFIG += release`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `debug_and_release`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         0, `qtConfig(c++11): CONFIG += c++11 strict_c++`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `c++11`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         0, `qtConfig(stack-protector-strong): CONFIG += stack_protector_strong`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `stack-protector-strong`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        31, `if(!host_build|!cross_compile):qtConfig(reduce_exports): CONFIG += hide_symbols`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `reduce_exports`);    

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         5, `unix:qtConfig(reduce_relocations): CONFIG += bsymbolic_functions`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `reduce_relocations`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         0, `qtConfig(separate_debug_info): CONFIG += separate_debug_info`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `separate_debug_info`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        30, `CONFIG(shared, static|shared):qtConfig(framework):export(QMAKE_FRAMEWORK_BUNDLE_NAME)`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `framework`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        28, `installed|if(!not_installed:qtConfig(static)): load(qt_installs)`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        11, `!if(static:qtConfig(shared)):QMAKE_CFLAGS += $$QMAKE_CFLAGS_SSE2`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `shared`);

    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
         4, `if(!qtConfig(debug_and_release)|CONFIG(release, debug|release)):CLEAN_HEADERS =`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `debug_and_release`);

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= `!isEmpty(QT_VERSION):qtConfig(simulator_and_device): \`;
    strLinesArray ~= `    sim_and_dev = true`;
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
       21, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `simulator_and_device`);

    strLinesArray = [];
    strLinesArray ~= `macx-xcode:qtConfig(static): \`;
    strLinesArray ~= `   QMAKE_XCODE_DEBUG_INFORMATION_FORMAT = dwarf`;
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
       11, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    strLinesArray = [];
    strLinesArray ~= `!no_qt_rpath:!static:qtConfig(rpath):!qtConfig(static):\`;
    strLinesArray ~= `    contains(all_qt_module_deps, core)\`;
    strLinesArray ~= `    QMAKE_RPATHDIR += $$[QT_INSTALL_LIBS/dev]`;
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
       21, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `rpath`);

    strLinesArray = [];
    strLinesArray ~= `!qtConfig(static)|host_build|no_import_scan: \`;
    strLinesArray ~= `    return()`;
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        1, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    strLinesArray = [];
    strLinesArray ~= `qtConfig(framework): \`;
    strLinesArray ~= `    deppath.name = DYLD_FRAMEWORK_PATH`;
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `framework`);
    
    strLinesArray = [];
    strLinesArray ~= `qtConfig(rpath): \`;
    strLinesArray ~= `   QMAKE_SONAME_PREFIX = @rpath`;
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `rpath`);
    
    strLinesArray = [];
    strLinesArray ~= `qtConfig(private_tests): \   # -developer-build`;
    strLinesArray ~= `    QMAKE_SYNCQT += -check-includes`;
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `private_tests`);
    
    strLinesArray = [];
    strLinesArray ~= `isEmpty(hr)|qtConfig($$hr): \`;
    strLinesArray ~= `    CLEAN_HEADERS += $$member(hh, 0)`;
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
       12, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `$$hr`);
    
    strLinesArray = [];
    strLinesArray ~= `!qtConfig(simulator_and_device):contains(QMAKE_MAC_SDK, ^$${simulator.sdk}.*): \`;
    strLinesArray ~= `    addExclusiveBuildsProper(simulator_and_device, simulator device)`;
    result = extractFunctionArguments(QTCONFIG_FUNCTION_STR, 1, 0,
        1, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `simulator_and_device`);

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 4) error

    // Single-line test snippets
    result = extractFunctionArguments(ERROR_FUNCTION_STR, 1, 0,
        61, `write_file($$ANDROID_DEPLOYMENT_SETTINGS_FILE, FILE_CONTENT)|error()`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == ``);

    result = extractFunctionArguments(ERROR_FUNCTION_STR, 1, 0,
         0, `error("QMAKE_ASSET_CATALOGS_BUILD_PATH must be set when using QMAKE_ASSET_CATALOGS.")`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `"QMAKE_ASSET_CATALOGS_BUILD_PATH must be set when using QMAKE_ASSET_CATALOGS."`);

    result = extractFunctionArguments(ERROR_FUNCTION_STR, 1, 0,
         0, `error("Multiple modules claim plugin type '$$PLUGIN_TYPE' ($$mod, in addition to $$PLUGIN_MODULE_NAME)")`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `"Multiple modules claim plugin type '$$PLUGIN_TYPE' ($$mod, in addition to $$PLUGIN_MODULE_NAME)"`);

    result = extractFunctionArguments(ERROR_FUNCTION_STR, 1, 0,
        21, `!load(device_config):error(Could not successfully load device configuration)`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `Could not successfully load device configuration`);

    result = extractFunctionArguments(ERROR_FUNCTION_STR, 1, 0,
         0, `error($$join(msg, $$escape_expand(\\n)))`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `$$join(msg, $$escape_expand(\\n))`);

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= `!exists($$CMAKE_MODULE_TESTS): \`;
    strLinesArray ~= `    error("Missing CMake tests. Either create tests in tests/auto/cmake," \`;
    strLinesArray ~= `          "or disable cmake config file creation with CONFIG-=create_cmake.")`;
    result = extractFunctionArguments(ERROR_FUNCTION_STR, 1, 0,
       31, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `"Missing CMake tests. Either create tests in tests/auto/cmake," "or disable cmake config file creation with CONFIG-=create_cmake."`);

    strLinesArray = [];
    strLinesArray ~= `isEmpty(VALID_ARCHS): \`;
    strLinesArray ~= `    error("QMAKE_APPLE_DEVICE_ARCHS or QMAKE_APPLE_SIMULATOR_ARCHS must contain at least one architecture")`;
    result = extractFunctionArguments(ERROR_FUNCTION_STR, 1, 0,
       22, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `"QMAKE_APPLE_DEVICE_ARCHS or QMAKE_APPLE_SIMULATOR_ARCHS must contain at least one architecture"`);

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 5) system test/replace function tests

    // Corner case 1: empty argument
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        0, "system(uname -a, , ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "uname -a");
    assert(result.arguments[1] == "");
    assert(result.arguments[2] == "ec");
    
    // Single-line test snippets
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
         1, "!system($$pkg_config --exists $$package):return(false)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$pkg_config --exists $$package");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
         8, "ret = $$system(\"$$1 -E $$system_quote($$PWD/data/macros.cpp) 2>$$QMAKE_SYSTEM_NULL_DEVICE\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$1 -E $$system_quote($$PWD/data/macros.cpp) 2>$$QMAKE_SYSTEM_NULL_DEVICE\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
         2, "$$system(\"$$QMAKE_CXX -print-libgcc-file-name\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$QMAKE_CXX -print-libgcc-file-name\"");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        14, "sysrooted = $$system(\"/usr/bin/xcrun -sdk $$QMAKE_MAC_SDK -find $$first(value) 2>/dev/null\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"/usr/bin/xcrun -sdk $$QMAKE_MAC_SDK -find $$first(value) 2>/dev/null\"");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        11, "output = $$system(\"$$1\", lines, result)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$1\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "result");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        25, "cmake_version_output = $$system(cmake --version 2>$$QMAKE_SYSTEM_NULL_DEVICE, lines)", __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == "cmake --version 2>$$QMAKE_SYSTEM_NULL_DEVICE");
    assert(result.arguments[1] == "lines");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        18, "CTEST_VERSION = $$system(ctest --version 2>$$QMAKE_SYSTEM_NULL_DEVICE)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "ctest --version 2>$$QMAKE_SYSTEM_NULL_DEVICE");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
         0, "system($$QT_BREAKPAD_ROOT_PATH/qtbreakpadsymbols --breakpad-exists)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QT_BREAKPAD_ROOT_PATH/qtbreakpadsymbols --breakpad-exists");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        21, "PKGCONFIG_CFLAGS = $$system($$PKG_CONFIG --cflags $$PKGCONFIG_LIB)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$PKG_CONFIG --cflags $$PKGCONFIG_LIB");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
         9, "JSON = $$system($$QMLIMPORTSCANNER $$system_quote($$_PRO_FILE_PWD_) $$IMPORTPATHS)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMLIMPORTSCANNER $$system_quote($$_PRO_FILE_PWD_) $$IMPORTPATHS");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
         0, "system($$QMAKE_SYNCQT)|error(\"Failed to run: $$QMAKE_SYNCQT\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_SYNCQT");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        16, "rccContents = $$system($$QMAKE_RCC_DEP -list $$system_quote($$absRes),lines)", __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == "$$QMAKE_RCC_DEP -list $$system_quote($$absRes)");
    assert(result.arguments[1] == "lines");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        20, "remaining_files = $$system($$QML_CACHEGEN_FILTER -filter-resource-file -o $$system_quote($$new_resource) $$system_quote($$absRes),lines)", __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == "$$QML_CACHEGEN_FILTER -filter-resource-file -o $$system_quote($$new_resource) $$system_quote($$absRes)");
    assert(result.arguments[1] == "lines");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
         8, "ret = $$system(\"$$1 -nologo -E $$2 $$system_quote($$PWD/data/macros.cpp) 2>NUL\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$1 -nologo -E $$2 $$system_quote($$PWD/data/macros.cpp) 2>NUL\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        11, "output = $$system(\"$$cmd_prefix $$QMAKE_CXX $$qtMakeExpand($$cxx_flags) -xc++ - 2>&1 $$cmd_suffix\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$cmd_prefix $$QMAKE_CXX $$qtMakeExpand($$cxx_flags) -xc++ - 2>&1 $$cmd_suffix\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        11, "output = $$system(\"$$cmd_prefix $$QMAKE_LINK $$QMAKE_LFLAGS -print-search-dirs\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$cmd_prefix $$QMAKE_LINK $$QMAKE_LFLAGS -print-search-dirs\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        11, "output = $$system(\"$$cmd\", blob, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$cmd\"");
    assert(result.arguments[1] == "blob");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        11, "output = $$system(\"$$cmd 2>&1\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$cmd 2>&1\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
         0, "system(\"$$QMAKE_CD $$system_quote($$OUT_PWD) && $$cmd\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$QMAKE_CD $$system_quote($$OUT_PWD) && $$cmd\"");
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        15, "WINRT_UUID = $$system(uuidgen)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "uuidgen");

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= "system(\"/usr/libexec/PlistBuddy -c 'Print NSPhotoLibraryUsageDescription' $$system_quote($$plist_path) &>/dev/null\"): \\";
    strLinesArray ~= "    QTPLUGIN += qiosnsphotolibrarysupport";
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"/usr/libexec/PlistBuddy -c 'Print NSPhotoLibraryUsageDescription' $$system_quote($$plist_path) &>/dev/null\"");

    strLinesArray = [];
    strLinesArray ~= "ret = $$system(\"$$1 -E $$system_quote($$PWD/data/macros.cpp) \\";
    strLinesArray ~= "     2>$$QMAKE_SYSTEM_NULL_DEVICE\", lines, ec)";
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$1 -E $$system_quote($$PWD/data/macros.cpp) 2>$$QMAKE_SYSTEM_NULL_DEVICE\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");

    strLinesArray = [];
    strLinesArray ~= "!system(\"$$system_quote($$system_path($$[QT_HOST_BINS/src]/$$QT_LICHECK)) check\" \\";
    strLinesArray ~= "        \"$$QT_RELEASE_DATE $$[QMAKE_SPEC] $$[QMAKE_XSPEC]\"): \\";
    strLinesArray ~= "     error(\"License check failed! Giving up ...\")";
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$system_quote($$system_path($$[QT_HOST_BINS/src]/$$QT_LICHECK)) check\" \"$$QT_RELEASE_DATE $$[QMAKE_SPEC] $$[QMAKE_XSPEC]\"");

    strLinesArray = [];
    strLinesArray ~= "actool_output_files = $$system(\\";
    strLinesArray ~= "        mkdir -p $$system_quote($$QMAKE_ASSET_CATALOGS_BUILD_PATH) && \\";
    strLinesArray ~= "        /usr/libexec/PlistBuddy -c \\'Print :com.apple.actool.compilation-results:output-files\\' \\";
    strLinesArray ~= "            /dev/stdin <<< $($${asset_catalog_compiler.commands} 2>/dev/null) | sed -Ene \\'s/^ +//p\\', lines)";
    result = extractFunctionArguments(SYSTEM_FUNCTION_STR, 1, 2,
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == "mkdir -p $$system_quote($$QMAKE_ASSET_CATALOGS_BUILD_PATH) && /usr/libexec/PlistBuddy -c \\'Print :com.apple.actool.compilation-results:output-files\\' /dev/stdin <<< $($${asset_catalog_compiler.commands} 2>/dev/null) | sed -Ene \\'s/^ +//p\\'");
    assert(result.arguments[1] == "lines");

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 6) find

    // Single-line test snippets
    result = extractFunctionArguments(FIND_FUNCTION_STR, 2, 0,
        26, `PKGCONFIG_INCLUDEPATH = $$find(PKGCONFIG_CFLAGS, ^-I.*)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `PKGCONFIG_CFLAGS`);
    assert(result.arguments[1] == `^-I.*`);

    result = extractFunctionArguments(FIND_FUNCTION_STR, 2, 0,
        16, `ICONS_FOUND = $$find(TEMPLATE_CONTENTS, \\\$\\\$\\{WINRT_MANIFEST\\.(logo|tile)_)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE_CONTENTS`);
    assert(result.arguments[1] == `\\\$\\\$\\{WINRT_MANIFEST\\.(logo|tile)_`);

    result = extractFunctionArguments(FIND_FUNCTION_STR, 2, 0,
        35, `parser = $$lower($$member($$list($$find(sfl, "^%parser\\s")), 1))`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `sfl`);
    assert(result.arguments[1] == `"^%parser\\s"`);

    result = extractFunctionArguments(FIND_FUNCTION_STR, 2, 0,
        25, `decl = $$member($$list($$find(sfl, "^%decl\\s")), 1)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `sfl`);
    assert(result.arguments[1] == `"^%decl\\s"`);

    result = extractFunctionArguments(FIND_FUNCTION_STR, 2, 0,
        25, `impl = $$member($$list($$find(sfl, "^%impl\\s")), 1)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `sfl`);
    assert(result.arguments[1] == `"^%impl\\s"`);

    result = extractFunctionArguments(FIND_FUNCTION_STR, 2, 0,
        19, `$$csources_var = $$find($$sources_var, ".*\\.c$")`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `$$sources_var`);
    assert(result.arguments[1] == `".*\\.c$"`);

    // Multi-line test snippets
    // TODO:

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 7) re_escape

    // Single-line test snippets
    result = extractFunctionArguments(REESCAPE_FUNCTION_STR, 1, 0,
        60, `!isEmpty(QMAKE_LINK_SHLIB_CMD):QMAKE_LINK_SHLIB_CMD ~= s/^$$re_escape($$QMAKE_LINK_SHLIB)$/$$QMAKE_LINK_C_SHLIB/`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `$$QMAKE_LINK_SHLIB`);

    // Multi-line test snippets
    // TODO:

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 8) replace

    // Single-line test snippets
    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        68, `ANDROID_CXX_STL_LIBS = $$ANDROID_SOURCES_CXX_STL_LIBDIR/libc++.so.$$replace(ANDROID_PLATFORM, "android-", "")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `ANDROID_PLATFORM`);
    assert(result.arguments[1] == `"android-"`);
    assert(result.arguments[2] == `""`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        39, `defineReplace(emitString): return("\"$$replace(1, \\\\, \\\\)\"")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `1`);
    assert(result.arguments[1] == `\\\\`);
    assert(result.arguments[2] == `\\\\`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        22, `cmake_module_name = $$replace(_name, ^Qt, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `_name`);
    assert(result.arguments[1] == `^Qt`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         9, `return($$replace(path, ([^/])$, \\1/))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `path`);
    assert(result.arguments[1] == `([^/])$`);
    assert(result.arguments[2] == `\\1/`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        14, `BUILD_DIR = $$replace($$list($$OUT_PWD/build), /, $$QMAKE_DIR_SEP)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `$$list($$OUT_PWD/build)`);
    assert(result.arguments[1] == `/`);
    assert(result.arguments[2] == `$$QMAKE_DIR_SEP`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `grp = $$replace(group, _?dbus_$${dbus_type}\$, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `group`);
    assert(result.arguments[1] == `_?dbus_$${dbus_type}\$`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        28, `QT_COMPILER_STDCXX_no_L = $$replace(QT_COMPILER_STDCXX, "L$", "")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT_COMPILER_STDCXX`);
    assert(result.arguments[1] == `"L$"`);
    assert(result.arguments[2] == `""`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         0, `QMAKE_MAC_SDK_MAJOR_MINOR_VERSION = $$replace(QMAKE_MAC_SDK_VERSION, "(\\d+)(\\.\\d+)(\\.\\d+)?", \\1\\2)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_MAC_SDK_VERSION`);
    assert(result.arguments[1] == `"(\\d+)(\\.\\d+)(\\.\\d+)?"`);
    assert(result.arguments[2] == `\\1\\2`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        39, `QM_FILES += $$OUT_PWD/$$LRELEASE_DIR/$$replace(translation, \\..*$, .qm)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `translation`);
    assert(result.arguments[1] == `\\..*$`);
    assert(result.arguments[2] == `.qm`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        28, `VCLIBS = Microsoft.VCLibs.$$replace(MSVC_VER, \\., ).00`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `MSVC_VER`);
    assert(result.arguments[1] == `\\.`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        39, `contains(MSVC_VER, "15.0"): VCLIBS = $$replace(VCLIBS, 150, 140)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `VCLIBS`);
    assert(result.arguments[1] == `150`);
    assert(result.arguments[2] == `140`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        15, `base = qlalr_$$replace(sf, ^.*/([^./]+)[^/]*$, \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `sf`);
    assert(result.arguments[1] == `^.*/([^./]+)[^/]*$`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        15, `nu = $$upper($$replace(name, -, _))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        14, `URITARGET = $$replace(URI, "\\.", "_")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `URI`);
    assert(result.arguments[1] == `"\\."`);
    assert(result.arguments[2] == `"_"`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        26, `error("Library '$$lower($$replace(nu, _, -))' is not defined.")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `nu`);
    assert(result.arguments[1] == `_`);
    assert(result.arguments[2] == `-`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `URI = $$replace(TARGETPATH, "/", ".")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGETPATH`);
    assert(result.arguments[1] == `"/"`);
    assert(result.arguments[2] == `"."`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        19, `TARGETPATHBASE = $$replace(TARGETPATH, \\.\\d+\$, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGETPATH`);
    assert(result.arguments[1] == `\\.\\d+\$`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        22, `qmltypes.commands = $$replace(TARGETPATHBASE, /, .)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGETPATHBASE`);
    assert(result.arguments[1] == `/`);
    assert(result.arguments[2] == `.`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        13, `CLEAN_QT = $$replace(QT, -private$, _private)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT`);
    assert(result.arguments[1] == `-private$`);
    assert(result.arguments[2] == `_private`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        11, `nptype = $$replace(ptype, [-/], _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `ptype`);
    assert(result.arguments[1] == `[-/]`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         9, `$$1 -= $$replace(flag, ^-, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `flag`);
    assert(result.arguments[1] == `^-`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        43, `error("Unknown module(s) in QT$$var_sfx: $$replace(BAD_QT, _private$, -private)")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `BAD_QT`);
    assert(result.arguments[1] == `_private$`);
    assert(result.arguments[2] == `-private`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         9, `name = $$replace(name,/,_)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `/`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         9, `name = $$replace(name, \\.qrc$, _qmlcache.qrc)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `\\.qrc$`);
    assert(result.arguments[2] == `_qmlcache.qrc`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         9, `name = $$replace(name,\.\.,)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `\.\.`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         0, `name = $$replace(name,-,_)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         9, `deps = $$replace(QT, -private$, _private)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT`);
    assert(result.arguments[1] == `-private$`);
    assert(result.arguments[2] == `_private`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        23, `QMAKE_MODULE_PATH += $$replace(dirs, \$, /modules)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `dirs`);
    assert(result.arguments[1] == `\$`);
    assert(result.arguments[2] == `/modules`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `opt = $$replace(c, "^([A-Z0-9_]+)=(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^([A-Z0-9_]+)=(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `opt = $$replace(c, "^--?enable-(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^--?enable-(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `opt = $$replace(c, "^--?(disable|no)-(.*)", "\\2")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^--?(disable|no)-(.*)"`);
    assert(result.arguments[2] == `"\\2"`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `opt = $$replace(c, "^--([^=]+)=(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^--([^=]+)=(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `opt = $$replace(c, "^-(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^-(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `val = $$replace(opt, "(qt|system)-(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `opt`);
    assert(result.arguments[1] == `"(qt|system)-(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `val = $$replace(c, $$e, "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `$$e`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        20, `$${lpfx}.export = $$replace(l, -, _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `l`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        26, `isEmpty(alias): alias = $$replace(l, -, _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `l`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         7, `lp = $$replace(l, "^-L", )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `l`);
    assert(result.arguments[1] == `"^-L"`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        31, `USES = $$eval($$list($$upper($$replace(QMAKE_USE, -, _))))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_USE`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        17, `NAME = $$upper($$replace($${1}.library, -, _))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `$${1}.library`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        21, `depends += $$upper($$replace(use_lib, -, _))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `use_lib`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        11, `result = $$replace(e, "^'(.*)'$", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `e`);
    assert(result.arguments[1] == `"^'(.*)'$"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `var = $$replace(e, "^config\\.", "")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `e`);
    assert(result.arguments[1] == `"^config\\."`);
    assert(result.arguments[2] == `""`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        48, `rhs = $$qtConfEvaluateSingleExpression($${1}, $$replace(e, ".*==", ""))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `e`);
    assert(result.arguments[1] == `".*=="`);
    assert(result.arguments[2] == `""`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        12, `feature = $$replace(name, [-+.], _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `[-+.]`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         8, `dep = $$replace($${currentConfig}.depends.$$d, -private$, _private)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `$${currentConfig}.depends.$$d`);
    assert(result.arguments[1] == `-private$`);
    assert(result.arguments[2] == `_private`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        17, `qtmver.value = $$replace(qtver.value, ^(\\d+\\.\\d+).*$, \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `qtver.value`);
    assert(result.arguments[1] == `^(\\d+\\.\\d+).*$`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        19, `qtvertag.value = $$replace(qtver.value, \\.,)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `qtver.value`);
    assert(result.arguments[1] == `\\.`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        22, `QMAKE_DOCS_TARGET = $$replace(QMAKE_DOCS, ^(.*/)?(.*)\\.qdocconf$, \\2)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_DOCS`);
    assert(result.arguments[1] == `^(.*/)?(.*)\\.qdocconf$`);
    assert(result.arguments[2] == `\\2`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        9, `deps = $$replace(QT, -private$, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT`);
    assert(result.arguments[1] == `-private$`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        10, `resrc = $$replace(rline, ^[ \\t]*<file[^>]*>([^<]+)</file>[ \\t]*$, \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `rline`);
    assert(result.arguments[1] == `^[ \\t]*<file[^>]*>([^<]+)</file>[ \\t]*$`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        10, `resrc = $$replace(rline, "^\\d+\\s+ICON\\s+[^\"]*\"([^\"]+)\"\$", \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `rline`);
    assert(result.arguments[1] == `"^\\d+\\s+ICON\\s+[^\"]*\"([^\"]+)\"\$"`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         2, `$$replace(_PRO_FILE_, \\.pro$, .qmlproject)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `_PRO_FILE_`);
    assert(result.arguments[1] == `\\.pro$`);
    assert(result.arguments[2] == `.qmlproject`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        11, `MODULE = $$replace(TARGET, ^qt, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGET`);
    assert(result.arguments[1] == `^qt`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        25, `QMAKE_PKGCONFIG_FILE = $$replace(TARGET, ^Qt, Qt$$QT_MAJOR_VERSION)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGET`);
    assert(result.arguments[1] == `^Qt`);
    assert(result.arguments[2] == `Qt$$QT_MAJOR_VERSION`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        30, `QMAKE_PKGCONFIG_REQUIRES += $$replace(QT.$${i}.name, ^Qt, Qt$$section(QT.$${i}.VERSION, ., 0, 0))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT.$${i}.name`);
    assert(result.arguments[1] == `^Qt`);
    assert(result.arguments[2] == `Qt$$section(QT.$${i}.VERSION, ., 0, 0)`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        12, `fwd_hdr = $$replace(ofwd_hdr, ^\\^, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `ofwd_hdr`);
    assert(result.arguments[1] == `^\\^`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         0, `resource_name = $$replace(resource_name, [^a-zA-Z0-9_], _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `resource_name`);
    assert(result.arguments[1] == `[^a-zA-Z0-9_]`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        21, `BUNDLEIDENTIFIER = $$replace(QMAKE_FRAMEWORK_BUNDLE_NAME, \\.framework$, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_FRAMEWORK_BUNDLE_NAME`);
    assert(result.arguments[1] == `\\.framework$`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         9, `file = $$replace(file, ^(\\.\\./)+, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `file`);
    assert(result.arguments[1] == `^(\\.\\./)+`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         6, `m = $$replace(out, ".*\\$\\(EXPORT_([^)]+)\\).*", \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `out`);
    assert(result.arguments[1] == `".*\\$\\(EXPORT_([^)]+)\\).*"`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         2, `$$replace(path, ^=, $$[SYSROOT])`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `path`);
    assert(result.arguments[1] == `^=`);
    assert(result.arguments[2] == `$$[SYSROOT]`);

    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
         6, `m = $$replace(out, ".*\\$\\(EXPORT_([^)]+)\\).*", \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `out`);
    assert(result.arguments[1] == `".*\\$\\(EXPORT_([^)]+)\\).*"`);
    assert(result.arguments[2] == `\\1`);

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= `qmake_args += \`;
    strLinesArray ~= `    "QMAKE_DEPENDS_$${NAME}_CC = $$upper($$replace(dep_uses, -, _))" \`;
    strLinesArray ~= `    "QMAKE_DEPENDS_$${NAME}_LD = $$upper($$replace(dep_uses, -, _))"`;
    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        58, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `dep_uses`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    strLinesArray = [];
    strLinesArray ~= `for (ent, $$qtConfScalarOrList($${test}.test.qmake)): \`;
    strLinesArray ~= `   contents += $$replace(ent, "@PWD@", $$pwd)`;
    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        68, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `ent`);
    assert(result.arguments[1] == `"@PWD@"`);
    assert(result.arguments[2] == `$$pwd`);
    
    strLinesArray = [];
    strLinesArray ~= `!isEmpty(QT.$$replace(1, -, _).name): \`;
    strLinesArray ~= `    return(true)`;
    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
       14, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `1`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    strLinesArray = [];
    strLinesArray ~= `!isEmpty(MODULE_PLUGIN_TYPES): \`;
    strLinesArray ~= `    module_plugtypes = "QT.$${MODULE_ID}.plugin_types = $$replace(MODULE_PLUGIN_TYPES, /[^.]+\\.[^.]+$, )"`;
    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
        85, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `MODULE_PLUGIN_TYPES`);
    assert(result.arguments[1] == `/[^.]+\\.[^.]+$`);
    assert(result.arguments[2] == ``);

    strLinesArray = [];
    strLinesArray ~= `QMAKE_CFLAGS_MSVC_COMPAT = $$replace(QMAKE_MSC_FULL_VER, "(..)(..)(.*)", \`;
    strLinesArray ~= `                                     "-fms-compatibility-version=\\1.\\2.\\3")`;
    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
       29, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_MSC_FULL_VER`);
    assert(result.arguments[1] == `"(..)(..)(.*)"`);
    assert(result.arguments[2] == `"-fms-compatibility-version=\\1.\\2.\\3"`);

    strLinesArray = [];
    strLinesArray ~= `!equals(WINSDK_VER, $$replace(winsdk_ver, ^(\\d+\\.\\d+).*$, \\1)): \`;
    strLinesArray ~= `    winsdk_ver =`;
    result = extractFunctionArguments(REPLACE_FUNCTION_STR, 3, 0,
       22, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `winsdk_ver`);
    assert(result.arguments[1] == `^(\\d+\\.\\d+).*$`);
    assert(result.arguments[2] == `\\1`);

    writeln("All preprocessor tests were successfully finished!");
}
