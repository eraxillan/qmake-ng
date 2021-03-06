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

module source.utils.text_utils;

import std.experimental.logger;

static import std.regex;

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
import source.qmakeexception;
// -------------------------------------------------------------------------------------------------
public:

struct TextSearchResult
{
    long indexOpen = INVALID_INDEX;
    long indexClose = INVALID_INDEX;
    bool success = false;

    this(const long indexOpen, const long indexClose, const bool success)
    {
        this.indexOpen = indexOpen;
        this.indexClose = indexClose;
        this.success = success;
    }

    @safe bool isValid() const nothrow
    {
        if (!success)
            return (indexOpen == INVALID_INDEX) && (indexClose == INVALID_INDEX);
        
        return (indexOpen >=0) && (indexClose > indexOpen);
    }

    @safe bool opCast(T:bool)() const nothrow { return success; }
}

struct FunctionBaseInfo
{
    string functionName;
    long requiredArgumentCount;
    long optionalArgumentCount;

    FunctionBaseInfo dup() const @property
    {
        return FunctionBaseInfo(functionName.dup, requiredArgumentCount, optionalArgumentCount);
    }
}

class QStack(DataType)
{
private:
    DataType[] m_array;
    alias m_array this;

public:
    void push(DataType item)
    {
        m_array ~= item;
    }
    
    void push(DataType[] item)
    {
        m_array ~= item;
    }

    DataType pop()
    {
        if (!m_array.empty)
        {
            DataType top = m_array.back;
            m_array.popBack();
            return top;
        }
        
        throw new CollectionException("trying to pop item from empty stack");
    }

    ref DataType top()
    {
        if (!m_array.empty)
            return m_array[m_array.length - 1];

        throw new CollectionException("trying to get top item value from empty stack");
    }
}

// -------------------------------------------------------------------------------------------------

@property string left(const string str, const long count)
{
    return str[0 .. count];
}

@property string right(const string str, const long count)
{
    return str[$ - count .. $];
}

bool isWhitespaceToken(const string str)
{
    return !std.regex.matchFirst(str, r"\s+").empty;
}

bool isUnderscore(const char ch)
{
    return (ch == CHAR_UNDERSCORE);
}

bool isDot(const char ch)
{
    return (ch == CHAR_DOT);
}

bool isAlphascore(const char ch)
{
    return (isAlpha(ch) || isUnderscore(ch));
}

bool containsQuote(const string str)
{
    foreach (ch; str)
    {
        if ((ch == CHAR_SINGLE_QUOTE) || (ch == CHAR_DOUBLE_QUOTE))
            return true;
    }
    return false;
}

string joinTokens(const string str, const long index, const long count)
{
    string result = "";
    for (long j = index; j < min(index + count, str.length); j++)
        result ~= str[j];
    return result;
}

string[] splitString(const string str, const string delim, const bool skipEmptyParts)
{
    string[] temp = str.split(delim);

    if (!skipEmptyParts)
        return temp;

    string[] temp_2;
    foreach (value; temp)
    {
        if (!value.empty)
            temp_2 ~= value;
    }
    return temp_2;
}

string sectionString(const string str, const string delim, const long start, const long end = INVALID_INDEX, const bool skipEmptyParts = false)
{
    string[] temp = splitString(str, delim, skipEmptyParts);

    // Naive optimization: no or just one section items
    if (temp.length <= 1)
        return str;    
    if (start == end)
    {
        immutable auto startNew = (start >= 0) ? start : (temp.length - (-start));
        return temp[startNew];
    }

    long startNew = start;
    if (start < 0)
        startNew = temp.length - (-startNew);
    long endNew = end;
    if ((start >= 0) && (end == INVALID_INDEX))
        endNew = temp.length - 1;
    else if (end < 0)
        endNew = temp.length - (-endNew);

    return temp[startNew .. endNew + 1].join(delim);
}

bool isNumeric(const string n, const int base)
{
    try
    {
        /*auto a =*/ to!uint(n, base);
        return true;
    }
    catch (std.conv.ConvException exc)
    {
        return false;
    }
}

string wildcardToRegex(const string pattern)
{
    string result;
    result ~= "^";

    string escapedPattern = to!string(std.regex.escaper(pattern).array);
    result ~= escapedPattern.replace(`\*`, `.*`).replace(`\?`, `.`);

    result ~= "$";
    return result;
}

// Minimal function call statement contains at least three characters: f()
private const auto MINIMAL_FUNCTION_CALL_LENGTH = 3;

TextSearchResult findFunctionCall(const string sourceStr, const string functionName, const long fromIndex)
in
{
    assert(!functionName.empty);
    assert(fromIndex >= 0 && fromIndex <= sourceStr.length - MINIMAL_FUNCTION_CALL_LENGTH);
}
out (result)
{
    assert(result.isValid());
}
do
{
    if (sourceStr.length < MINIMAL_FUNCTION_CALL_LENGTH)
        return TextSearchResult();

    //immutable auto functionCallIndex_2 = sourceStr.indexOf(functionName ~ STR_OPENING_PARENTHESIS, fromIndex);
    auto result = std.regex.matchFirst(sourceStr[fromIndex .. $], functionName ~ `\s*\(`);
    if (!result)
        return TextSearchResult();

    return TextSearchResult(result.pre.length + fromIndex, result.pre.length + fromIndex + result.hit.length - 1, true);
}

unittest
{
    // left
    assert("pineapple".left(4) == "pine");
    // right
    assert("pineapple".right(5) == "apple");
    // FIXME: isWhitespaceToken
    // FIXME: joinTokens

    // FIXME: splitString

    // sectionString
    void testSplit()
    {
        immutable string csv = "forename,middlename,surname,phone";
          immutable string path = "/usr/local/bin/myapp";

        assert(sectionString(csv, ",", 2, 2) == "surname");
        assert(sectionString(path, "/", 3, 4) == "bin/myapp");
        assert(sectionString(path, "/", 3, 3, true) == "myapp");
        assert(sectionString(csv, ",", -3, -2) == "middlename,surname");
        assert(sectionString(path, "/", -1) == "myapp");
    }
    testSplit();
    
    // FIXME: isNumeric
    // FIXME: isHexNumeric
    // FIXME: wildcardToRegex
    
    //assert(false);
}
