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

module common_utils;

import std.experimental.logger;

import std.uni;
import std.regex;
import std.algorithm;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;
import std.range;
import common_const;
// -------------------------------------------------------------------------------------------------

// FIXME: use alias this to eliminate dublicated with std.array properties
public class QStack(DataType)
{
	DataType[] m_array;

    public void push(DataType item)
	{
		m_array ~= item;
	}
	
	public void push(DataType[] item)
	{
		m_array ~= item;
	}

    public DataType pop()
	{
		if (!m_array.empty)
		{
			DataType top = m_array.back;
			m_array.popBack();
			return top;
		}
		
		throw new Exception("trying to pop item from empty stack");
	}

    public ref DataType top()
	{
		if (!m_array.empty)
			return m_array[m_array.length - 1];

//		static DataType defaultValue;
//		return defaultValue;
		throw new Exception("trying to get top item value from empty stack");
    }

    public void setTop(DataType value)
	{
		if (m_array.empty)
			throw new Exception("trying to set top item value from empty stack");
		
		m_array[m_array.length - 1] = value;
    }

    public bool isEmpty()
	{
        return m_array.empty;
    }
	
	public ulong length()
	{
		return m_array.length;
	}
	
	public DataType[] data()
	{
		return m_array;
	}
}

// -------------------------------------------------------------------------------------------------

public void setupDatetimeLocale()
{
	import std.process : environment;
	import core.stdc.locale: setlocale, LC_ALL, LC_TIME;

	auto value = environment.get("LC_TIME");
	if (value is null)
	{
		warning("LC_TIME is not defined: use en_US.UTF-8 by default");
		setlocale(LC_TIME, "en_US.UTF-8");
	}
	else
		setlocale(LC_TIME, value.ptr);
}

public auto getDateTimeString()
{
	import core.stdc.time : time, localtime, strftime;
	auto unixTime = time(null);
	auto tmVar = localtime(&unixTime);
	char[256] buffer;
    auto len = strftime(buffer.ptr, 80, toStringz("%a %b. %d %T %Y"), tmVar);
	auto prettyStr = buffer[0 .. len].idup;
	prettyStr = toLower(prettyStr);
	return prettyStr;
}

@property public string left(in string str, in long count)
{
	return str[0 .. count];
}

@property public string right(in string str, in long count)
{
	return str[$ - count .. $];
}

public bool isWhitespaceToken(in string str)
{
	// FIXME: use `isWhite` function
	//
	// FIXME: implement unit-test
	// if (/\s/.test(token)
	return !matchFirst(str, r"\s+").empty;
}

public bool isUnderscore(in char ch)
{
	return (ch == CHAR_UNDERSCORE);
}

public bool isDot(in char ch)
{
	return (ch == CHAR_DOT);
}

public bool isAlphascore(in char ch)
{
	return (isAlpha(ch) || isUnderscore(ch));
}

public bool containsQuote(in string str)
{
	foreach (ch; str)
	{
		if ((ch == CHAR_SINGLE_QUOTE) || (ch == CHAR_DOUBLE_QUOTE))
			return true;
	}
	return false;
}

public string joinTokens(in string str, in long index, in long count)
{
    string result = "";
    for (long j = index; j < min(index + count, str.length); j++)
        result ~= str[j];
    return result;
}

public string[] splitString(in string str, in string delim, in bool skipEmptyParts)
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

public string sectionString(in string str, in string delim, in int start, in int end = -1, in bool skipEmptyParts = false)
{
	string[] temp = splitString(str, delim, skipEmptyParts);

	// Naive optimization: no or just one section items
	if (temp.length <= 1)
		return str;	
	if (start == end)
	{
		immutable int startNew = (start >= 0) ? start : (cast(int)temp.length - (-start));
		return temp[startNew];
	}

	int startNew = start;
	if (start < 0)
		startNew = cast(int)temp.length - (-startNew);
	int endNew = end;
	if ((start >= 0) && (end == -1))
		endNew = cast(int)temp.length - 1;
	else if (end < 0)
		endNew = cast(int)temp.length - (-endNew);

	return temp[startNew .. endNew + 1].join(delim);
}

public bool isNumeric(in string n, in int base)
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

public string wildcardToRegex(in string pattern)
{
    /*return "^" + Regex.Escape(pattern)
                      .Replace(@"\*", ".*")
                      .Replace(@"\?", ".")
               + "$";*/
	string result;
	result ~= "^";

	string escapedPattern = to!string(pattern.escaper.array);
	result ~= escapedPattern.replace(`\*`, `.*`).replace(`\?`, `.`);

	result ~= "$";
	return result;
}

public long detectIndentSize(in string str)
{
	if (str.strip().empty)
		return 0;

	long result = 0;

	for (long i = 0; i < str.length; i++)
	{
		if (!isWhitespaceToken("" ~ str[i]))
			break;

		result++;
	}

	return result;
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
