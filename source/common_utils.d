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

// -------------------------------------------------------------------------------------------------

// FIXME: use alias this to eliminate dublicated with std.array properties
public class QStack(DataType)
{
	DataType[] m_array;

    public void push(in DataType item)
	{
		m_array ~= item;
	}
	
	public void push(in DataType[] item)
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

    public DataType top()
	{
		if (!m_array.empty)
			return m_array[m_array.length - 1];

		static DataType defaultValue;
		return defaultValue;
//		throw new Exception("trying to get top item value from empty stack");
    }

    public void setTop(in DataType value)
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

public auto getDateTimeString()
{
	import std.string : format, split;
	import std.datetime : DateTime, Clock;

	DateTime dateTime = cast(DateTime)Clock.currTime();
	with (dateTime)
	{
		return format(
			"%s " ~ // day of the week (eg. 'Saturday')
			"%s.%02s.%s " ~ // date, month, year
			"[%s:%02s:%02s%s]", // hour:minute:second am/pm
			split("Sunday Monday Tuesday Wednesday Thursday Friday Saturday")[dayOfWeek],
				day, cast(int)month, year,
				hour == 0 || hour == 12 ? 12 : hour % 12, minute, second, hour <= 11 ? "am" : "pm");
	}
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

public string joinTokens(in string str, in int index, in int count)
{
    string result = "";
    for (int j = index; j < min(index + count, str.length); j++)
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

/+
public bool isNumeric(in string n)
{
	try
	{
		/*auto a =*/ to!double(n);
		return true;
	}
	catch(std.conv.ConvException exc)
	{
		return false;
	}
}
+/

public bool isHexNumeric(in string n)
{
	try
	{
		/*auto a =*/ to!uint(n, 16);
		return true;
	}
	catch(std.conv.ConvException exc)
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
