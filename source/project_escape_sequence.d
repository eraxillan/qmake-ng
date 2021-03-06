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

module source.project_escape_sequence;

import std.experimental.logger;

import std.uni;
import std.utf;
import std.algorithm;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;
import std.range;

import source.common_const;
import source.utils.text_utils;
import source.qmakeexception;

// -------------------------------------------------------------------------------------------------

public class EscapeSequence
{
private:
	alias ReplacementAction = string function(const string str, const int from, const int to);
	immutable ReplacementAction[string] m_convertMap;

public:
    this()
	{
		import std.exception : assumeUnique;

		ReplacementAction[string] convertMap; // mutable buffer
        convertMap["\\\\"] = (const string str, const int from, const int length) {
            trace("'\\' escape sequence");
            return "\\";
        };
        convertMap["\\\""] = (const string str, const int from, const int length) {
            trace("'\" escape sequence");
            return "\"";
        };
        convertMap["\\\'"] = (const string str, const int from, const int length) {
            trace("'\'' escape sequence");
            return "\'";
        };
        convertMap["\\x"] = (const string str, const int from, const int length) {
            trace("'\\xABCD' or '\\xAB' escape sequence");
            string result = "";
            string charCodeStr = joinTokens(str, from + 2, length);
			try {
				int charCode = to!int(charCodeStr, 16);
				// FIXME: implement
				char[4] buf;
				encode(buf, cast(dchar)charCode);
				result ~= buf;
//                result = string.fromCharCode(charCode);
            } catch (ConvException exc) {
                throw new EscapeSequenceException("invalid hexadecimal character code '" ~ charCodeStr ~ "'");
            }
            return result;
        };
        convertMap["\\?"] = (const string str, const int from, const int length) {
            trace("'\?' escape sequence");
            return "\?";
        };
        convertMap["\\a"] = (const string str, const int from, const int length) {
            trace("'\\a' escape sequence");
            return "\a";
        };
        convertMap["\\b"] = (const string str, const int from, const int length) {
            trace("'\\b' escape sequence");
            return "\b";
        };
        convertMap["\\f"] = (const string str, const int from, const int length) {
            trace("'\\f' escape sequence");
            return "\f";
        };
        convertMap["\\n"] = (const string str, const int from, const int length) {
            trace("'\\n' escape sequence");
            return "\n";
        };
        convertMap["\\r"] = (const string str, const int from, const int length) {
            trace("'\\r' escape sequence");
            return "\r";
        };
        convertMap["\\t"] = (const string str, const int from, const int length) {
            trace("'\\t' escape sequence");
            return "\t";
        };
        convertMap["\\v"] = (const string str, const int from, const int length) {
            trace("'\\v' escape sequence");
            return "\v";
        };

        convertMap["\\$"] = (const string str, const int from, const int length) {
            trace("'\\$' escape sequence");
            return "$";
        };

        // Add octal character codes: from 0 to 777
		// FIXME: implement
		/+
        for (int i = 0; i < 777; i++)
		{
            convertMap["\\" ~ to!string(i).leftJustify(3, '0')] = (const string str, const int from, const int length) {
                auto charCode_8_3 = parse!int(to!string(i), 8);
                return String.fromCharCode(charCode_8_3);
            };
        }
		+/
		
		convertMap.rehash; // for faster lookups
		m_convertMap = assumeUnique(convertMap);
    }

	struct Result
	{
		bool result = false;
		int length = 0;

		this(const bool _result, const int _length)
		{
			result = _result;
			length = _length;
		}
	}
	
    Result isEscapeSequence(const string str, const int indexFrom)
	{
        // NOTE: any escape sequence must start with exactly one backslash
        if ("" ~ str[indexFrom] != STR_BACKSLASH)
            return Result(false, 0);

        // Escape sequence could contain two, three or four characters
        auto twoTokens = joinTokens(str, indexFrom, 2);     // \r
        auto fourTokens = joinTokens(str, indexFrom, 4);    // \123, \xAB

        if (twoTokens == "\\x")
		{
            // \x can be followed by one, two, three or four hex digits
            auto digitCount = 0;
            auto index = indexFrom + 1;
            do
			{
                index++;
                if (!isNumeric("" ~ str[index], 16))
                    break;

                digitCount++;
            } while (true);

            if (digitCount == 0)
                throw new EscapeSequenceException("invalid sequence \\x: must be followed with hexadecimal character code");

            return Result(true, digitCount + 2);
        } else if (fourTokens in m_convertMap)
            return Result(true, 4);
        else if (twoTokens in m_convertMap)
            return Result(true, 2);

        return Result(false, 0);
    }

    string getEscapeSequence(const string str, const int indexFrom, const int length)
	{
		auto key = str[indexFrom .. indexFrom + length];
        assert(key.length == 2 || key.length == 4);

        if (key in m_convertMap)
        {
            return m_convertMap[key](str, indexFrom, length);
        }
        else
        {
            throw new EscapeSequenceException("invalid sequence '" ~ key ~ "'");
        }
    }
}
