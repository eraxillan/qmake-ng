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
module source.type_deduction;

import std.experimental.logger;

import std.typecons;
import std.uni;
import std.conv;
import std.stdio;
import std.file;
import std.process;
import std.getopt;
import std.path;
import std.string;
import std.range;
import std.algorithm;
import std.container.rbtree;

import source.common_const;
import source.common_utils;
import source.project_variable;
import source.persistent_property;
import source.project_function;
import source.project_context;
import source.qmakeexception;


public:
alias RvalueEvalResult = Tuple!(VariableType, "type", string[], "value");

/+
enum VariableType
{
    UNKNOWN = -1,
    VOID = 0,
    BOOLEAN = 1,
    RAW_STRING,               // string with any characters, e.g. argument of message() test function: message(I am a raw string)
    NUMBER,                   // integer number, e.g. qmake debug level: debug(1, This text will be shown under debug level 1)
    STRING,                   // string without whitespaces/commas, e.g. variable name
    STRING_LIST,    	  	  // array of strings without whitespaces/commas
    RESTRICTED_STRING,        // string without whitespaces/commas with value from the specified array, e.g. TEMPLATE = app|...|lib
    RESTRICTED_STRING_LIST,   // array of such strings
    OBJECT,                   // object with properties (e.g. host.arch)
    OBJECT_LIST,              // list of objects with properties described above
	COUNT
}
+/

VariableType deduceRvalueType(const RvalueEvalResult[] rvalueCollection)
{
    // Corner case
    if (rvalueCollection.empty)
    {
        trace("Result as rvalue collection: <empty>");
        return VariableType.UNKNOWN;
    }

    trace("Result as rvalue collection: ", rvalueCollection);

    // Collect unique data types from rvalue collection
    version(unittest)
    {
        throw new NotImplementedError("RedBlackTree must be replaced by smth that compiles in unit-test mode!");
    }
    else
    {
        auto rvalueDataTypes = redBlackTree!VariableType();
        for (int i = 0; i < rvalueCollection.length; i++)
        {
            rvalueDataTypes.insert(rvalueCollection[i].type);
        }

        // Check whether we need conversion
        immutable bool needConversion = uniq(rvalueDataTypes[]).walkLength > 1;
        // FIXME: implement implicit conversion
        assert((!needConversion));

        VariableType result = rvalueCollection[0].type;
        trace("Final result type: ", result);
        return result;
    }
}

string[] prettifyRvalue(const RvalueEvalResult[] rvalueCollection, const VariableType dataType)
{
    string[] result;

    switch (dataType)
    {
        case VariableType.STRING:
        {
            result = [""];
            for (int i = 0; i < rvalueCollection.length; i++)
            {
                if (!rvalueCollection[i].value.empty)
                    result[0] ~= rvalueCollection[i].value[0];
            }
            trace("Result final value: ", "`", result[0], "`");
            break;
        }
        case VariableType.STRING_LIST:
        case VariableType.RESTRICTED_STRING_LIST:
        {
            for (int i = 0; i < rvalueCollection.length; i++)
            {
                result ~= rvalueCollection[i].value;
            }
            trace("Result is already list, no conversion required");
            break;
        }
        default:
        {
            throw new NotImplementedException("Invalid variable type");
        }
    }

    return result;
}
