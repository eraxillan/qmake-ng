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

import std.typecons : No;

import std.algorithm;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;

import preprocessor;
import qmakeparser;

bool tryParseProject(string fileName)
{
    auto proFileContents = std.file.readText(fileName);
    proFileContents = preprocessLines(splitLines(proFileContents));
        
    auto parseTree = QMakeProject(proFileContents);
    if (!parseTree.successful)
    {
        writeln("Parsing failed:");
        writeln(parseTree);
    }
    return parseTree.successful;
}

unittest
{
    assert(!isInsideQuotes("abc \"123\" a:b xyz \"567\"", ":", 11));
    assert(isInsideQuotes("abc \"123\" \"x:y\" xyz \"567\"", ":", 12));

    assert(tryParseProject("tests/linux-clang-qmake.conf"));
}

void main()
{
    version (tracer)
    {
        traceAll();
        //setTraceConditionFunction(function(string ruleName, const ref ParseTree p) {return ruleName.startsWith("QMakeProject.");});
    }
    
    if (!tryParseProject("tests/contains.pro"))
    {
        writeln("Test tests/contains.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/contains.pro passed\n\n");
    
    if (!tryParseProject("tests/eval.pro"))
    {
        writeln("Test tests/eval.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/eval.pro passed\n\n");
    
    if (!tryParseProject("/home/eraxillan/Qt/5.10.0/gcc_64/mkspecs/features/qt_module_headers.prf"))
    {
        writeln("Test features/qt_module_headers.prf FAILED\n\n");
        return;
    }
    writeln("Test qt_module_headers.prf passed\n\n");
    
    if (!tryParseProject("/home/eraxillan/Qt/5.10.0/gcc_64/mkspecs/features/qt_functions.prf"))
    {
        writeln("Test features/qt_functions.prf FAILED\n\n");
        return;
    }
    writeln("Test qt_functions.prf passed\n\n");
    
    if (!tryParseProject("/home/eraxillan/Qt/5.10.0/gcc_64/mkspecs/features/uikit/qt.prf"))
    {
        writeln("Test features/uikit/qt.prf FAILED\n\n");
        return;
    }
    writeln("Test uikit/qt.prf passed\n\n");
    

    if (!tryParseProject("tests/test_function_call_1.pro"))
    {
        writeln("Test tests/test_function_call_1.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/test_function_call_1.pro passed\n\n");
    
    if (!tryParseProject("tests/block_1.pro"))
    {
        writeln("Test tests/block_1.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/block_1.pro passed\n\n");
    if (!tryParseProject("tests/block_2.pro"))
    {
        writeln("Test tests/block_2.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/block_2.pro passed\n\n");
    if (!tryParseProject("tests/block_3.pro"))
    {
        writeln("Test tests/block_3.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/block_3.pro passed\n\n");
    if (!tryParseProject("tests/block_4.pro"))
    {
        writeln("Test tests/block_4.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/block_4.pro passed\n\n");
    
    if (!tryParseProject("tests/scope_1.pro"))
    {
        writeln("Test tests/scope_1.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/scope_1.pro passed\n\n");
    if (!tryParseProject("tests/scope_2.pro"))
    {
        writeln("Test tests/scope_2.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/scope_2.pro passed\n\n");
    if (!tryParseProject("tests/scope_3.pro"))
    {
        writeln("Test tests/scope_3.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/scope_3.pro passed\n\n");
    if (!tryParseProject("tests/scope_4.pro"))
    {
        writeln("Test tests/scope_4.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/scope_4.pro passed\n\n");
    if (!tryParseProject("tests/scope_5.pro"))
    {
        writeln("Test tests/scope_5.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/scope_5.pro passed\n\n");
    if (!tryParseProject("tests/scope_6.pro"))
    {
        writeln("Test tests/scope_6.pro FAILED\n\n");
        return;
    }
    writeln("Test tests/scope_6.pro passed\n\n");

    if (!tryParseProject("/home/eraxillan/Qt/5.10.0/gcc_64/mkspecs/features/default_post.prf"))
    {
        writeln("Test features/default_post.prf FAILED\n\n");
        return;
    }
    writeln("Test qt_parts.prf passed\n\n");
    if (!tryParseProject("tests/qml_module.prf"))
    {
        writeln("Test features/qml_module.prf FAILED\n\n");
        return;
    }
    writeln("Test tests/qml_module.prf passed\n\n");    
    if (!tryParseProject("tests/android-base-head.conf"))
    {
        writeln("Test tests/android-base-head.conf FAILED\n\n");
        return;
    }        
    writeln("Test tests/android-base-head.conf passed\n\n");

    // Runtime parsing
    // Iterate over all *.d files in current directory ("") and all its subdirectories
    auto projectFiles = dirEntries("/home/eraxillan/Qt/5.10.0/gcc_64/mkspecs", SpanMode.depth).filter!(
        f => f.name.endsWith(".pro") || f.name.endsWith(".pri") || f.name.endsWith(".prf") || f.name.endsWith(".conf")
    );
    int successfulCount = 0, failedCount = 0;
    foreach (d; projectFiles)
    {
        writeln("\n");
        writeln(d.name);

        if (tryParseProject(d.name))
        {
            successfulCount++;
            writeln("SUCCESS");
        // } else failedCount++;
        } else break;
    }
    
    int totalCount = successfulCount + failedCount;
    writeln("Total file count: " ~ std.conv.to!string(totalCount));
    writeln("Successfully parsed: " ~ std.conv.to!string(successfulCount));
    writeln("Failed to parse: " ~ std.conv.to!string(failedCount) ~ " or " ~ std.conv.to!string(100 * failedCount / totalCount) ~ "%");
}

