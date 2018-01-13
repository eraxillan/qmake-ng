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

import std.experimental.logger;

import std.string;
import std.conv;
import std.algorithm;
import std.stdio;
import std.file;
import std.getopt;
import std.path;

import project;

void main()
{
    version (tracer)
    {
        traceAll();
        //setTraceConditionFunction(function(string ruleName, const ref ParseTree p) {return ruleName.startsWith("QMakeProject.");});
    }

    // Runtime parsing
    // Iterate over all *.d files in current directory ("") and all its subdirectories
//    auto projectFiles = dirEntries("/home/eraxillan/Qt/5.10.0/android_armv7/mkspecs", SpanMode.depth).filter!(
//    auto projectFiles = dirEntries("/home/eraxillan/Qt/5.10.0/android_x86/mkspecs", SpanMode.depth).filter!(
//    auto projectFiles = dirEntries("/home/eraxillan/Qt/5.10.0/gcc_64/mkspecs", SpanMode.depth).filter!(
    auto projectFiles = dirEntries("/home/eraxillan/Qt/5.10.0/Src", SpanMode.depth).filter!(
        f => f.name.endsWith(".pro") || f.name.endsWith(".pri") || f.name.endsWith(".prf") || f.name.endsWith(".conf")
    );
    int successfulCount = 0, failedCount = 0;
    foreach (d; projectFiles)
    {
        if (d.name.indexOf("qtbase/qmake/doc/snippets/") != -1
         || d.name.indexOf("qtdoc/doc/src/snippets/") != -1
         || d.name.indexOf("qtdoc/doc/snippets/") != -1
         || d.name.indexOf("activeqt/doc/snippets/") != -1
         || d.name.indexOf("qtxmlpatterns/src/xmlpatterns/doc/snippets/") != -1
         || d.name.indexOf("qtscript/src/script/doc/snippets/") != -1
         || d.name.indexOf("qtscript/src/scripttools/doc/snippets/") != -1
         || d.name.indexOf("qttools/src/designer/src/designer/doc/snippets/") != -1
         || d.name.indexOf("qttools/src/designer/src/uitools/doc/snippets/") != -1
         || d.name.indexOf("qttools/src/linguist/linguist/doc/snippets/") != -1
         || d.name.indexOf("qttools/examples/designer/doc/snippets/") != -1
         || d.name.indexOf("qtdatavis3d/src/datavisualization/doc/snippets/") != -1
         || d.name.indexOf("qtsvg/src/svg/doc/snippets/") != -1
         
         || d.name.indexOf("qtquickcontrols2.conf") != -1
         || d.name.indexOf("Sensors.conf") != -1
         || d.name.indexOf("3rdparty/chromium") != -1
         || d.name.indexOf("shared/deviceskin/") != -1
        )
        {
            trace("Skipping documentation snippet...");
            continue;
        }
        
        trace(d.name);

		auto pro = new Project();
        if (pro.tryParse(d.name))
        {
            successfulCount++;
            info("All qmake project files found were successfully parsed");
        } else failedCount++;
       // } else break;
    }
    
    int totalCount = successfulCount + failedCount;
    info("Total file count: " ~ std.conv.to!string(totalCount));
    info("Successfully parsed: " ~ std.conv.to!string(successfulCount));
    info("Failed to parse: " ~ std.conv.to!string(failedCount) ~ " or " ~ std.conv.to!string(100 * failedCount / totalCount) ~ "%");
}
