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

import std.experimental.logger;

import std.string;
import std.conv;
import std.algorithm;
import std.stdio;
import std.file;
import std.getopt;
import std.path;

import project;

int parseQtSourceProjects(string qtDir)
{
    auto projectFiles = dirEntries(qtDir, SpanMode.depth).filter!(
        f => f.name.endsWith(".pro") || f.name.endsWith(".pri") || f.name.endsWith(".prf") || f.name.endsWith(".conf")
    );
    
    int successfulCount, failedCount;
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

    immutable int totalCount = successfulCount + failedCount;
    info("Total file count: " ~ std.conv.to!string(totalCount));
    info("Successfully parsed: " ~ std.conv.to!string(successfulCount));
    info("Failed to parse: " ~ std.conv.to!string(failedCount) ~ " or "
        ~ std.conv.to!string(100 * failedCount / totalCount) ~ "%");

    return (failedCount == 0) ? 0 : 1;
}

int main(string[] args)
{
    auto helpInformation = getopt(
        args
    );

    if (helpInformation.helpWanted || (args.length < 2))
    {
        defaultGetoptPrinter("qmake-ng, Qt's qmake replacement.\n" ~
                             "Usage: " ~ args[0] ~ " [options] <project.pro>|<dir_path>\n" ~
                             "Options:",
            helpInformation.options);
        return 1;
    }

    if (!exists(args[1]))
    {
        writeln("File or directory '" ~ args[1] ~ "' not exists. Aborting");
        return 1;
    }

    version (tracer)
    {
        traceAll();
        //setTraceConditionFunction(function(string ruleName, const ref ParseTree p) {return ruleName.startsWith("QMakeProject.");});
    }

    // Runtime parsing

    // NOTE: parse Qt directory separately - we need to skip snippets
    // Qt path usually looks like this: /home/<user_name>/Qt/5.10.0/Src
    // FIXME: add option like "-qt"
    if (isDir(args[1]) && (args[1].indexOf("/Qt/") != -1) && (args[1].indexOf("/Src") != -1))
    {
        info("Qt directory recognized in the specified path");
        return parseQtSourceProjects(args[1]);
    }

    if (isFile(args[1]))
    {
        auto pro = new Project();
        if (pro.tryParse/*eval*/(args[1]))
        {
            info("qmake project file '" ~ args[1] ~ "' was successfully parsed");
            return 0;
        }

        error("failed to parse project file: '" ~ args[1] ~ "'");
        return 1;
    }
    else
    {
        auto projectFiles = dirEntries(args[1], SpanMode.depth).filter!(
            f => f.name.endsWith(".pro") || f.name.endsWith(".pri")
              || f.name.endsWith(".prf") || f.name.endsWith(".conf")
        );
        int successfulCount, failedCount;
        foreach (d; projectFiles)
        {       
            trace(d.name);

    		auto pro = new Project();
            if (pro.tryParse(d.name))
            {
                successfulCount++;
                info("All qmake project files found were successfully parsed");
            } else failedCount++;
            // } else break;
        }

        immutable int totalCount = successfulCount + failedCount;
        info("Total file count: " ~ std.conv.to!string(totalCount));
        info("Successfully parsed: " ~ std.conv.to!string(successfulCount));
        info("Failed to parse: " ~ std.conv.to!string(failedCount) ~ " or "
            ~ std.conv.to!string(100 * failedCount / totalCount) ~ "%");
        return (failedCount == 0) ? 0 : 1;
    }
}
