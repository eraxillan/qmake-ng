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
static import std.file;
static import std.path;
static import std.getopt;
import qmakeexception;
import project;
import project_context;
import project_variable;
import common_const;
import common_utils;
import persistent_property;

public enum CmdLineFlags
{
    QMAKE_CMDLINE_SUCCESS       = 0x00,
    QMAKE_CMDLINE_SHOW_USAGE    = 0x01,
    QMAKE_CMDLINE_BAIL          = 0x02,
    QMAKE_CMDLINE_ERROR         = 0x04
}


/// Global qmake mode, can only be in one mode per invocation
enum QmakeMode { invalid = -1, nothing, project, makefile, prl }
enum QmakeProWarningLevel { invalid = -1, none = 0x00, parser = 0x01, logic = 0x02, deprecated_ = 0x04, all = 0xFF }
enum QmakePropertyAction { invalid = -1, set, unset, query }
enum QmakeAssignmentTime { invalid = 1, early, before, after, late }

struct QmakeOptions
{
    bool showVersion;
    string outputFile;
    uint logLevelNumber; // LogLevel.error

    string proTemplate;
    string proTemplatePrefix;

    QmakeProWarningLevel proWarningLevel = QmakeProWarningLevel.logic | QmakeProWarningLevel.deprecated_;
    QmakeMode mode = QmakeMode.nothing;
    QmakeAssignmentTime assignmentTime = QmakeAssignmentTime.invalid;
    
    bool recursive; // false by default
    QmakePropertyAction propertyAction = QmakePropertyAction.invalid;
    string qtConfPath;
    string cachePath;
    string specFileName;
    bool useCache;

    bool doDeps = true;
    bool doMoc = true;
    bool doStubMakefile;
    bool doDepHeuristics = true;
    bool doPreprocess /*= false*/;
    
    bool doPwd = true;    // true by default

    string[] projectFileNames;
    string[] assignmentStatements;
    string[] properties;
}

public int parseCommandlineOptions(in string[] argv, out QmakeOptions options)
{
     // Original Qt qmake compatibility:
    // dlang getopt do not support long options with single dash
    // so just replace one dash with two sebsuquent ones
    string[string] argReplaceMap = [
        // Global options
        "-v": "--version",
        "-version": "--version",
        "-help": "--help",
        "-o": "--outputfile",
        "-output": "--outputfile",
        "-d": "--verbose",
        // Template
        "-t": "--template",
        "-tp": "--templateprefix",
        // Project file parser warning level
        "-Wnone": "-prowarninglevel none",
        "-Wall": "--prowarninglevel all",
        "-Wparser": "-prowarninglevel parser",
        "-Wlogic": "-prowarninglevel logic",
        "-Wdeprecated": "-prowarninglevel deprecated_",
        // Mode
        "-makefile": "--mode makefile",
        "-project": "--mode project",
        // Variable assignment options
        "-early": "--assignment early",
        "-before": "--assignment before",
        "-after": "--assignment after",
        "-late": "--assignment late",
        // Recurivity
        "-nr": "--recursive false",
        "-norecursive": "--recursive false",
        "-r": "--recursive true",
        "-recursive": "--recursive true",
        // Persistent property
        "-set":   "--persistent-property set",
        "-unset": "--persistent-property unset",
        "-query": "--persistent-property query",
        // Qt configuration file
        "-qtconf": "--qtconf",
        // Makefile mode options
        "-cache": "--cachefile",
        "-spec": "--spec",
        "-nocache": "--cache false",
        "-nodepend": "--depend false",
        "-nodepends": "--depend false",
        "-nomoc": "--moc false",
        "-createstub": "--createstub",
        "-nodependheuristics": "--dependheuristics false",
        "-E": "--do-preprocess",
        // Project mode options
        "-nopwd": "--pwd false",
        // Misc
        "-install": "--install"
    ];
    string[] newArgv;
    foreach (arg; argv)
    {
        string newArg = arg;
        if (arg in argReplaceMap)
            newArg = argReplaceMap[arg];
        newArgv ~= newArg;
    }

    // Remove qmake-ng executable path from arguments
    newArgv = newArgv.remove(0);

    trace(argv);
    trace(newArgv);

    // FIXME: temponarily enable verbose output by default, for debugging purposes
    newArgv ~= ["--verbose", "--verbose", "--verbose"];

    // FIXME: original qmake autodetects spec if not specified somehow! this feature must be implemented too

    auto getoptResult = std.getopt.getopt(
        newArgv,
        std.getopt.config.caseSensitive,
        // Global options
        "version", &options.showVersion,
        // `help` option automatically added by `getopt`
        "outputfile", "", &options.outputFile,
        "verbose+", "", &options.logLevelNumber,
        // Template
        "template", "", &options.proTemplate,
        "templateprefix", "", &options.proTemplatePrefix,
        // Project file parser warning level
        "prowarninglevel", "", &options.proWarningLevel,
        // Mode
        "mode", "", &options.mode,
        // Variable assignment options
        "assignment", &options.assignmentTime,
        // Recursivity
        "recursive", &options.recursive,
        // Persistent property
        "persistent-property", "", &options.propertyAction,
        // Qt configuration file
        "qtconf", &options.qtConfPath,
        // Makefile mode options
        "cachefile", &options.cachePath,
        //std.getopt.config.required,
        "spec", &options.specFileName,
        "cache", &options.useCache,
        "depend", &options.doDeps,
        "moc", &options.doMoc,
        "createstub", &options.doStubMakefile,
        "dependheuristics", &options.doDepHeuristics,
        "do-preprocess", &options.doPreprocess,
        // Project mode options
        "pwd", &options.doPwd
    );
    /+
    if (cmdRet == QMakeGlobals::ArgumentMalformed) {
        fprintf(stderr, "***Option %s requires a parameter\n", qPrintable(args.at(x - 1)));
        return Option::QMAKE_CMDLINE_SHOW_USAGE | Option::QMAKE_CMDLINE_ERROR;
    }
    } else {
        fprintf(stderr, "***Unknown option %s\n", arg.toLatin1().constData());
        return Option::QMAKE_CMDLINE_SHOW_USAGE | Option::QMAKE_CMDLINE_ERROR;
    }
    +/

    // Show help and exit
    if (getoptResult.helpWanted || (argv.length < 2))
    {
        printUsage();
        return CmdLineFlags.QMAKE_CMDLINE_SHOW_USAGE;
    }

    // Show version and exit
    if (options.showVersion)
    {
        printVersionInfo();
        return CmdLineFlags.QMAKE_CMDLINE_BAIL;
    }

    switch (options.logLevelNumber)
    {
        case 0: globalLogLevel = LogLevel.error; break;
        case 1: globalLogLevel = LogLevel.warning; break;
        case 2: globalLogLevel = LogLevel.info; break;
        case 3: globalLogLevel = LogLevel.trace; break;
        default: globalLogLevel = LogLevel.error; break;
    }

    // Detect qmake project names in command-line arguments
    foreach (arg; newArgv)
    {
        // Skip options (they must already have double-dashed prefix)
        if (arg.startsWith("--"))
            continue;
        
        if (std.file.exists(arg) && std.file.isFile(arg))
            options.projectFileNames ~= arg;
        else
            options.assignmentStatements ~= arg;
    }

    trace("Mode: ", options.mode);
    trace("Log level: ", globalLogLevel);
    trace("Spec: ", options.specFileName);
    trace();
    trace("Positional arguments: ", newArgv);
    return 0;
}

private void printVersionInfo()
{
    writefln(
`QMake version %s
Using Qt version %s in %s
`
    , QMAKE_VERSION_STR, QT_VERSION_STR, QT_LIB_DIR
    );
}

private void printUsage()
{
    /*defaultGetoptPrinter("qmake-ng, Qt's qmake replacement.\n" ~
                           "Usage: " ~ args[0] ~ " [options] <project.pro>|<dir_path>\n" ~
                           "Options:", getoptResult.options);*/

    writefln(
`Usage: %s [mode] [options] [files]

QMake has two modes, one mode for generating project files based on
some heuristics, and the other for generating makefiles. Normally you
shouldn't need to specify a mode, as makefile generation is the default
mode for qmake, but you may use this to test qmake on an existing project

Mode:
  -project       Put qmake into project file generation mode%s
                 In this mode qmake interprets files as files to
                 be built,
                 defaults to %s
                 Note: The created .pro file probably will 
                 need to be edited. For example add the QT variable to 
                 specify what modules are required.
  -makefile      Put qmake into makefile generation mode%s
                 In this mode qmake interprets files as project files to
                 be processed, if skipped qmake will try to find a project
                 file in your current working directory

Warnings Options:
  -Wnone         Turn off all warnings; specific ones may be re-enabled by
                 later -W options
  -Wall          Turn on all warnings
  -Wparser       Turn on parser warnings
  -Wlogic        Turn on logic warnings (on by default)
  -Wdeprecated   Turn on deprecation warnings (on by default)

Options:
  * You can place any variable assignment in options and it will be *
  * processed as if it was in [files]. These assignments will be    *
  * processed before [files] by default.                            *
  -o file        Write output to file
  -d             Increase debug level
  -t templ       Overrides TEMPLATE as templ
  -tp prefix     Overrides TEMPLATE so that prefix is prefixed into the value
  -help          This help
  -v             Version information
  -early         All subsequent variable assignments will be
                 parsed right before default_pre.prf
  -before        All subsequent variable assignments will be
                 parsed right before [files] (the default)
  -after         All subsequent variable assignments will be
                 parsed after [files]
  -late          All subsequent variable assignments will be
                 parsed right after default_post.prf
  -norecursive   Don't do a recursive search
  -recursive     Do a recursive search
  -set <prop> <value> Set persistent property
  -unset <prop>  Unset persistent property
  -query <prop>  Query persistent property. Show all if <prop> is empty.
  -qtconf file   Use file instead of looking for qt.conf
  -cache file    Use file as cache           [makefile mode only]
  -spec spec     Use spec as QMAKESPEC       [makefile mode only]
  -nocache       Don't use a cache file      [makefile mode only]
  -nodepend      Don't generate dependencies [makefile mode only]
  -nomoc         Don't generate moc targets  [makefile mode only]
  -nopwd         Don't look for files in pwd [project mode only]
`
            , std.file.thisExePath()
            , ""
            , "*; *; *; *.ts; *.xlf; *.qrc"
            , " (default)"
        );
}
