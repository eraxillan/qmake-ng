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

module source.project_function;

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
import std.regex;

import source.qmakeexception;
import source.common_const;
import source.common_utils;
import source.io_utils;
import source.logger;
import source.project_variable;
import source.project_context;
import source.persistent_property;

// -------------------------------------------------------------------------------------------------
public:

enum ProFunctionType { Invalid = -1, Replace = 0, Test, Count }

struct FunctionTypeInfo
{
    bool isVariadic;
    VariableType[] argumentTypes;
    VariableType returnType;

    FunctionTypeInfo dup() const @property
    {
        return FunctionTypeInfo(isVariadic, argumentTypes.dup, returnType);
    }
}

struct ProFunction
{
    this(const FunctionBaseInfo fbi, const FunctionTypeInfo fti, const Action action)
    {
        this.fbi = fbi.dup;
        this.fti = fti.dup;
        this.action = action;
    }

    // Compile-time function info
    FunctionBaseInfo fbi;
    FunctionTypeInfo fti;

    // Run-time function info
    string[] arguments;
    alias Action = const(string[]) delegate(ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments);
    Action action;

    ProFunction dup() const @property
    {
        return ProFunction(fbi.dup, fti.dup, action);
    }
}

// --------------------------------------------------------------------------------------------------------------------

// qmake builtin test and replace functions
immutable ProFunction[string] builtinTestFunctions;
immutable ProFunction[string] builtinReplaceFunctions;

// Helpers
private:

static string[] returnBoolString(const bool b)
{
    return b ? [STR_TRUE] : [STR_FALSE];
}

static bool isActiveConfig(const string config, const string specName,
        const string[] configVarValues, const bool useRegex = false)
{
    // Magic types for easy flipping
    if (config == STR_TRUE)
        return true;
    if (config == STR_FALSE)
        return false;

    // FIXME: implement "host_build { ... }" scope statement in parser first
    /+
    if (config == STR_HOSTBUILD)
        return m_hostBuild;
    +/

    if (useRegex && (config.canFind('*') || config.canFind('?')))
    {
        // FIXME: need testing!
        bool b = true;
        if (b)
            assert(0);

        auto re = regex(wildcardToRegex(config));

        // mkspecs
        if (specName.matchFirst(re))
            return true;

        // CONFIG variable
        foreach (configValue; configVarValues)
        {
            if (configValue.matchFirst(re))
                return true;
        }
    }
    else
    {
        // mkspecs
        if (specName == config)
            return true;

        // CONFIG variable
        if (configVarValues.canFind(config))
            return true;
    }

    return false;
}

shared static this()
{
    import std.exception : assumeUnique;

    ProFunction[string] temp; // mutable buffer

    // qmake built-in replace functions

    temp["first"] = ProFunction(FunctionBaseInfo("first", 1, -1), FunctionTypeInfo(true, [VariableType.STRING_LIST], VariableType.STRING),
                                (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        if (arguments.length < 1)
            throw new Exception(
                "Invalid argument count: expected 1, got " ~ to!string(arguments.length));
        return [arguments[0]];
    });
    temp["files"] = ProFunction(FunctionBaseInfo("files", 1, 1), FunctionTypeInfo(false, [VariableType.STRING_LIST], VariableType.STRING_LIST),
                                (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        string[] result;

        immutable string pattern = arguments[0];
        immutable bool recursive = arguments.length == 2
            ? (arguments[1] == "true" ? true : false) : false;

        immutable string dir = std.path.dirName(pattern);
        immutable string filenamePattern = std.path.baseName(pattern);

        trace("Glob pattern: ", pattern);
        trace("Directory: ", dir);
        trace("File name pattern: ", filenamePattern);
        assert(std.file.exists(dir));
        auto deResult = std.file.dirEntries(dir, filenamePattern, recursive
            ? SpanMode.depth : SpanMode.shallow, true);
        foreach (DirEntry de; deResult)
        {
            assert(std.file.exists(de.name));

            result ~= de.name;
        }

        // Showcase stable sorting
        import std.algorithm.mutation : SwapStrategy;

        std.algorithm.sort!("a < b", SwapStrategy.unstable)(result);
        trace("Matches:");
        writeln(result);

        return result;
    });
    temp["list"] = ProFunction(FunctionBaseInfo("list", 1, -1), FunctionTypeInfo(true, [VariableType.STRING], VariableType.STRING_LIST),
                               (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        return arguments;
    });

    temp["replace"] = ProFunction(FunctionBaseInfo("replace", 3, 0),
                                  FunctionTypeInfo(false, [VariableType.STRING, VariableType.STRING, VariableType.STRING], VariableType.STRING_LIST),
                                  (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
                string variableName = arguments[0];
                string sourceString = arguments[1];
                string targetString = arguments[2];

                // FIXME: avoid join() usage
                string variableValue = context.getVariableRawValue(variableName).join("");

                string result = replaceAll(variableValue, regex(sourceString, "g"), targetString);
                trace("Source: ", variableValue);
                trace("Regular expression: ", sourceString);
                trace("Result: ", result);
                return [result];
            });

    temp["reverse"] = ProFunction(FunctionBaseInfo("reverse", 1, 0),  FunctionTypeInfo(false, [VariableType.STRING], VariableType.STRING_LIST),
                                  (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        string variableName = arguments[0];

        string[] variableValue = context.getVariableRawValue(variableName);
        string[] result = variableValue.reverse;
        trace("Variable name: ", variableName);
        trace("Variable value: ", variableValue);
        trace("Variable reversed value: ", result);
        return result;
    });

    temp["section"] = ProFunction(FunctionBaseInfo("section", 3, 1), FunctionTypeInfo(false, [VariableType.STRING], VariableType.STRING),
                                  (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        trace(arguments);
        assert(arguments.length == 3 || arguments.length == 4);

        string variableName = arguments[0];
        string separator = arguments[1];
        int begin = to!int(arguments[2]);
        int end = -1;
        if (arguments.length == 4)
            end = to!int(arguments[3]);

        string[] value = context.getVariableRawValue(variableName);

        string[] result;
        foreach (str; value)
        {
            result ~= sectionString(str, separator, begin, end);
        }

        trace("Variable name: ", variableName);
        trace("Variable value: ", value);
        trace("Sectioned value: ", result);

        return result;
    });

    temp["split"] = ProFunction(FunctionBaseInfo("split", 2, 0), FunctionTypeInfo(false, [VariableType.STRING, VariableType.STRING], VariableType.STRING_LIST),
                                (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
                string variableName = arguments[0];
                string separator = arguments[1];

                string variableValue = context.getVariableRawValue(variableName).join("");
                string[] result = variableValue.split(separator);
                trace("Variable name: ", variableName);
                trace("Variable value: ", variableValue);
                trace("Variable splitted value: ", result);
                return result;
            });

    temp["unique"] = ProFunction(FunctionBaseInfo("unique", 1, 0), FunctionTypeInfo(false, [VariableType.STRING], VariableType.STRING_LIST),
                                 (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        string variableName = arguments[0];

        string[] variableValue;
        if (context.isVariableDefined(variableName))
            variableValue = context.getVariableRawValue(variableName);
        else
            error("Undefined variable '", variableName, "', assume empty string");

        string[] result;
        foreach (item; variableValue)
        {
            if (result.countUntil(item) == -1)
                result ~= item;
        }
        trace("Variable name: ", variableName);
        trace("Variable value: ", variableValue);
        trace("Variable uniqued value: ", result);
        return result;
    });

    // system(command[, mode[, stsvar]])
    temp["system"] = ProFunction(FunctionBaseInfo("system", 1, 2),
                                 FunctionTypeInfo(false, [VariableType.STRING, VariableType.STRING, VariableType.STRING], VariableType.STRING_LIST),
                                 (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        string[] result;

        trace(arguments);
        assert(arguments.length == 1 || arguments.length == 2 || arguments.length == 3);

        string command = arguments[0];

        // Determine output result collecting mode
        bool blob = false;
        bool lines = false;
        bool singleLine = true;
        if (arguments.length >= 2)
        {
            if (toLower(arguments[1]) == "false")
            {
                trace("system(): `single-line` mode disabled");
                singleLine = false;
            }
            else if (toLower(arguments[1]) == "blob")
            {
                trace("system(): `blob` mode enabled");
                blob = true;
            }
            else if (toLower(arguments[1]) == "lines")
            {
                trace("system(): `lines` mode enabled");
                lines = true;
            }
            else
                throw new NotSupportedException("Invalid system() replace function mode "
                    ~ "`" ~ arguments[1] ~ "`");
        }

        import std.process : pipeProcess, pipeShell, wait, Redirect;

        // Launch external process
        // NOTE: assume that it writes to stdout and might write to stderr
        auto pipes = pipeShell(command, Redirect.stdout | Redirect.stderr);

        // Wait for process completion
        int exitCode = wait(pipes.pid);
        trace("External process finished with code `", exitCode, "`");

        // Save exit status value in the specified variable (if any)
        if (arguments.length == 3)
        {
            immutable string variableName = arguments[2];
            // FIXME: also check using regex whether such name is suitable as ID
            assert(!variableName.empty);
            // NOTE: whether this variable defined or not doesn't matter
            context.assignVariable(variableName, [to!string(exitCode)], VariableType.STRING);
            trace("Exit code ", "`", exitCode, "` was saved into project variable `", variableName, "`");
        }

        // Store lines of errors
        string[] errors;
        foreach (line; pipes.stderr.byLine)
            errors ~= line.idup;
        if (!errors.empty)
        {
            writeln("system() replace function stderr output:");
            writeln("----------------------------------------");
            writeln(errors);
            writeln("----------------------------------------");
        }

        if (lines)
        {
            // Store lines of output
            trace("line-mode output:");
            foreach (line; pipes.stdout.byLine)
                result ~= line.idup;
        }
        else
        {
            string output;
            char[256] buffer;
            while (!pipes.stdout.eof())
            {
                output ~= pipes.stdout.rawRead(buffer);
            }
            if (blob)
            {
                trace("blob-mode output: ");

                result ~= output;
            }
            else
            {
                trace("raw output: ", output);

                string prettyOutput = output.replace('\t', ' ');
                if (singleLine)
                {
                    trace("single-line mode output: ");
                    prettyOutput = prettyOutput.replace('\n', ' ');
                }
                else
                    trace("text-mode output: ");
                
                result ~= splitString(output, " ", true);
            }
        }

        writeln("----------------------------------------");
        writeln(result);
        writeln("----------------------------------------");
        return result;
    });

    temp.rehash; // for faster lookups
    builtinReplaceFunctions = assumeUnique(temp);

    // ----------------------------------------------------------------------------------------------------------------
    // qmake built-in test functions
    temp.clear();
    assert(temp.empty);

    temp["defined"] = ProFunction(FunctionBaseInfo("defined", 1, 1),
                                  FunctionTypeInfo(false, [VariableType.STRING, VariableType.STRING], VariableType.BOOLEAN),
                                  (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
                trace(arguments);
                assert(arguments.length == 1 || arguments.length == 2);

                immutable string name = arguments[0];
                if (arguments.length == 2)
                {
                    immutable string type = arguments[1];
                    switch (type)
                    {
                    case "test":
                        {
                            // return returnBool(m_functionDefs.testFunctions.contains(var));
                            throw new NotImplementedException("test");
                        }
                    case "replace":
                        {
                            // return returnBool(m_functionDefs.replaceFunctions.contains(var));
                            throw new NotImplementedException("replace");
                        }
                    case "var":
                        {
                            immutable bool b = context.isVariableDefined(name);
                            trace("Variable name: ", name);
                            trace("Whether variable defined: ", b);
                            return returnBoolString(b);
                        }
                    default:
                        throw new Exception("Unexpected type " ~ type);
                    }
                }
                // return returnBool(m_functionDefs.replaceFunctions.contains(var) || m_functionDefs.testFunctions.contains(var));
                throw new NotImplementedException("test || replace");
            });

    temp["CONFIG"] = ProFunction(FunctionBaseInfo("CONFIG", 1, 1),
                                 FunctionTypeInfo(false, [VariableType.STRING, VariableType.STRING], VariableType.BOOLEAN),
                                 (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
                trace(arguments);
                assert(arguments.length == 1 || arguments.length == 2);

                // FIXME: add spec to built-in variables and remove this hardcode
                const string specName = "linux-g++";

                string[] configValue = context.getVariableRawValue("CONFIG");
                immutable(bool) isConfigValueEmpty = configValue.empty || configValue[0].empty;
                assert(!isConfigValueEmpty);
                trace("CONFIG variable value:");
                trace(configValue);

                if (arguments.length == 1)
                {
                    immutable bool b = isActiveConfig(arguments[0], specName, configValue, false);
                    trace("CONFIG result 1: ", b);
                    return returnBoolString(b);
                }

                const auto mutuals = splitString(arguments[1], "|", true);
                trace("Mutual conditions: ", mutuals);

                for (long i = configValue.length - 1; i >= 0; i--)
                {
                    for (long mut = 0; mut < mutuals.length; mut++)
                    {
                        if (configValue[i] == mutuals[mut].strip())
                        {
                            bool b = configValue[i] == arguments[0];
                            trace("CONFIG result 2: ", b);
                            return returnBoolString(b);
                        }
                    }
                }

                trace("CONFIG result 3: ", false);
                return returnBoolString(false);
            });
    temp["isActiveConfig"] = temp["CONFIG"];

    temp["for"] = ProFunction(FunctionBaseInfo("for", 2, 0),
                              FunctionTypeInfo(false, [VariableType.STRING, VariableType.STRING], VariableType.BOOLEAN),
                              (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        error("Control flow error: currently implemented in project.d");
        bool b = false; assert(b);
        return ["false"];
    });

    temp["include"] = ProFunction(FunctionBaseInfo("include", 1, 3), FunctionTypeInfo(false, [VariableType.STRING], VariableType.BOOLEAN),
                                  (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        import source.project : Project;

        assert(arguments.length >= 1);
        if (arguments.length > 1)
            error("Optional 'include' test functions arguments are not implemented yet");

        string projectFileName = arguments[0];
        string projectDirectory = context.getVariableRawValue("PWD")[0];
        if (!isAbsolute(projectFileName))
            projectFileName = buildNormalizedPath(absolutePath(projectFileName, projectDirectory));
        trace("absolute project path: '", projectFileName, "'");
        NgLogger.get().traceIncludeBegin(projectFileName);
        if (!isValidFilePath(projectFileName))
        {
            //error("project file '", projectFileName, "' was not found, so return FALSE");
            //return false;
            throw new NotImplementedException("file not found: " ~ "`" ~ projectFileName ~ "`");
        }

        auto pro = new Project(context, persistentStorage);
        if (!pro.eval(projectFileName))
        {
            error("\n===============================================================================================");
            error("[include] Subproject file '" ~ projectFileName ~ "' evaluation failed!");
            error("\n===============================================================================================");

            return ["false"];
        }

        info("\n===============================================================================================");
        info("[include] Subproject file '" ~ projectFileName ~ "' was successfully evaluated");
        info("\n===============================================================================================");

        NgLogger.get().traceIncludeEnd(projectFileName);

        return ["true"];
    });

    temp["load"] = ProFunction(FunctionBaseInfo("load", 1, 0), FunctionTypeInfo(false, [VariableType.STRING], VariableType.BOOLEAN),
                               (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        import source.project : Project;

        assert(arguments.length >= 1);
        if (arguments.length > 1)
            error("Optional 'include' test functions arguments are not implemented yet");
        string projectFileName = arguments[0];

        string[] featureDirectoryRaw = context.getVariableRawValue("QMAKESPEC_FEATURES");
        assert(!featureDirectoryRaw.empty && featureDirectoryRaw.length == 1);
        string featureDirectory = featureDirectoryRaw[0];
        assert(isValidDirectoryPath(featureDirectory));

        projectFileName = buildNormalizedPath(featureDirectory, projectFileName);
        projectFileName = std.path.setExtension(projectFileName, "prf");
        trace("absolute feature file path: '", projectFileName, "'");
        NgLogger.get().traceLoadBegin(projectFileName);
        if (!isValidFilePath(projectFileName))
        {
            error("feature file '", projectFileName,
                "' was not found or not a file, so return FALSE");
            throw new NotImplementedException("file not found: " ~ "`" ~ projectFileName ~ "`");
            //return false;
        }

        auto pro = new Project(context, persistentStorage);
        if (!pro.eval(projectFileName))
        {
            error("\n===============================================================================================");
            error("[load] Feature project file '" ~ projectFileName ~ "' evaluation failed!");
            error("\n===============================================================================================");

            return ["false"];
        }
        
        info("\n===============================================================================================");
        info("[load] Feature project file '" ~ projectFileName ~ "' was successfully evaluated");
        info("\n===============================================================================================");
        
        NgLogger.get().traceLoadEnd(projectFileName);
        
        return ["true"];
    });

    temp["equals"] = ProFunction(FunctionBaseInfo("equals", 2, 0),
                                 FunctionTypeInfo(false, [VariableType.STRING, VariableType.STRING], VariableType.BOOLEAN),
                                 (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
                string variableName = arguments[0];
                string value = arguments[1];
                trace("Variable name: ", "`", variableName, "`");
                trace("Variable value: ", "`", value, "`");

                string[] variableRawValue = context.getVariableRawValue(variableName);
                if (variableRawValue.length >= 2)
                    throw new Exception(
                        "Variable type mismatch: 'equals' test function can work only with string-typed variable");

                string variableValue = variableRawValue[0];
                trace("Variable value: '", variableValue, "'");
                trace("String to compare with: '", value, "'");
                return (variableRawValue[0] == value) ? ["true"] : ["false"];
            });
    temp["isEqual"] = temp["equals"];

    temp["isEmpty"] = ProFunction(FunctionBaseInfo("isEmpty", 1, 0), FunctionTypeInfo(false, [VariableType.STRING], VariableType.BOOLEAN),
                                  (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        string variableName = arguments[0];

        if (!context.isVariableDefined(variableName))
        {
            trace("Variable was not defined yet");
            return ["true"];
        }

        string[] variableRawValue = context.getVariableRawValue(variableName);
        immutable(bool) isEmpty = variableRawValue.empty || variableRawValue[0].empty;
        trace("Variable name: ", variableName);
        trace("Variable value: ", variableRawValue);
        trace("Variable value is empty: ", isEmpty);
        return isEmpty ? ["true"] : ["false"];
    });

    temp["contains"] = ProFunction(FunctionBaseInfo("contains", 2, 0), FunctionTypeInfo(false, [VariableType.STRING], VariableType.BOOLEAN),
                                   (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        string variableName = arguments[0];
        string value = arguments[1];

        string[] variableRawValue = context.getVariableRawValue(variableName);
        assert(variableRawValue.length >= 1);

        trace("Variable name: ", variableName);
        trace("Variable value: ", variableRawValue);
        trace("Value to search for: ", value);
        return (variableRawValue.countUntil(value) > 0) ? ["true"] : ["false"];
    });

    temp["exists"] = ProFunction(FunctionBaseInfo("exists", 1, 0), FunctionTypeInfo(false, [VariableType.STRING], VariableType.BOOLEAN),
                                 (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        string fileName = arguments[0];

        trace("File name: ", fileName);
        trace("Whether file exists: ", std.file.exists(fileName));
        return std.file.exists(fileName) ? ["true"] : ["false"];
    });

    temp["debug"] = ProFunction(FunctionBaseInfo("debug", 2, -1), FunctionTypeInfo(true, [VariableType.STRING_LIST], VariableType.BOOLEAN),
                                (ref ProExecutionContext /*context*/, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        assert(arguments.length >= 2);

        // FIXME: implement level usage
        immutable int level = to!int(arguments[0]);
        writefln("DEBUG LEVEL: %d", level);

        string message = arguments[1 .. $].join(" ");
        writefln("Project DEBUG: " ~ message);

        return ["true"];
    });

    temp["message"] = ProFunction(FunctionBaseInfo("message", 1, -1), FunctionTypeInfo(true, [VariableType.STRING_LIST], VariableType.BOOLEAN),
                                  (ref ProExecutionContext /*context*/, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        assert(arguments.length >= 1);

        string message = arguments.join(" ");
        writefln("Project MESSAGE: " ~ message);

        return ["true"];
    });

    temp["warning"] = ProFunction(FunctionBaseInfo("warning", 1, -1), FunctionTypeInfo(true, [VariableType.STRING_LIST], VariableType.BOOLEAN),
                                  (ref ProExecutionContext /*context*/, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        assert(arguments.length >= 1);

        string message = arguments.join(" ");
        writefln("Project WARNING: " ~ message);

        return ["true"];
    });

    temp["error"] = ProFunction(FunctionBaseInfo("error", 1, -1), FunctionTypeInfo(true, [VariableType.STRING_LIST], VariableType.BOOLEAN),
                                (ref ProExecutionContext /*context*/, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        assert(arguments.length >= 1);

        string message = arguments.join(" ");
        writefln("Project ERROR: " ~ message);

        return ["true"];
    });

    // export(variablename)
    // Exports the current value of variablename from the local context of a function to the global context.
    temp["export"] = ProFunction(FunctionBaseInfo("export", 1, 0), FunctionTypeInfo(false, [VariableType.STRING], VariableType.BOOLEAN),
                                 (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        assert(arguments.length == 1);

        string variableName = arguments[0];
        if (!context.isVariableDefined(variableName))
            throw new Exception("Variable '" ~ variableName ~ "' was defined yet");

        // FIXME: implement!
        // Specified variable must be set in parent context (if any)

        // NOTE: just a workaround to let compiler auto-deduce lambda function return type
        return ["true"];
    });

    temp["unset"] = ProFunction(FunctionBaseInfo("unset", 1, 0), FunctionTypeInfo(false, [VariableType.STRING], VariableType.BOOLEAN),
                                (ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments) {
        assert(arguments.length == 1);

        string variableName = arguments[0];
        if (context.isVariableDefined(variableName))
            context.unsetVariable(variableName);
        else
            error("Variable '", variableName, "' was already unset, nothing to do");

        // NOTE: just a workaround to let compiler auto-deduce lambda function return type
        return ["true"];
    });

    temp.rehash; // for faster lookups
    builtinTestFunctions = assumeUnique(temp);
}
