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

module project_function;

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
import qmakeexception;
import common_const;
import common_utils;
import project_variable;
import project_context;

// -------------------------------------------------------------------------------------------------

enum ProFunctionType { Invalid = -1, Replace = 0, Test, Count }

public struct ProFunction
{
	this(in string name, in VariableType returnType, in bool isVariadic, in int requiredArgumentCount,
			in int optionalArgumentCount, VariableType[] argumentTypes, in Action action)
	{
		m_name = name;
		m_returnType = returnType;
		m_isVariadic = isVariadic;
		m_requiredArgumentCount = requiredArgumentCount;
		m_optionalArgumentCount = optionalArgumentCount;
		m_argumentTypes = argumentTypes;
		m_action = action;
	}

	// Compile-time function info
	public string m_name;
	public VariableType m_returnType;
	public bool m_isVariadic;
	public int m_requiredArgumentCount;
	public int m_optionalArgumentCount;
	public VariableType[] m_argumentTypes;

	// Run-time function info
	public string[] m_arguments;
	//alias Action = const(string[]) function(ref ProExecutionContext context, in string[] arguments);
	alias Action = const(string[]) delegate(ref ProExecutionContext context, in string[] arguments);
	public Action m_action;

	ProFunction dup() const @property
	{
		auto result = ProFunction(m_name.dup, m_returnType, m_isVariadic,
				m_requiredArgumentCount, m_optionalArgumentCount, m_argumentTypes.dup, m_action);
		return result;
	}
}

// --------------------------------------------------------------------------------------------------------------------

// qmake builtin test and replace functions
public immutable ProFunction[string] builtinTestFunctions;
public immutable ProFunction[string] builtinReplaceFunctions;

// Helpers
private static string[] returnBoolString(in bool b)
{
	return b ? [STR_TRUE] : [STR_FALSE];
}

private static bool isActiveConfig(in string config, in string specName,
		in string[] configVarValues, in bool useRegex = false)
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

static this()
{
	import std.exception : assumeUnique;

	ProFunction[string] temp; // mutable buffer

	// qmake built-in replace functions

	temp["first"] = ProFunction("first", VariableType.STRING, true, 1, -1,
			[VariableType.STRING_LIST], (ref ProExecutionContext context, in string[] arguments) {
		if (arguments.length < 1)
			throw new Exception(
				"Invalid argument count: expected 1, got " ~ to!string(arguments.length));
		return [arguments[0]];
	});
	temp["files"] = ProFunction("files", VariableType.STRING_LIST, false, 1, 1,
			[VariableType.STRING_LIST], (ref ProExecutionContext context, in string[] arguments) {
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
	temp["list"] = ProFunction("list", VariableType.STRING_LIST, true, 1, -1,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
		return arguments;
	});

	temp["replace"] = ProFunction("replace", VariableType.STRING_LIST, false, 3, 0, [VariableType.STRING,
			VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
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

	temp["reverse"] = ProFunction("reverse", VariableType.STRING_LIST, false, 1, 0,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
		string variableName = arguments[0];

		string[] variableValue = context.getVariableRawValue(variableName);
		string[] result = variableValue.reverse;
		trace("Variable name: ", variableName);
		trace("Variable value: ", variableValue);
		trace("Variable reversed value: ", result);
		return result;
	});

	temp["section"] = ProFunction("section", VariableType.STRING, false, 3, 1,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
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

	temp["split"] = ProFunction("split", VariableType.STRING_LIST, false, 2, 0,
			[VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				string variableName = arguments[0];
				string separator = arguments[1];

				string variableValue = context.getVariableRawValue(variableName).join("");
				string[] result = variableValue.split(separator);
				trace("Variable name: ", variableName);
				trace("Variable value: ", variableValue);
				trace("Variable splitted value: ", result);
				return result;
			});

	temp["unique"] = ProFunction("unique", VariableType.STRING_LIST, false, 1, 0,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
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
	temp["system"] = ProFunction("system", VariableType.STRING_LIST, false, 1, 2, [VariableType.STRING,
			VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
		trace(arguments);
		assert(arguments.length == 1 || arguments.length == 2 || arguments.length == 3);

		string command = arguments[0];

		/+
		bool blob = false;
        bool lines = false;
        bool singleLine = true;
        if (arguments.length >= 2)
		{
			if (toLower(arguments[1]) == "false")
				singleLine = false;
            else if (toLower(arguments[1]) == "blob")
                blob = true;
            else if (toLower(arguments[1]) == "lines")
                lines = true;
        }
//
		string exitStatusVar = arguments[2];

		import std.process : pipeProcess, pipeShell, wait, Redirect;

		// Launch external process
		// NOTE: assume that it writes to stdout and might write to stderr
		command = "LC_ALL=C g++ -pipe -E -v -xc++ - 2>&1 </dev/null >/dev/null";
		auto pipes = pipeShell(command, Redirect.stdout | Redirect.stderr);

		// Wait for process completion
		int exitCode = wait(pipes.pid);
		trace("External process finished with code `", exitCode, "`");

		// Save exit status value in the specified variable (if any)
		if (arguments.length >= 3)
		{
			immutable string variableName = arguments[2];
			// FIXME: also check using regex whether such name is suitable as ID
			assert(!variableName.empty);
			// NOTE: whether this variable defined or not doesn't matter
			context.assignVariable(variableName, [to!string(exitCode)], VariableType.STRING);
			trace("Exit code was saved into project variable `", variableName, "`");
		}

		// Store lines of errors.
		/*string[] errors;
		foreach (line; pipes.stderr.byLine) errors ~= line.idup;
		writeln("");
		writeln(errors);*/
		
		lines = false;
		if (lines)
		{
			/*
            QTextStream stream(bytes);
            while (!stream.atEnd())
                ret += ProString(stream.readLine());
			*/

			// Store lines of output
			string[] output;
			foreach (line; pipes.stdout.byLine) output ~= line.idup;
			writeln("");
			foreach (line; output)
				writeln(line);
			writeln("");
        }
		else
		{
			string output;
			char[256] buffer;
			while (!pipes.stdout.eof())
			{
				output ~= pipes.stdout.rawRead(buffer);
			}
			writeln("");
			writeln(output);
			writeln("");

            /*QString output = QString::fromLocal8Bit(bytes);
            if (blob) {
                ret += ProString(output);
            } else {
                output.replace(QLatin1Char('\t'), QLatin1Char(' '));
                if (singleLine)
                    output.replace(QLatin1Char('\n'), QLatin1Char(' '));
                ret += split_value_list(QStringRef(&output));
            }*/
        }
        +/

		bool b = true;
		if (b)
			assert(0);
		return [""];
	});

	temp.rehash; // for faster lookups
	builtinReplaceFunctions = assumeUnique(temp);

	// ----------------------------------------------------------------------------------------------------------------
	// qmake built-in test functions
	temp.clear();
	assert(temp.empty);

	temp["defined"] = ProFunction("defined", VariableType.BOOLEAN, false, 1, 1,
			[VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
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

	temp["CONFIG"] = ProFunction("CONFIG", VariableType.BOOLEAN, false, 1, 1,
			[VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
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

	temp["for"] = ProFunction("for", VariableType.BOOLEAN, false, 2, 0, [VariableType.STRING,
			VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
		error("Control flow error: currently implemented in project.d");
		return ["false"];
	});

	temp["include"] = ProFunction("include", VariableType.BOOLEAN, false, 1, 3,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
		import project : Project;
		import persistent_property : PersistentPropertyStorage;

		assert(arguments.length >= 1);
		if (arguments.length > 1)
			error("Optional 'include' test functions arguments are not implemented yet");
		string projectFileName = arguments[0];
		string projectDirectory = context.getVariableRawValue("PWD")[0];
		if (!isAbsolute(projectFileName))
			projectFileName = buildNormalizedPath(absolutePath(projectFileName, projectDirectory));
		trace("absolute project path: '", projectFileName, "'");
		if (!exists(projectFileName) || !isFile(projectFileName))
		{
			//error("project file '", projectFileName, "' was not found, so return FALSE");
			//return false;
			throw new NotImplementedException("file not found: " ~ "`" ~ projectFileName ~ "`");
		}

		// FIXME: pass persistent storage object too, with context
		auto persistentStorage = new PersistentPropertyStorage();
		auto pro = new Project(context, persistentStorage);
		if (!pro.eval(projectFileName))
		{
			error("qmake project file '", projectFileName, "' evaluation failed, include failed");
			return ["false"];
		}
		info("qmake project file '" ~ projectFileName ~ "' was successfully parsed and included");
		return ["true"];
	});

	temp["load"] = ProFunction("load", VariableType.BOOLEAN, false, 1, 0,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
		import project : Project;
		import persistent_property : PersistentPropertyStorage;

		assert(arguments.length >= 1);
		if (arguments.length > 1)
			error("Optional 'include' test functions arguments are not implemented yet");
		string projectFileName = arguments[0];

		// FIXME: use PlatformInfo class, need to extract it from app.d first
		string featureDirectory = "/opt/Qt/5.11.3/gcc_64/mkspecs/features";
		projectFileName = buildNormalizedPath(featureDirectory, projectFileName);
		projectFileName = std.path.setExtension(projectFileName, "prf");
		trace("absolute feature file path: '", projectFileName, "'");
		if (!std.file.exists(projectFileName) || !std.file.isFile(projectFileName))
		{
			error("feature file '", projectFileName,
				"' was not found or not a file, so return FALSE");
			throw new NotImplementedException("file not found: " ~ "`" ~ projectFileName ~ "`");
			//return false;
		}

		// FIXME: pass persistent storage object too, with context
		auto persistentStorage = new PersistentPropertyStorage();
		auto pro = new Project(context, persistentStorage);
		if (!pro.eval(projectFileName))
		{
			error("qmake project file '", projectFileName, "' evaluation failed, include failed");
			return ["false"];
		}
		info("qmake project file '" ~ projectFileName ~ "' was successfully parsed and included");
		return ["true"];
	});

	temp["equals"] = ProFunction("equals", VariableType.BOOLEAN, false, 2, 0,
			[VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
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

	temp["isEmpty"] = ProFunction("isEmpty", VariableType.BOOLEAN, false, 1, 0,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
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

	temp["contains"] = ProFunction("contains", VariableType.BOOLEAN, false, 2, 0,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
		string variableName = arguments[0];
		string value = arguments[1];

		string[] variableRawValue = context.getVariableRawValue(variableName);
		assert(variableRawValue.length >= 1);

		trace("Variable name: ", variableName);
		trace("Variable value: ", variableRawValue);
		trace("Value to search for: ", value);
		return (variableRawValue.countUntil(value) > 0) ? ["true"] : ["false"];
	});

	temp["exists"] = ProFunction("exists", VariableType.BOOLEAN, false, 1, 0,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
		string fileName = arguments[0];

		trace("File name: ", fileName);
		trace("Whether file exists: ", std.file.exists(fileName));
		return std.file.exists(fileName) ? ["true"] : ["false"];
	});

	temp["debug"] = ProFunction("debug", VariableType.BOOLEAN, true, 2, -1,
			[VariableType.STRING_LIST], (ref ProExecutionContext /*context*/ , in string[] arguments) {
		assert(arguments.length >= 2);

		// FIXME: implement level usage
		immutable int level = to!int(arguments[0]);
		writefln("DEBUG LEVEL: %d", level);

		string message = arguments[1 .. $].join(" ");
		writefln("Project DEBUG: " ~ message);

		return ["true"];
	});

	temp["message"] = ProFunction("message", VariableType.BOOLEAN, true, 1, -1,
			[VariableType.STRING_LIST], (ref ProExecutionContext /*context*/ , in string[] arguments) {
		assert(arguments.length >= 1);

		string message = arguments.join(" ");
		writefln("Project MESSAGE: " ~ message);

		return ["true"];
	});

	temp["warning"] = ProFunction("warning", VariableType.BOOLEAN, true, 1, -1,
			[VariableType.STRING_LIST], (ref ProExecutionContext /*context*/ , in string[] arguments) {
		assert(arguments.length >= 1);

		string message = arguments.join(" ");
		writefln("Project WARNING: " ~ message);

		return ["true"];
	});

	temp["error"] = ProFunction("error", VariableType.BOOLEAN, true, 1, -1,
			[VariableType.STRING_LIST], (ref ProExecutionContext /*context*/ , in string[] arguments) {
		assert(arguments.length >= 1);

		string message = arguments.join(" ");
		writefln("Project ERROR: " ~ message);

		return ["true"];
	});

	// export(variablename)
	// Exports the current value of variablename from the local context of a function to the global context.
	temp["export"] = ProFunction("export", VariableType.BOOLEAN, false, 1, 0,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
		assert(arguments.length == 1);

		string variableName = arguments[0];
		if (!context.isVariableDefined(variableName))
			throw new Exception("Variable '" ~ variableName ~ "' was defined yet");

		// FIXME: implement

		// NOTE: just a workaround to let compiler auto-deduce lambda function return type
		return ["true"];
	});

	temp["unset"] = ProFunction("unset", VariableType.BOOLEAN, false, 1, 0,
			[VariableType.STRING], (ref ProExecutionContext context, in string[] arguments) {
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
