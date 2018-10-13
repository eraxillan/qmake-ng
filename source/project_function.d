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

import common_const;
import project_variable;
import project_context;
// -------------------------------------------------------------------------------------------------

public class ProFunction
{
	static this()
	{
		replaceFunctions["first"] = new ProFunction("first", VariableType.STRING, true, 1, -1, [VariableType.STRING_LIST],
			(ref ProExecutionContext context, in string[] arguments) {
				if (arguments.length < 1)
					throw new Exception("Invalid argument count: expected 1, got " ~ to!string(arguments.length));
				return [arguments[0]];
			}
		);
		replaceFunctions["list"] = new ProFunction("list", VariableType.STRING_LIST, true, 1, -1, [VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				return arguments;
			}
		);

		replaceFunctions["replace"] = new ProFunction("replace", VariableType.STRING_LIST,
			false, 3, 0, [VariableType.STRING, VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				string variableName = arguments[0];
				string sourceString = arguments[1];
				string targetString = arguments[2];

				string variableValue = context.getVariableRawValue(variableName).join("");
				string result = variableValue.replace(sourceString, targetString);
				trace("Variable name: ", variableName);
				trace("Variable value: ", variableValue);
				trace("Variable replaced value: ", result);
				return [result];
			}
		);

		replaceFunctions["reverse"] = new ProFunction("reverse", VariableType.STRING_LIST,
			false, 1, 0, [VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				string variableName = arguments[0];

				string[] variableValue = context.getVariableRawValue(variableName);
				string[] result = variableValue.reverse;
				trace("Variable name: ", variableName);
				trace("Variable value: ", variableValue);
				trace("Variable reversed value: ", result);
				return result;
			}
		);

		replaceFunctions["split"] = new ProFunction("split", VariableType.STRING_LIST,
			false, 2, 0, [VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				string variableName = arguments[0];
				string separator = arguments[1];

				string variableValue = context.getVariableRawValue(variableName).join("");
				string[] result = variableValue.split(separator);
				trace("Variable name: ", variableName);
				trace("Variable value: ", variableValue);
				trace("Variable splitted value: ", result);
				return result;
			}
		);

		replaceFunctions["unique"] = new ProFunction("unique", VariableType.STRING_LIST,
			false, 1, 0, [VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
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
			}
		);

		// ------------------------------------------------------------------------------------------------------------

		testFunctions["for"] = new ProFunction("for", VariableType.BOOLEAN,
			false, 2, 0, [VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				error("Control flow error: currently implemented in project.d");
				return ["false"];
			}
		);

		testFunctions["include"] = new ProFunction("include", VariableType.BOOLEAN, false, 1, 0, [VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				error("Control flow error: currently implemented in project.d");
				return ["false"];
			}
		);

		testFunctions["equals"] = new ProFunction("equals", VariableType.BOOLEAN,
			false, 2, 0, [VariableType.STRING, VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				string variableName = arguments[0];
				string value = arguments[1];

				string[] variableRawValue = context.getVariableRawValue(variableName);
				if (variableRawValue.length >= 2)
					throw new Exception("Variable type mismatch: 'equals' test function can work only with string-typed variable");
				
				string variableValue = variableRawValue[0];
				trace("Variable value: '", variableValue, "'");
				trace("String to compare with: '", value, "'");
				return (variableRawValue[0] == value) ? ["true"] : ["false"];
			}
		);

		testFunctions["isEmpty"] = new ProFunction("isEmpty", VariableType.BOOLEAN,
			false, 1, 0, [VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				string variableName = arguments[0];

				if (!context.isBuiltinVariable(variableName) && !context.isUserDefinedVariable(variableName))
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
			}
		);

		testFunctions["contains"] = new ProFunction("contains", VariableType.BOOLEAN,
			false, 2, 0, [VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				string variableName = arguments[0];
				string value = arguments[1];

				string[] variableRawValue = context.getVariableRawValue(variableName);
				assert(variableRawValue.length >= 1);

				trace("Variable name: ", variableName);
				trace("Variable value: ", variableRawValue);
				trace("Value to search for: ", value);
				return (variableRawValue.countUntil(value) > 0) ? ["true"] : ["false"];
			}
		);

		testFunctions["exists"] = new ProFunction("exists", VariableType.BOOLEAN,
			false, 1, 0, [VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				string fileName = arguments[0];

				trace("File name: ", fileName);
				trace("Whether file exists: ", std.file.exists(fileName));
				return std.file.exists(fileName) ? ["true"] : ["false"];
			}
		);

		testFunctions["debug"] = new ProFunction("debug", VariableType.BOOLEAN,
			true, 2, -1, [VariableType.STRING_LIST],
			(ref ProExecutionContext /*context*/, in string[] arguments) {
				assert(arguments.length >= 2);

				// FIXME: implement level usage
				immutable int level = to!int(arguments[0]);
				writefln("DEBUG LEVEL: %d", level);

				string message = arguments[1 .. $].join(" ");
				writefln("Project DEBUG: " ~ message);

				return ["true"];
			}
		);

		testFunctions["message"] = new ProFunction("message", VariableType.BOOLEAN,
			true, 1, -1, [VariableType.STRING_LIST],
			(ref ProExecutionContext /*context*/, in string[] arguments) {
				assert(arguments.length >= 1);

				string message = arguments.join(" ");
				writefln("Project MESSAGE: " ~ message);

				return ["true"];
			}
		);

		testFunctions["warning"] = new ProFunction("warning", VariableType.BOOLEAN,
			true, 1, -1, [VariableType.STRING_LIST],
			(ref ProExecutionContext /*context*/, in string[] arguments) {
				assert(arguments.length >= 1);

				string message = arguments.join(" ");
				writefln("Project WARNING: " ~ message);

				return ["true"];
			}
		);

		testFunctions["error"] = new ProFunction("error", VariableType.VOID,
			true, 1, -1, [VariableType.STRING_LIST],
			(ref ProExecutionContext /*context*/, in string[] arguments) {
				assert(arguments.length >= 1);

				string message = arguments.join(" ");
				writefln("Project ERROR: " ~ message);

				return ["true"];
			}
		);

		testFunctions["unset"] = new ProFunction("unset", VariableType.VOID,
			false, 1, 0, [VariableType.STRING],
			(ref ProExecutionContext context, in string[] arguments) {
				assert(arguments.length == 1);

				string variableName = arguments[0];
				if (context.isVariableDefined(variableName))
					context.unsetVariable(variableName);
				else
					error("Variable '", variableName, "' was already unset, nothing to do");

				// NOTE: just a workaround to let compiler auto-deduce lambda function return type
				return ["true"];
			}
		);
	}
	
	this(in string name, in VariableType returnType, in bool isVariadic, in int requiredArgumentCount, in int optionalArgumentCount, VariableType[] argumentTypes, Action action)
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
	alias Action = const(string[]) function(ref ProExecutionContext context, in string[] arguments);
	public Action m_action;

	// qmake builtin test and replace functions
	private static ProFunction[string] testFunctions;
	private static ProFunction[string] replaceFunctions;
	
    static VariableType getFunctionArgumentType(in string functionName, in int argumentIndex)
	{
        if (isReplaceFunction(functionName))
		{
			string pureFunctionName = functionName[STR_FUNCTION_EXPAND_MARKER.length .. $];

			if ((pureFunctionName in replaceFunctions) is null)
				throw new Exception("Undefined replace function '" ~ pureFunctionName ~ "' found, aborting");

            return replaceFunctions[pureFunctionName].m_argumentTypes[argumentIndex];
        }
		else if (isTestFunction(functionName))
		{
			if ((functionName in testFunctions) is null)
				throw new Exception("Undefined test function '" ~ functionName ~ "' found, aborting");

            return testFunctions[functionName].m_argumentTypes[argumentIndex];
        }
		else
		{
            throw new Exception("Replace/test function not found: " ~ functionName);
        }
    }

	// NOTE: 'system' function has both replace and test versions
	static bool isTestFunction(in string str)
	{
		auto functionNames = [
			"cache", "CONFIG", "contains", "count", "debug",
			"defined", "equals", "error", "eval", "exists",
			"export", "for", "greaterThan", "if", "include",
			"infile", "isActiveConfig", "isEmpty", "isEqual",
			"lessThan", "load", "log", "message", "mkpath",
			"requires", "system", "touch", "unset", "warning",
			"write_file",
			"packagesExist", "prepareRecursiveTarget", "qtCompileTest", "qtHaveModule"
		];

		return functionNames.countUntil(str) != -1;
	}
	
	static bool hasTestFunctionDescription(in string name)
	in
	{
		assert(!name.empty, "function name cannot be empty");
	}
	do
	{
		return (name in testFunctions) !is null;
	}
	
	static ProFunction getTestFunctionDescription(in string name)
	in
	{
		assert(!name.empty, "function name cannot be empty");
	}
	do
	{
		return testFunctions[name];
	}

	static bool isReplaceFunction(in string str)
	{
		auto functionNames = [
			"absolute_path", "basename", "cat", "clean_path", "dirname",
			"enumerate_vars", "escape_expand", "find", "files", "first",
			"format_number", "fromfile", "getenv", "join", "last", "list",
			"lower", "member", "num_add", "prompt", "quote", "re_escape",
			"relative_path", "replace", "sprintf", "resolve_depends",
			"reverse", "section", "shadowed", "shell_path", "shell_quote",
			"size", "sort_depends", "sorted", "split", "str_member", "str_size",
			"system", "system_path", "system_quote", "take_first", "take_last",
			"unique", "upper", "val_escape"
		];

		if (!str.startsWith(STR_FUNCTION_EXPAND_MARKER))
			return false;

		// FIXME: check
		// return functionNames.canFind(str.substring(STR_FUNCTION_EXPAND_MARKER.length));
		return functionNames.countUntil(str[STR_FUNCTION_EXPAND_MARKER.length .. $]) != -1;
	}
	
	static bool hasReplaceFunctionDescription(in string name)
	in
	{
		assert(!name.empty, "function name cannot be empty");
	}
	do
	{
		return (name in replaceFunctions) !is null;
	}
	
	static ProFunction getReplaceFunctionDescription(in string name)
	in
	{
		assert(!name.empty, "function name cannot be empty");
	}
	do
	{
		return replaceFunctions[name];
	}
}
