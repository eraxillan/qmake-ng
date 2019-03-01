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

module project_context;

import std.experimental.logger;

import std.typecons;
import std.uni;
import std.algorithm;
import std.container;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;
import std.range;
import std.regex;
import std.process;

import common_const;
import common_utils;
import qmakeexception;
import project_variable;
import project_function;
import persistent_property;
import qmakeparser;

// -------------------------------------------------------------------------------------------------

/*
// 1) OUTPUT_LIB = $${LIB_NAME}
private const auto projectVariableExpansionRegex_1 = r"\$\$\{(?P<name>([_a-zA-Z][_a-zA-Z0-9]*)+)\}";
// 2) OUTPUT_LIB = $$LIB_NAME
private const auto projectVariableExpansionRegex_2 = r"\$\$(?P<name>([_a-zA-Z][_a-zA-Z0-9]*)+)\b";
// 3) DESTDIR = $(PWD)
private const auto environmentVariableExpansionRegex_1 = r"\$\((?P<name>(([_a-zA-Z][_a-zA-Z0-9]*)+))\)";
// 4) DESTDIR = $$(PWD)
private const auto environmentVariableExpansionRegex_2 = r"\$\$\((?P<name>(([_a-zA-Z][_a-zA-Z0-9]*)+))\)";
// 5) target.path = $$[QT_INSTALL_PLUGINS]/designer
private const auto qmakePropertyExpansionRegex_1 = r"\$\$\[(?P<name>([_a-zA-Z][_a-zA-Z0-9]*)+)\]";
private const auto qmakePropertyExpansionRegex_2 = r"\$\$\[(?P<name>([_a-zA-Z][_a-zA-Z0-9]*)+\/get)\]";
*/

// -------------------------------------------------------------------------------------------------

/**
 * Project file execution context.
 * Store built-in and variables and functions
 */
public class ProExecutionContext
{
    // qmake built-in and user-defined variables
	private ProVariable[string] m_builtinVariables;
	private ProVariable[string] m_userVariables;

    // qmake user-defined test and replace functions
	private ProFunction[string] m_userTestFunctions;
	private ProFunction[string] m_userReplaceFunctions;

	private ProVariable[string] cloneBuiltinVariables() const
	{
		ProVariable[string] result;
		foreach (name; builtinVariables.keys)
			result[name] = builtinVariables[name].dup();
		return result;
	}

    this()
	{
		m_builtinVariables = cloneBuiltinVariables();
    }

    public void reset()
	{
        m_builtinVariables = cloneBuiltinVariables();
        m_userVariables.clear;
    }

    public bool isBuiltinVariable(in string name) const
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        return (name in m_builtinVariables) !is null;
    }

    public bool isUserDefinedVariable(in string name) const
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        return (name in m_userVariables) !is null;
    }

    public bool isUserDefinedTestFunction(in string name) const
    in
    {
        assert(!name.empty, "test function name cannot be empty");
    }
    do
    {
        return (name in m_userTestFunctions) !is null;
    }

    public bool isUserDefinedReplaceFunction(in string name) const
    in
    {
        assert(!name.empty, "replace function name cannot be empty");
    }
    do
    {
        return (name in m_userReplaceFunctions) !is null;
    }

    public string[] getBuiltinVariableNames() const
    {
        return m_builtinVariables.keys.sort.release();
    }

    public string[] getUserDefinedVariableNames() const
    {
        return m_userVariables.keys.sort.release();
    }

    public string[] getUserDefinedTestFunctionNames() const
    {
        return m_userTestFunctions.keys.sort.release();
    }

    public string[] getUserDefinedReplaceFunctionNames() const
    {
        return m_userReplaceFunctions.keys.sort.release();
    }

    public void setupPaths(in string projectFileName)
    {
        assignVariable("PWD", [dirName(projectFileName)], VariableType.STRING);
        assignVariable("OUT_PWD", [dirName(projectFileName)], VariableType.STRING);
        assignVariable("_PRO_FILE_", [projectFileName], VariableType.STRING);
        assignVariable("_PRO_FILE_PWD_", [dirName(projectFileName)], VariableType.STRING);
    }

    public bool isVariableDefined(in string name) const
    {
        return isBuiltinVariable(name) || isUserDefinedVariable(name);
    }

    public bool isFunctionDefined(in string name) const
    {
        return isUserDefinedTestFunction(name) || isUserDefinedReplaceFunction(name);
    }

    private void getVariableDescription(in string name, ref ProVariable var)
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{		
		if (isBuiltinVariable(name))
            var = m_builtinVariables[name];
        else if (isUserDefinedVariable(name))
            var = m_userVariables[name];
        else
            throw new Exception("Undefined variable '" ~ name ~ "'");
    }

    private void getTestFunctionDescription(in string name, ref ProFunction func)
    in
    {
        assert(!name.empty, "project user-defined test function name cannot be empty");
    }
    do
    {
        if (isUserDefinedTestFunction(name))
            func = m_userTestFunctions[name];
        else
            throw new Exception("Undefined test function '" ~ name ~ "'");
    }

    private void getReplaceFunctionDescription(in string name, ref ProFunction func)
    in
    {
        assert(!name.empty, "project user-defined replace function name cannot be empty");
    }
    do
    {
        if (isUserDefinedReplaceFunction(name))
            func = m_userReplaceFunctions[name];
        else
            throw new Exception("Undefined replace function '" ~ name ~ "'");
    }

    private bool addUserVariableDescription(in string name, in VariableType type = VariableType.STRING_LIST)
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        if (isBuiltinVariable(name) || isUserDefinedVariable(name))
		{
			throw new Exception("variable '" ~ name ~ "' was already defined");
			//warning("variable '" ~ name ~ "' was already defined");
            //return false;
		}

		// FIXME: add other fields
        m_userVariables[name] = ProVariable(name, type, [], []);
		return true;
    }

    public bool addUserTestFunction(in string name, ref ParseTree codeBlock)
    in
    {
        assert(!name.empty);
        assert(!isUserDefinedTestFunction(name) && !isUserDefinedReplaceFunction(name));
    }
    do
    {
        // this(in string name, in VariableType returnType,
        // in bool isVariadic, in int requiredArgumentCount, in int optionalArgumentCount,
        // VariableType[] argumentTypes, Action action)
        //
        // private alias Action = const(string[]) function(ref ProExecutionContext context, in string[] arguments);
        //
        // private void evalBlock(ref ProExecutionContext context, ref ParseTree bodyNode)

        // FIXME: determine it using `return` statements parsing
        VariableType returnType = VariableType.STRING_LIST;
        
        m_userTestFunctions[name] = new ProFunction(name, returnType, true, -1, -1, [], 
            (ref ProExecutionContext context, in string[] arguments) { return ["true"]; }
         );
        return true;
    }

    private string[] getBuiltinVariableDefaultRawValue(in string name)
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
		assert((name in builtinVariables) !is null);
	}
	do
	{
        return builtinVariables[name].value.dup;
    }

	// FIXME: add const
    public string[] getVariableRawValue(in string name) /+const+/
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        ProVariable variableDescription;
		getVariableDescription(name, variableDescription);
        return variableDescription.value;
    }

	// FIXME: add const
    private string getVariableValue(in string name) /+const+/
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        ProVariable variableDescription;
		getVariableDescription(name, variableDescription);
        switch (variableDescription.type)
		{
            case VariableType.STRING:
            case VariableType.RESTRICTED_STRING:
                if (variableDescription.value.empty)
                {
                    return [];
                }
                return variableDescription.value[0];
            case VariableType.STRING_LIST:
            case VariableType.RESTRICTED_STRING_LIST:
                return variableDescription.value.join(" ");
            default:
			{
                throw new Exception("Unsupported variable type '" ~ to!string(variableDescription.type) ~ "'");
            }
        }
    }
	
	private void setVariableValue(in string name, in string[] value)
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        if (isBuiltinVariable(name))
            m_builtinVariables[name].value = value.dup;
        else if (isUserDefinedVariable(name))
            m_userVariables[name].value = value.dup;
        else
            throw new Exception("Undefined variable '" ~ name ~ "'");
    }

    public void unsetVariable(in string name)
    in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
    {
        if (isBuiltinVariable(name))
            m_builtinVariables.remove(name);
        else if (isUserDefinedVariable(name))
            m_userVariables.remove(name);
        else
            throw new Exception("Undefined variable '" ~ name ~ "'");
    }

    // var = value
    public void assignVariable(in string name, in string[] value, in VariableType variableType)
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        if (!isBuiltinVariable(name) && !isUserDefinedVariable(name))
            addUserVariableDescription(name, variableType);

        validateAssignmentOperands(name, value);
		setVariableValue(name, value);
    }

    // var += value
    public void appendAssignVariable(in string name, in string[] value)
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        if (!isBuiltinVariable(name) && !isUserDefinedVariable(name))
            addUserVariableDescription(name);

        validateAssignmentOperands(name, value);
		setVariableValue(name, getVariableRawValue(name) ~ value);
    }

    // var *= value
    public void appendUniqueAssignVariable(in string name, in string[] value)
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        if (!isBuiltinVariable(name) && !isUserDefinedVariable(name))
            addUserVariableDescription(name);

        validateAssignmentOperands(name, value);

        string[] currentValue = getVariableRawValue(name);
        for (int i = 0; i < value.length; i++)
		{
            if (currentValue.countUntil(value[i]) < 0)
                currentValue ~= value[i];
        }
		setVariableValue(name, currentValue);
    }

    // var -= value
    public void removeAssignVariable(in string name, in string[] value)
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        if (!isBuiltinVariable(name) && !isUserDefinedVariable(name))
            throw new Exception("Variable '" ~ name ~ "' must be defined before usage of the '-=' operator");

        validateAssignmentOperands(name, value);

        // Search for value in the array and remove all occurences
        string[] currentValue = getVariableRawValue(name);
        for (int i = 0; i < value.length; i++)
		{
			auto index = currentValue.countUntil(value[i]);
			if (index >= 0)
				currentValue = currentValue.remove(i);
        }
		setVariableValue(name, currentValue);
    }

    private void validateAssignmentOperands(in string name, in string[] value)
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
	}
	do
	{
        ProVariable variableDescription;
		getVariableDescription(name, variableDescription);
        switch (variableDescription.type)
		{
            case VariableType.RESTRICTED_STRING: {
                if (value.length != 1)
                    throw new Exception("variable '" ~ name ~ " assignment value must be a single string token, not a list");

				// FIXME: implement
//                if (!variableDescription.canBeEmpty && !value[0].length)
//                    throw new Exception("variable '" ~ name ~ "' can not have empty value");

                if (variableDescription.valueRange.countUntil(value[0]) < 0)
                    throw new Exception("variable '" ~ name ~ "' assignment value must be one of the strings: " ~ to!string(variableDescription.valueRange));

                break;
            }
            case VariableType.RESTRICTED_STRING_LIST: {
				// FIXME: implement
//                if (!variableDescription.canBeEmpty && value.empty)
//                    throw new Exception("variable " + name + " can not have empty value");

                for (int i = 0; i < value.length; i++)
				{
                    if (variableDescription.valueRange.countUntil(value[i]) < 0)
                        throw new Exception(name ~ " assignment rvalue must be one of the strings: " ~ to!string(variableDescription.valueRange));
                }

                break;
            }
            case VariableType.STRING: {
				// FIXME: implement
                // NOTE: currently all rvalues in PEG grammar stored as list for convenience
                //if (!typeUtils.isString(value) && !typeUtils.isArray(value))
                //    throw new Exception(name + " assignment value type mismatch: '" + typeUtils.typeOf(value) + "' but string expected");

                break;
            }
            case VariableType.STRING_LIST: {
                break;
            }
            case VariableType.OBJECT_LIST: {
                // FIXME: implement
                break;
            }
            default: {
                throw new Exception("Unsupported variable type " ~ to!string(variableDescription.type));
            }
        }
    }

    private enum ExpandableType
    {
        Invalid = -1,
        MakefileVariable_1, MakefileVariable_2,
        ProjectVariable_1, ProjectVariable_2,
        EnvironmentVariable,
        PersistentProperty,
        Leftover,
        Count
    }

    /*private*/ struct Expandable
    {
        ExpandableType type = ExpandableType.Invalid;
        long from = -1;
        long length = -1;
        string source;
        const string[] result;
        bool isDefined;

        this(in ExpandableType type, in long from, in long length, in string source, in string[] result, in bool isDefined)
        {
            this.type      = type;
            this.from      = from;
            this.length    = length;
            this.source    = source;
            this.result    = result;
            this.isDefined = isDefined;
        }
    }

    private alias VariableInfo = Tuple!(string, "name", long, "from", long, "to");
    private alias VariableSourcePosition = Tuple!(long, "prefixLength", long, "postfixLength", ExpandableType, "type");

    private VariableInfo extractVariable(in string str, in long from, in long after)
    {
        VariableInfo result;

        // First variable char must be letter or underscore
        if (!isAlphascore(str[from]))
            return result;

        result.from = from;
        result.name ~= str[from];

        // Subsequent chars can be also digits
        long j;
        for (j = from + 1; j < str.length; j++)
        {
            if (!isAlphaNum(str[j]) && !isUnderscore(str[j]))
                break;

            result.name ~= str[j];
        }
        result.to = j;
        result.to += after;

        return result;
    }

    private VariableSourcePosition recognizeVariable(in string twoTokens, in string threeTokens)
    {
        // makefile-time project variable 1: $VAR
        long prefixLength, postfixLength;
        ExpandableType variableType = ExpandableType.Invalid;

        if (isAlpha(threeTokens[1]))
        {
            variableType = ExpandableType.MakefileVariable_1;
            prefixLength = 1;
            postfixLength = 0;
        }
        // makefile-time project variable 2: ${VAR}
        else if (twoTokens == STR_GENERATOR_EXPAND_MARKER)
        {
            variableType = ExpandableType.MakefileVariable_2;
            prefixLength = 2;
            postfixLength = 1;
        }
        // project variable 1: $$VAR
        else if ((twoTokens == STR_EXPAND_MARKER) && (threeTokens.length == 3) && isAlpha(threeTokens[2]))
        {
            variableType = ExpandableType.ProjectVariable_1;
            prefixLength = 2;
            postfixLength = 0;
        }
        // project variable 2: $${VAR}
        else if (threeTokens == STR_VARIABLE_EXPAND_MARKER)
        {
            variableType = ExpandableType.ProjectVariable_2;
            prefixLength = 3;
            postfixLength = 1;
        }
        // environment variable: $$(VAR)
        else if (threeTokens == STR_ENV_VARIABLE_EXPAND_MARKER)
        {
            variableType = ExpandableType.EnvironmentVariable;
            prefixLength = 3;
            postfixLength = 1;
        }
        // persistent property: $$[VAR] or $$[VAR/get]
        else if (threeTokens == STR_PROPERTY_EXPAND_MARKER)
        {
            variableType = ExpandableType.PersistentProperty;
            prefixLength = 3;
            postfixLength = 1;
        }
        else
            throw new Exception("Syntax error: unsupported expand expression! '$' must be escaped");
        
        return VariableSourcePosition(prefixLength, postfixLength, variableType);
    }

    private Expandable[] findExpandables(in string str, ref PersistentPropertyStorage storage, ref bool hasLeftovers)
    {
        assert(STR_GENERATOR_EXPAND_MARKER.length == 2);
        assert(STR_EXPAND_MARKER.length == 2);
        assert(STR_VARIABLE_EXPAND_MARKER.length == 3);
        assert(STR_VARIABLE_EXPAND_MARKER.length == 3);

        Expandable[] result;
        string leftover;
        long i = 0;
        while (i < str.length)
        {
            immutable auto twoTokens = joinTokens(str, i, 2);
            immutable auto threeTokens = joinTokens(str, i, 3);
            assert(threeTokens.length == 1 || threeTokens.length == 2 || threeTokens.length == 3);

            // Single/double quotes must already be eliminated on this stage of parsing
            // FIXME: not all :(
//            if (containsQuote(threeTokens))
//                throw new EvalLogicException("expandVariable function requires string without single/double quotes");

            // generator expression: $VAR, ${VAR}
            // project variable: $$VAR, $${VAR}
            // environment variable: $$(VAR)
            // persistent property: $$[VAR]
            //
            // Minimal expandable expression example: $V
            if ((threeTokens[0] != CHAR_SINGLE_EXPAND_MARKER) || (threeTokens.length < 2))
            {
                leftover ~= str[i];
                i++;

                continue;
            }

            if (!leftover.empty)
            {
                hasLeftovers = true;
                result ~= Expandable(ExpandableType.Leftover, /*variableInfo.from*/ -1, /*expandableLength*/ 0, str, [leftover], false);
                leftover = "";
            }

            immutable VariableSourcePosition vsp = recognizeVariable(twoTokens, threeTokens);
            immutable auto variableInfo = extractVariable(str, i + vsp.prefixLength, vsp.postfixLength);
            
            long expandableLength = (variableInfo.to - variableInfo.from);

            string[] variableValue;
            bool isDefined;
            switch (vsp.type)
            {
                case ExpandableType.MakefileVariable_1:
                case ExpandableType.MakefileVariable_2:
                {
                    // FIXME: implement expand
                    isDefined = false;
                    variableValue ~= str[variableInfo.from .. variableInfo.to];

                    writeln("Makefile-time project variable 1: ", variableInfo.name);
                    writeln("Value: '", variableValue, "'");
                    break;
                }
                case ExpandableType.ProjectVariable_1:
                case ExpandableType.ProjectVariable_2:
                {
                    if (!isVariableDefined(variableInfo.name))
                    {
                        isDefined = false;
                        error("Expand undefined variable '", variableInfo.name, "' to empty string");
                    }
                    else
                    {
                        isDefined = true;
                        string[] rawValue = getVariableRawValue(variableInfo.name);
                        if (!rawValue.empty)
                            variableValue ~= rawValue;

                        writeln("Project variable: ", variableInfo.name);
                        writeln("Value: '", rawValue, "'");                        
                    }

                    break;
                }
                case ExpandableType.EnvironmentVariable:
                {
                    auto value = environment.get(variableInfo.name);
                    if (value is null)
                    {
                        isDefined = false;
                        error("Expand undefined environment variable '", variableInfo.name, "' to empty string");
                    }
                    else
                    {
                        isDefined = true;
                        if (!value.empty)
                            variableValue ~= value;

                        writeln("Environment variable: ", variableInfo.name);
                        writeln("Value: '", value, "'");                        
                    }

                    break;
                }
                case ExpandableType.PersistentProperty:
                {
                    immutable string getSuffix = joinTokens(str, variableInfo.to, 4);
                    if (getSuffix == STR_PROPERTY_GET_SUFFIX)
                        expandableLength = variableInfo.to + getSuffix.length + 1;
                    else
                        expandableLength = variableInfo.to + 1;

// FIXME: test
                    string propertyName = variableInfo.name;
                    string propertyValue;
                    if (!storage.hasValue(propertyName))
                    {
                        isDefined = false;
                        throw new Exception("Undefined persistent property '" ~ propertyName ~ "'");
                    }

                    isDefined = true;
                    propertyValue = storage.value(propertyName);

                    writeln("Persistent property (project-time): ", propertyName);
                    writeln("Value: '", propertyValue, "'");
                    if (!propertyValue.empty)
                        variableValue ~= propertyValue;

                    break;
                }
                default:
                {
                    throw new Exception("Unsupported expandable entity type");
                }
            }

            result ~= Expandable(vsp.type, variableInfo.from, expandableLength, str, variableValue, isDefined);

            i += vsp.prefixLength;
            i += expandableLength;
        }

        if (!leftover.empty)
        {            
            hasLeftovers = true;
            result ~= Expandable(ExpandableType.Leftover, /*variableInfo.from*/ -1, /*expandableLength*/ 0, str, [leftover], false);
            leftover = "";
        }

        return result;
    }

    /**
     * Expand all kind of variables in the string.
     * Params:
     *      persistentStorage = qmake persistent storage object reference
     *      strSource = expression string to be expanded
     * Returns: strSource with all project/environment variables and persistent properties expanded
     *          (i.e. replaced with their actual values)
     */
    public string[] expandAllVariables(ref PersistentPropertyStorage persistentStorage, in string strSource)
	{
        // Naive optimization
        if (strSource.empty)
            return [strSource];

        string[] result;
        bool hasLeftovers;
        Expandable[] expandables = findExpandables(strSource, persistentStorage, hasLeftovers);

        if (hasLeftovers)
        {
            // NOTE: if there is at least one leftover in the expandable source string,
            //       then the result will be a string instead of list
            string temp;
            foreach (e; expandables)
            {
//                writeln("Expandable 1:\n", e, "\n");

                temp ~= e.result.join(STR_EMPTY);
            }
            result ~= temp;
        }
        else
        {
            foreach (e; expandables)
            {
//                writeln("Expandable: 2\n", e, "\n");

                result ~= e.result;
            }
        }
        return result;

        /*
        string[] result;
        foreach (token; tokens)
        {
            if (token.canFind(CHAR_SINGLE_EXPAND_MARKER))
            {
                string[] temp = expandVariable(persistentStorage, token);
                writeln("Expanded token: ", temp);
                result ~= temp;
            }
            else
            {
                writeln("Constant token: ", token);
                result ~= token;
            }
        }

        return result;
        */

        /*uint i;
        while (i < strSource.length)
        {
            auto token = joinTokens(str, i, 1);
            auto twoTokens = joinTokens(str, i, 2);

            switch (token)
            {
                // generator expression: $VAR, ${VAR}, ??? $(VAR), $[VAR]
                case STR_SINGLE_EXPAND_MARKER:
                // project variable: $${VAR}, $$VAR
                // environment variable: $$(VAR)
                // persistent property: $$[VAR]
            }
        }*/

/*
		auto replaceProVarFunc(Captures!string captures)
		{
			string variableName = captures["name"];
			if (variableName.empty)
				throw new Exception("variable name cannot be empty");
            
            trace("Expanding variable '", variableName, "'...");
            if (!isVariableDefined(variableName))
            {
                warning("Expand undefined variable '", variableName, "' to empty string");
                return "";
            }

            string[] rawValue = getVariableRawValue(variableName);
            string prettyValue = getVariableValue(variableName);

            trace("Variable pretty value: ", prettyValue);
            trace("Variable raw value: ", rawValue);

            result ~= rawValue;

			return getVariableValue(variableName);
		}

        auto replaceEnvVarFunc(Captures!string captures)
        {
            string environmentVariableName = captures["name"];
			if (environmentVariableName.empty)
				throw new Exception("environment variable name cannot be empty");
            
            trace("Expanding environment variable '", environmentVariableName, "'...");

            auto value = environment.get(environmentVariableName);
            if (value is null)
            {
                error("Expand undefined environment variable '", environmentVariableName, "' to empty string");
                return "";
            }

            trace("Environment variable value: ", value);
            return value;
        }

        auto replacePropertyFunc(Captures!string captures)
        {
            string propertyName = captures["name"];
			if (propertyName.empty)
				throw new Exception("qmake persistent property name cannot be empty");
            
            trace("Expanding property '", propertyName, "'...");
            
            string propertyValue;
            if (!persistentStorage.hasValue(propertyName))
            {
                throw new Exception("Undefined persistent property '" ~ propertyName ~ "'");
            }

            propertyValue = persistentStorage.value(propertyName);
            trace("Property value: ", propertyValue);
			return propertyValue;
        }
		
        string strExpanded = strSource.dup;

		strExpanded = replaceAll!replaceProVarFunc(strExpanded, regex(projectVariableExpansionRegex_1, "g"));
        strExpanded = replaceAll!replaceProVarFunc(strExpanded, regex(projectVariableExpansionRegex_2, "g"));
        strExpanded = replaceAll!replaceEnvVarFunc(strExpanded, regex(environmentVariableExpansionRegex_2, "g"));
        strExpanded = replaceAll!replaceEnvVarFunc(strExpanded, regex(environmentVariableExpansionRegex_1, "g"));
        strExpanded = replaceAll!replacePropertyFunc(strExpanded, regex(qmakePropertyExpansionRegex_1, "g"));
        strExpanded = replaceAll!replacePropertyFunc(strExpanded, regex(qmakePropertyExpansionRegex_2, "g"));

        writeln("RESULT: ", result);

        return strExpanded;*/
    }
}
