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
import std.process;

import project_variable;

// -------------------------------------------------------------------------------------------------

//var builtinVariablesModule = require("./builtin_variable_description");
//var persistentStorage = require("./persistent_property_storage");
//const VariableTypeEnum = builtinVariablesModule.VariableTypeEnum;

// -------------------------------------------------------------------------------------------------

// 1) OUTPUT_LIB = $${LIB_NAME}
//var projectVariableExpansionRegex_1 = /\$\$\{([_a-zA-Z][_a-zA-Z0-9]*)+\}/g;
private const auto projectVariableExpansionRegex_1 = r"\$\$\{(?P<name>([_a-zA-Z][_a-zA-Z0-9]*)+)\}";
// 2) OUTPUT_LIB = $$LIB_NAME
//var projectVariableExpansionRegex_2 = /\$\$([_a-zA-Z][_a-zA-Z0-9]*)+\b/g;
private const auto projectVariableExpansionRegex_2 = r"\$\$(?P<name>([_a-zA-Z][_a-zA-Z0-9]*)+)\b";
// 3) DESTDIR = $(PWD)
//var environmentVariableExpansionRegex_1 = /\$\(([_a-zA-Z][_a-zA-Z0-9]*)+\)/g;
private const auto environmentVariableExpansionRegex_1 = r"\$\(([_a-zA-Z][_a-zA-Z0-9]*)+\)";
// 4) DESTDIR = $$(PWD)
//var environmentVariableExpansionRegex_2 = /\$\$\(([_a-zA-Z][_a-zA-Z0-9]*)+\)/g;
private const auto environmentVariableExpansionRegex_2 = r"\$\$\(([_a-zA-Z][_a-zA-Z0-9]*)+\)";
// 5) target.path = $$[QT_INSTALL_PLUGINS]/designer
//var qmakePropertyExpansionRegex_1 = /\$\$\[([_a-zA-Z][_a-zA-Z0-9]*)+\]/g;
//var qmakePropertyExpansionRegex_2 = /\$\$\[([_a-zA-Z][_a-zA-Z0-9]*)+\/get\]/g;
private const auto qmakePropertyExpansionRegex_1 = r"\$\$\[([_a-zA-Z][_a-zA-Z0-9]*)+\]";
private const auto qmakePropertyExpansionRegex_2 = r"\$\$\[([_a-zA-Z][_a-zA-Z0-9]*)+\/get\]";

// -------------------------------------------------------------------------------------------------


// FIXME: add qmake persistent storage
// FIXME: add environment variables
public class ProExecutionContext
{
	private ProVariable[string] m_builtinVariables;
	private ProVariable[string] m_userVariables;
	
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

    public string[] getBuiltinVariableNames() const
    {
        return m_builtinVariables.keys.sort.release();
    }
    public string[] getUserDefinedVariableNames() const
    {
        return m_userVariables.keys.sort.release();
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

    public string expandVariables(in string strSource)
	{
        if (strSource.empty)
            return strSource;
/+
        auto replaceProVarFunc = (getVariableValue, match, variableName, offset, string) {
            assert.isString(variableName);
            assert.isNotEmpty(variableName);
            assert.isFunction(getVariableValue);
            return this.getVariableValue(variableName);
        }

        auto replaceEnvVarFunc = function(match, variableName, offset, string) {
            assert.isString(variableName);
            assert.isNotEmpty(variableName);

            return (process.env[variableName] !== undefined) ? process.env[variableName] : "";
        }

        auto replacePropertyFunc = function(match, variableName, offset, string) {
            assert.isString(variableName);
            assert.isNotEmpty(variableName);

            return persistentStorage.query(variableName);
        }
+/
        string strExpanded = strSource.dup;
		
		auto replaceProVarFunc(Captures!string captures)
		{
			string variableName = captures["name"];
			if (variableName.empty)
				throw new Exception("variable name cannot be empty");

			return getVariableValue(variableName);
		}
		
		strExpanded = replaceAll!replaceProVarFunc(strExpanded, regex(projectVariableExpansionRegex_1, "g"));
        strExpanded = replaceAll!replaceProVarFunc(strExpanded, regex(projectVariableExpansionRegex_2, "g"));
		// FIXME: implement
//        strExpanded = replaceAll(strExpanded, environmentVariableExpansionRegex_2.regex, environment.get("$1", ""));
//        strExpanded = replaceAll(strExpanded, environmentVariableExpansionRegex_1.regex, environment.get("$1", ""));
//        strExpanded = strExpanded.replace(qmakePropertyExpansionRegex_1.regex, replacePropertyFunc);
//        strExpanded = strExpanded.replace(qmakePropertyExpansionRegex_2.regex, replacePropertyFunc);

        return strExpanded;
    }
}
