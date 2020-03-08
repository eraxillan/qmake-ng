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

module source.project_context;

import std.experimental.logger;

import std.algorithm;
import std.conv;
import std.stdio;
import std.path;
import std.string;

import source.common_const;
import source.text_utils;
import source.logger;
import source.qmakeexception;
import source.project_variable;
import source.project_function;

// -------------------------------------------------------------------------------------------------

enum FlowControlStatement { None = -1, Return, Break, Next, Error, Count }

alias ProjectContextStack = QStack!ProExecutionContext;

class ProExecutionContext
{
private:
    /// Variables
	ProVariable[string] m_builtinVariables;
	ProVariable[string] m_userVariables;

    /// Functions
    ProFunction[string] m_builtinReplaceFunctions;
    ProFunction[string] m_builtinTestFunctions;
    ProFunction[string] m_userReplaceFunctions;
    ProFunction[string] m_userTestFunctions;

    /// Replace function result storage
    string[] m_functionResult;

    /// Current "program" flow control
    FlowControlStatement m_flowControlStatement = FlowControlStatement.None;

private:
	ProVariable[string] cloneBuiltinVariables() const
	{
		ProVariable[string] result;
		foreach (name; builtinVariables.keys)
			result[name] = builtinVariables[name].dup();
		return result;
	}

    ProFunction[string] cloneBuiltinReplaceFunctions() const
    {
		ProFunction[string] result;
		foreach (name; builtinReplaceFunctions.keys)
			result[name] = builtinReplaceFunctions[name].dup();
		return result;
	}

    ProFunction[string] cloneBuiltinTestFunctions() const
    {
		ProFunction[string] result;
		foreach (name; builtinTestFunctions.keys)
			result[name] = builtinTestFunctions[name].dup();
		return result;
	}

    bool isBuiltinVariable(const string name) const
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
    }
    do
    {
        return (name in m_builtinVariables) !is null;
    }

    bool isUserDefinedVariable(const string name) const
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
    }
    do
    {
        return (name in m_userVariables) !is null;
    }

    void getVariableDescription(const string name, ref ProVariable var)
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

    bool addUserVariableDescription(const string name, const VariableType type = VariableType.STRING_LIST)
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

    void setVariableValue(const string name, const string[] value)
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
    }
    do
    {
        string[] clearValue;
        foreach (v; value)
        {
            if (!v.empty)
                clearValue ~= v;
        }

        if (isBuiltinVariable(name))
        {
            trace("Set built-in variable ", "`", name, "`", " value to ", clearValue);
            m_builtinVariables[name].value = clearValue.dup;

            NgLogger.get().traceProjectVariableAssignment(name, value);
        }
        else if (isUserDefinedVariable(name))
        {
            trace("Set user-defined variable ", "`", name, "`", " value to ", clearValue);
            m_userVariables[name].value = clearValue.dup;
        }
        else
            throw new Exception("Undefined variable '" ~ name ~ "'");
    }

    void validateAssignmentOperands(const string name, const string[] value)
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

    // ---------------------------------------------------------------------------------------------------------------------------

    bool isBuiltinReplaceFunction(const string name)
    {
        return ((name in m_builtinReplaceFunctions) !is null);
    }

    bool isUserReplaceFunction(const string name)
    {
        return ((name in m_userReplaceFunctions) !is null);
    }

    bool isBuiltinTestFunction(const string name)
    {
        return ((name in m_builtinTestFunctions) !is null);
    }

    bool isUserTestFunction(const string name)
    {
        return ((name in m_userTestFunctions) !is null);
    }

    // ---------------------------------------------------------------------------------------------------------------------------

public:
    this()
	{
		m_builtinVariables = cloneBuiltinVariables();
        m_builtinReplaceFunctions = cloneBuiltinReplaceFunctions();
        m_builtinTestFunctions = cloneBuiltinTestFunctions();
    }

    void reset()
	{
        m_builtinVariables = cloneBuiltinVariables();
        m_builtinReplaceFunctions = cloneBuiltinReplaceFunctions();
        m_builtinTestFunctions = cloneBuiltinTestFunctions();

        m_userVariables.clear;
        m_userReplaceFunctions.clear;
        m_userTestFunctions.clear;
    }

    string[] getBuiltinVariableNames() const
    {
        return m_builtinVariables.keys.sort.release();
    }

    string[] getUserDefinedVariableNames() const
    {
        return m_userVariables.keys.sort.release();
    }

    void setupPaths(const string projectFileName)
    {
        string dir = dirName(projectFileName);
        assignVariable("PWD", [dir], VariableType.STRING);
        assignVariable("OUT_PWD", [dir], VariableType.STRING);
        assignVariable("_PRO_FILE_", [projectFileName], VariableType.STRING);
        assignVariable("_PRO_FILE_PWD_", [dir], VariableType.STRING);
    }

    // FIXME: workaround needed until cache file `.qmake.stash` support code will be implemented
    // Also, those values are valid for Linux+gcc platform only
    void setupCacheVariables()
    {
        /*assignVariable("QMAKE_CXX.INCDIRS",
            [
            "/usr/include/c++/5", " /usr/include/x86_64-linux-gnu/c++/5",
            "/usr/include/c++/5/backward", "/usr/lib/gcc/x86_64-linux-gnu/5/include",
            "/usr/local/include", "/usr/lib/gcc/x86_64-linux-gnu/5/include-fixed",
            "/usr/include/x86_64-linux-gnu", "/usr/include"
            ],
            VariableType.STRING_LIST);
        assignVariable("QMAKE_CXX.LIBDIRS",
            [
                "/usr/lib/gcc/x86_64-linux-gnu/5", "/usr/lib/x86_64-linux-gnu",
                "/usr/lib", "/lib/x86_64-linux-gnu", " /lib"
            ],
            VariableType.STRING_LIST);
        assignVariable("QMAKE_CXX.QT_COMPILER_STDCXX", ["199711L"], VariableType.STRING);
        assignVariable("QMAKE_CXX.QMAKE_GCC_MAJOR_VERSION", ["5"], VariableType.STRING);
        assignVariable("QMAKE_CXX.QMAKE_GCC_MINOR_VERSION", ["4"], VariableType.STRING);
        assignVariable("QMAKE_CXX.QMAKE_GCC_PATCH_VERSION", ["0"], VariableType.STRING);
        assignVariable("QMAKE_CXX.COMPILER_MACROS",
            [
                "QT_COMPILER_STDCXX",
                "QMAKE_GCC_MAJOR_VERSION",
                "QMAKE_GCC_MINOR_VERSION",
                "QMAKE_GCC_PATCH_VERSION"
            ],
            VariableType.STRING_LIST);*/
    }

    bool isVariableDefined(const string name) const
    {
        return isBuiltinVariable(name) || isUserDefinedVariable(name);
    }

    bool isVariableValueEmpty(const string name) /+const+/
    {
        if (!isVariableDefined(name))
            return true;

        string[] valueList = getVariableRawValue(name);
        return valueList.empty || (valueList.length == 1 && valueList[0].empty);
    }

    VariableType getVariableType(const string name) /+const+/
    in
	{
		assert(!name.empty, "project variable name cannot be empty");
        assert((isBuiltinVariable(name) || isUserDefinedVariable(name)));
	}
	do
	{
        ProVariable variableDescription;
		getVariableDescription(name, variableDescription);
        return variableDescription.type;
    }

	// FIXME: add const
    string[] getVariableRawValue(const string name) /+const+/
	in
	{
		assert(!name.empty, "project variable name cannot be empty");
        assert((isBuiltinVariable(name) || isUserDefinedVariable(name)));
	}
	do
	{
        ProVariable variableDescription;
		getVariableDescription(name, variableDescription);
        return variableDescription.value;
    }

    void unsetVariable(const string name)
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
    void assignVariable(const string name, const string[] value, const VariableType variableType)
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
    void appendAssignVariable(const string name, const string[] value)
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
    void appendUniqueAssignVariable(const string name, const string[] value)
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
    void removeAssignVariable(const string name, const string[] value)
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

	bool hasReplaceFunctionDescription(const string name)
	in
	{
		assert(!name.empty, "function name cannot be empty");
        assert(!m_builtinReplaceFunctions.empty);
	}
	do
	{
		return isBuiltinReplaceFunction(name) || isUserReplaceFunction(name);
	}

	ProFunction getReplaceFunctionDescription(const string name)
	in
	{
		assert(!name.empty, "function name cannot be empty");
        assert(!m_builtinReplaceFunctions.empty);
	}
	do
	{
        if (isBuiltinReplaceFunction(name))
		    return m_builtinReplaceFunctions[name];
        else if (isUserReplaceFunction(name))
            return m_userReplaceFunctions[name];
        else
            throw new NotImplementedException("replace function " ~ "`" ~ name ~ "`" ~ " was not defined yet");
	}

    bool hasTestFunctionDescription(const string name)
	in
	{
		assert(!name.empty, "function name cannot be empty");
        assert(!m_builtinTestFunctions.empty);
	}
	do
	{
		return isBuiltinTestFunction(name) || isUserTestFunction(name);
	}

	ProFunction getTestFunctionDescription(const string name)
	in
	{
		assert(!name.empty, "function name cannot be empty");
        assert(!m_builtinTestFunctions.empty);
	}
	do
	{
		if (isBuiltinTestFunction(name))
		    return m_builtinTestFunctions[name];
        else if (isUserTestFunction(name))
            return m_userTestFunctions[name];
        else
            throw new NotImplementedException("test function " ~ "`" ~ name ~ "`" ~ " was not defined yet");
	}

    // ---------------------------------------------------------------------------------------------------------------------------
    // NOTE: all user-defined functions are variadic

    void addReplaceFunctionDescription(const string name, const ProFunction.Action action)
    {
        m_userReplaceFunctions[name] = ProFunction(
            FunctionBaseInfo(name, -1, -1),
            FunctionTypeInfo(true, [VariableType.STRING], VariableType.STRING_LIST),
            action
        );
    }

    void addTestFunctionDescription(const string name, ProFunction.Action action)
    {
        m_userTestFunctions[name] = ProFunction(
            FunctionBaseInfo(name, -1, -1),
            FunctionTypeInfo(true, [VariableType.STRING], VariableType.STRING_LIST),
            action
        );
    }

    // ---------------------------------------------------------------------------------------------------------------------------

    void pushFunctionResult(const string[] value)
    {
        m_functionResult = value.dup;
    }

    bool hasFunctionResult() const
    {
        return !m_functionResult.empty;
    }

    string[] popFunctionResult()
    {
        string[] temp = m_functionResult.dup;
        m_functionResult = [];
        return temp;
    }
    
    bool hasFlowControlStatement() const
    {
        return m_flowControlStatement != FlowControlStatement.None;
    }

    auto getFlowControlStatement() const
    {
        return m_flowControlStatement;
    }

    void setFlowControlStatement(const FlowControlStatement fcs)
    {
        m_flowControlStatement = fcs;
    }
}
