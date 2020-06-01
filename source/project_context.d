/****************************************************************************
**
** Copyright (C) 2018, 2019, 2020 Alexander Kamyshnikov
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
import source.utils.text_utils;
import source.logger;
import source.qmakeexception;
import source.project_variable;
import source.project_function;

// ---------------------------------------------------------------------------------------------------------------------

/**
  Type of build script flow control statement
*/
enum FlowControlStatement { None = -1, Return, Break, Next, Error, Count }

alias ProjectContextStack = QStack!ProExecutionContext;

/**
  Build script context: built-in and user defined variables and function
*/
class ProExecutionContext
{
public:
    /// Build script context compare result item
    struct ProVariableDiff
    {
        string name;     /// Variable name
        string[] value;  /// Variable value
        string action;   /// Type of change (add/remove/change)
    }

    /// ctor
    this()
    {
        m_builtinVariables = cloneBuiltinVariables();
        m_builtinReplaceFunctions = cloneBuiltinReplaceFunctions();
        m_builtinTestFunctions = cloneBuiltinTestFunctions();
    }

    /// Create an independent copy of build script context
    @property ProExecutionContext dup()
    {
        auto result = new ProExecutionContext();

        result.m_builtinVariables = this.m_builtinVariables.dup;
        result.m_userVariables = this.m_userVariables.dup;
        result.m_builtinReplaceFunctions = this.m_builtinReplaceFunctions.dup;
        result.m_userReplaceFunctions = this.m_userReplaceFunctions.dup;
        result.m_builtinTestFunctions = this.m_builtinTestFunctions.dup;
        result.m_userTestFunctions = this.m_userTestFunctions.dup;

        result.m_functionResult = this.m_functionResult.dup;
        result.m_flowControlStatement = this.m_flowControlStatement;

        return result;
    }

private:
    /// Variables
    ProVariable[string] m_builtinVariables, m_builtinVariablesBackup;
    ProVariable[string] m_userVariables, m_userVariablesBackup;

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

private:
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

    bool isInternalVariable(const string name) const
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
    }
    do
    {
        const auto internalVariablesName = ["_PRO_FILE_", "PWD", "_PRO_FILE_PWD_", "OUT_PWD"];
        return (internalVariablesName.countUntil(name) != -1);
    }

private:
    void getVariableDescription(const string name, ref ProVariable var) const
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
        assert(isBuiltinVariable(name) || isUserDefinedVariable(name));
    }
    do
    {
        // TODO: implement default invalid ctor on ProVariable
        var.type = VariableType.UNKNOWN;

        if (isBuiltinVariable(name))
            var = m_builtinVariables[name].dup;
        else if (isUserDefinedVariable(name))
            var = m_userVariables[name].dup;
    }

    void addUserVariableDescription(const string name, const VariableType type = VariableType.STRING_LIST)
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
        assert(!isBuiltinVariable(name) && !isUserDefinedVariable(name));
    }
    out
    {
        assert(isUserDefinedVariable(name));
    }
    do
    {
        // FIXME: implement and add other fields
        m_userVariables[name] = ProVariable(name, type, [], []);
    }

    void setVariableValue(const string name, const string[] value)
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
        assert(isBuiltinVariable(name) || isUserDefinedVariable(name));
    }
    do
    {
        string[] clearValue;
        foreach (v; value)
        {
            if (!v.empty)
                clearValue ~= v;
        }
        if (clearValue.empty)
            clearValue = [""];

        if (isBuiltinVariable(name))
        {
            tracef("Set built-in variable '%s' value to [%s]", name, clearValue.join(STR_WS));
            m_builtinVariables[name].value = clearValue.dup;

            NgLogger.get().traceProjectVariableAssignment(name, value, true);
        }
        else if (isUserDefinedVariable(name))
        {
            tracef("Set user-defined variable '%s' value to [%s]", name, clearValue.join(STR_WS));
            m_userVariables[name].value = clearValue.dup;

            NgLogger.get().traceProjectVariableAssignment(name, value, false);
        }
    }

    void validateAssignmentOperands(const string name, const string[] value) const
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
        assert(isBuiltinVariable(name) || isUserDefinedVariable(name));
    }
    do
    {
        ProVariable variableDescription;
        getVariableDescription(name, variableDescription);
        switch (variableDescription.type)
        {
            case VariableType.BOOLEAN: {
                // FIXME: implement
                break;
            }
            case VariableType.RESTRICTED_STRING: {
                if (value.length != 1)
                    throw new EvalVariableException("variable '" ~ name ~
                        " assignment value must be a single string token, not a list");

                if (variableDescription.valueRange.countUntil(value[0]) < 0)
                    throw new EvalVariableException("variable '" ~ name ~
                        "' assignment value must be one of the strings: " ~
                        to!string(variableDescription.valueRange));

                break;
            }
            case VariableType.RESTRICTED_STRING_LIST: {
                for (int i = 0; i < value.length; i++)
                {
                    if (variableDescription.valueRange.countUntil(value[i]) < 0)
                        throw new EvalVariableException(name ~
                            " assignment rvalue must be one of the strings: " ~
                            to!string(variableDescription.valueRange));
                }

                break;
            }
            case VariableType.STRING: {
                // FIXME: implement
                // NOTE: currently all rvalues in PEG grammar stored as list for convenience
                //if (!typeUtils.isString(value) && !typeUtils.isArray(value))
                //    throw new EvalVariableException(name + " assignment value type mismatch: '" +
                //        typeUtils.typeOf(value) + "' but string expected");

                break;
            }
            case VariableType.STRING_LIST: {
                // Mothing to check in this case
                break;
            }
            case VariableType.OBJECT_LIST: {
                // FIXME: implement
                break;
            }
            default: {
                throw new EvalVariableException("Unsupported variable type " ~ to!string(variableDescription.type));
            }
        }
    }

    // -----------------------------------------------------------------------------------------------------------------
private:
    bool isBuiltinReplaceFunction(const string name) const
    {
        return ((name in m_builtinReplaceFunctions) !is null);
    }

    bool isUserReplaceFunction(const string name) const
    {
        return ((name in m_userReplaceFunctions) !is null);
    }

    bool isBuiltinTestFunction(const string name) const
    {
        return ((name in m_builtinTestFunctions) !is null);
    }

    bool isUserTestFunction(const string name) const
    {
        return ((name in m_userTestFunctions) !is null);
    }

    // -----------------------------------------------------------------------------------------------------------------

public:
    /// Return alphabetically sorted list of built-in variable names
    string[] getBuiltinVariableNames() const
    out(result)
    {
        assert(!result.empty, "built-in variable list cannot be empty");
    }
    do
    {
        return m_builtinVariables.keys.sort.release();
    }

    /// Return alphabetically sorted list of user-defined variable names
    string[] getUserDefinedVariableNames() const
    {
        return m_userVariables.keys.sort.release();
    }

public:
    /// Setup project path variable values: absolute paths to project and directory
    void setupPaths(const string projectFileName)
    {
        string dir = dirName(projectFileName);
        assignVariable("PWD", [dir], VariableType.STRING);
        assignVariable("OUT_PWD", [dir], VariableType.STRING);
        assignVariable("_PRO_FILE_", [projectFileName], VariableType.STRING);
        assignVariable("_PRO_FILE_PWD_", [dir], VariableType.STRING);
    }

    /// Cache compiler-related variables in `.qmake.stash` file
    void setupCacheVariables()
    {
        // FIXME: implement
    }

public:
    /// Check whether specified variable is defined: either as built-in or user-defined one
    bool isVariableDefined(const string name) const
    {
        return isBuiltinVariable(name) || isUserDefinedVariable(name);
    }

    bool isVariableValueEmpty(const string name) const
    {
        if (!isVariableDefined(name))
            return true;

        string[] valueList = getVariableValue(name);
        return valueList.empty || (valueList.length == 1 && valueList[0].empty);
    }

    VariableType getVariableType(const string name) const
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

    // FIXME: replace all getVariableRawValue occurences with getVariableValue and remove it
    /// Return project variable value as raw string list, without type conversion
    string[] getVariableValue(const string name) const { return getVariableRawValue(name); }
    string[] getVariableRawValue(const string name) const
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
        assert(isBuiltinVariable(name) || isUserDefinedVariable(name));
    }
    do
    {
        ProVariable variableDescription;
        getVariableDescription(name, variableDescription);
        return variableDescription.value;
    }

public:
    void unsetVariable(const string name)
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
        assert(isBuiltinVariable(name) || isUserDefinedVariable(name));
    }
    out
    {
        assert(!isBuiltinVariable(name) && !isUserDefinedVariable(name));
    }
    do
    {
        if (isBuiltinVariable(name))
        {
            m_builtinVariables.remove(name);
            NgLogger.get().traceProjectVariableUnset(name, true);
        }
        else if (isUserDefinedVariable(name))
        {
            m_userVariables.remove(name);
            NgLogger.get().traceProjectVariableUnset(name, false);
        }
    }

    // var = value
    void assignVariable(const string name, const string[] value, const VariableType variableType)
    in
    {
        assert(!name.empty, "project variable name cannot be empty");
        assert(variableType != VariableType.UNKNOWN);
    }
    out
    {
        assert(isBuiltinVariable(name) || isUserDefinedVariable(name));
        // NOTE: empty value got one empty string, need to take this into account
        //assert(getVariableValue(name) == value);
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
    out
    {
        assert(isBuiltinVariable(name) || isUserDefinedVariable(name));
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
    out
    {
        assert(isBuiltinVariable(name) || isUserDefinedVariable(name));
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
        assert(isBuiltinVariable(name) || isUserDefinedVariable(name));
    }
    do
    {
        validateAssignmentOperands(name, value);

        // Search for value in the array and remove all occurences
        string[] currentValue = getVariableRawValue(name);
        for (int i = 0; i < value.length; i++)
        {
            immutable auto index = currentValue.countUntil(value[i]);
            if (index >= 0)
                currentValue = currentValue.remove(i);
        }
        setVariableValue(name, currentValue);
    }

public:
    bool hasReplaceFunctionDescription(const string name) const
    in
    {
        assert(!name.empty, "function name cannot be empty");
        assert(!m_builtinReplaceFunctions.empty);
    }
    do
    {
        return isBuiltinReplaceFunction(name) || isUserReplaceFunction(name);
    }

    void getReplaceFunctionDescription(const string name, ref ProFunction func) const
    in
    {
        assert(!name.empty, "function name cannot be empty");
        assert(hasReplaceFunctionDescription(name));
    }
    do
    {
        if (isBuiltinReplaceFunction(name))
            func = m_builtinReplaceFunctions[name].dup;
        else if (isUserReplaceFunction(name))
            func = m_userReplaceFunctions[name].dup;
    }

    bool hasTestFunctionDescription(const string name) const
    in
    {
        assert(!name.empty, "function name cannot be empty");
        assert(!m_builtinTestFunctions.empty);
    }
    do
    {
        return isBuiltinTestFunction(name) || isUserTestFunction(name);
    }

    void getTestFunctionDescription(const string name, ref ProFunction func) const
    in
    {
        assert(!name.empty, "function name cannot be empty");
        assert(hasTestFunctionDescription(name));
    }
    do
    {
        if (isBuiltinTestFunction(name))
            func = m_builtinTestFunctions[name].dup;
        else if (isUserTestFunction(name))
            func = m_userTestFunctions[name].dup;
    }

    // -----------------------------------------------------------------------------------------------------------------
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

    // -----------------------------------------------------------------------------------------------------------------

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

    // -----------------------------------------------------------------------------------------------------------------

    void saveState()
    {
        m_builtinVariablesBackup = m_builtinVariables.dup;
        m_userVariablesBackup = m_userVariables.dup;

        // FIXME: save and compare user-defined test/replace functions too!
    }

    void restoreState()
    {
        m_builtinVariablesBackup.clear;
        m_userVariablesBackup.clear;
    }

    ProVariableDiff[] compareVariables(const ProVariable[] variablesFirst, const ProVariable[] variablesSecond,
        const bool builtin) const
    {
        ProVariableDiff[] result;
        string[] processedNames;

        // FIXME: refactor using std alhorithms like setDifference

        // 1) appended: items absent in first but exist in second
        if (!builtin)
        {
            foreach (v2; variablesSecond)
            {
                if (isInternalVariable(v2.name) || (processedNames.countUntil(v2.name) != -1))
                    continue;

                if (countUntil(variablesFirst, v2) == -1)
                {
                    result ~= ProVariableDiff(v2.name, v2.value.dup, "+");
                    processedNames ~= v2.name;
                }
            }

            // 2) removed: items exist in first but absent in second
            foreach (v1; variablesFirst)
            {
                if (isInternalVariable(v1.name) || (processedNames.countUntil(v1.name) != -1))
                    continue;

                if (countUntil(variablesSecond, v1) == -1)
                {
                    result ~= ProVariableDiff(v1.name, v1.value.dup, "-");
                    processedNames ~= v1.name;
                }
            }
        }

        // 3) diff: items exist in both first and second but with changed value
        foreach (v1; variablesFirst)
        {
            if (isInternalVariable(v1.name) || (processedNames.countUntil(v1.name) != -1))
                continue;

            foreach (v2; variablesSecond)
            {
                if (v1.name == v2.name && v1.value != v2.value)
                {
                    result ~= ProVariableDiff(v2.name, v2.value.dup, "!=");
                    processedNames ~= v2.name;
                }
            }
        }

        return result;
    }

    ProVariableDiff[] getBuiltinVariablesChanges() const
    {
        auto diffBuiltin = compareVariables(m_builtinVariablesBackup.values, m_builtinVariables.values, true);
        if (!diffBuiltin.empty)
        {
            writeln("Built-in variables changes:");
            writeln(diffBuiltin);
        }
        else writeln("No changes in built-in variables");
        writeln("-----------------------------------------------------------------------------------------------");

        return diffBuiltin;
    }

    ProVariableDiff[] getUserVariablesChanges() const
    {
        auto diffUser = compareVariables(m_userVariablesBackup.values, m_userVariables.values, false);
        if (!diffUser.empty)
        {
            writeln("User-defined variables changes:");
            writeln(diffUser);
        }
        else writeln("No changes in user-defined variables");
        writeln("-----------------------------------------------------------------------------------------------");

        return diffUser;
    }
}
