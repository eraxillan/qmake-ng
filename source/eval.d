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

module eval;

import std.experimental.logger;

import std.typecons;
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
import common_utils;
import project_escape_sequence;
import project_variable;
import project_function;
import project_context;

import rpn;
import qmakeexception;
import persistent_property;

/*
var builtinVariableModule = require("./builtin_variable_description");
var builtinFunctionModule = require("./builtin_function_description");
var executionContextModule = require("./pro_execution_context");

var commonModule = require("./common");
var QStack = commonModule.QStack;

var proParserModule = require("./pro_parser");
var ProParser = proParserModule.ProParser;
*/

// -------------------------------------------------------------------------------------------------

//#region Assignment operators: =|+=|*=|-=|~=
alias AssignmentOperatorAction = void function(in string name, in string value, in string context);
immutable AssignmentOperatorAction[string] assignmentOperators;
//#endregion

//#region Boolean operators: AND, OR, NOT
enum AssociativityType
{
    Invalid,
    Left = 0,
    Right
}

alias BooleanOperatorAction = bool function(in bool, in bool);
struct BooleanOperator
{
    ubyte precedence;
    AssociativityType associativity;
    ubyte operandCount;
    BooleanOperatorAction action;

    this(ubyte _precedence, AssociativityType _associativity, ubyte _operandCount,
            BooleanOperatorAction _action)
    {
        precedence = _precedence;
        associativity = _associativity;
        operandCount = _operandCount;
        action = _action;
    }
}

immutable BooleanOperator[string] booleanOperators;
//#endregion

static this()
{
    import std.exception : assumeUnique;

    AssignmentOperatorAction[string] temp1; // mutable buffer
    temp1[STR_EQUALS] = (in string name, in string value, in string context) { /*context.assignVariable(name, value);*/ };
    temp1[STR_PLUS_EQUALS] = (in string name, in string value, in string context) { /*context.appendAssignVariable(name, value);*/ };
    temp1[STR_ASTERISK_EQUALS] = (in string name, in string value, in string context) { /*context.appendUniqueAssignVariable(name, value);*/ };
    temp1[STR_MINUS_EQUALS] = (in string name, in string value, in string context) { /*context.removeAssignVariable(name, value);*/ };
    temp1[STR_TILDE_EQUALS] = (in string name, in string value, in string context) { /* FIXME: implement */ };
    temp1.rehash; // for faster lookups
    assignmentOperators = assumeUnique(temp1);

    BooleanOperator[string] temp2;
    temp2[STR_EXCLAMATION_MARK] = BooleanOperator(4, AssociativityType.Left, 1,
            (in bool val1, in bool val2) { return !val1; });
    temp2[STR_COLON] = BooleanOperator(3, AssociativityType.Left, 2, (in bool val1, in bool val2) {
        return (val1 && val2);
    });
    temp2[STR_VERTICAL_BAR] = BooleanOperator(3, AssociativityType.Left, 2,
            (in bool val1, in bool val2) { return (val1 || val2); });
    temp2.rehash;
    booleanOperators = assumeUnique(temp2);
}

// -------------------------------------------------------------------------------------------------

class ExpressionEvaluator
{
    // FIXME: replace with QStack!ProExecutionContext for function/block evaluation
    private ProExecutionContext m_executionContext;
    private PersistentPropertyStorage m_persistentStorage;

    this(ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage)
    {
        m_executionContext = context;
        m_persistentStorage = persistentStorage;
    }

    public string[] evalRPN(in string[] rpnExpr)
    in
    {
        assert(!rpnExpr.empty, "RPN cannot be empty");
    }
    do
    {
        string[] values = rpnExpr.dup;
        auto array = new QStack!string;
        for (int i; i < values.length; i++)
        {
            string token = values[i];
            trace("RPN[" ~ to!string(i) ~ "] = '" ~ token ~ "'");

            if (token.startsWith(STR_INTERNAL_MARKER))
            {
//                console.log("CMD:", token);

                switch (token)
                {
                case STR_ARITY_MARKER:
                    {
                        throw new EvalLogicException("Logic error: STR_ARITY_MARKER must be already processed");
                    }
                case STR_INVOKE_REPLACE:
                    {
                        evalReplaceFunction(array);
                        break;
                    }
                case STR_INVOKE_TEST:
                    {
                        evalTestFunction(array);
                        break;
                    }
                case STR_BLOCK_BEGIN:
                    {
                        // FIXME: implement
                        throw new NotImplementedException("Code block eval not implemented yet");
                    }
                case STR_BLOCK_END:
                    {
                        // FIXME: implement
                        throw new NotImplementedException("Code block eval not implemented yet");
                    }
                default:
                    {
                        // Currently supported operand types: function arity number, replace/test function name
                        string operand = token[STR_INTERNAL_MARKER.length .. $];
                        if (isNumeric(operand))
                        {
                            // 1) Function arity number
                            array.push(operand);

                            i++;
                            if (i >= values.length)
                                throw new Exception("Function arity command is absent");

                            token = values[i];
                            if (token != STR_ARITY_MARKER)
                                throw new Exception("Function arity command is absent");
                        }
                        else if (ProFunction.isReplaceFunction(STR_EXPAND_MARKER ~ operand)
                                || ProFunction.isTestFunction(operand))
                        {
                            // 2) Replace/test function name
                            array.push(operand);
                        }
                        else
                        {
                            throw new Exception("Invalid operand '" ~ operand ~ "'");
                        }
                    }
                }
            }
            else if (isStringValue(token))
            {
                /*string tokenExpanded = m_executionContext.expandVariables(m_persistentStorage, token);
                string[] expandedArray = tokenExpanded.split(" ");

                // FIXME HACK: expand function deal only with raw string, but functions works with lists
                // FIXME HACK: Remove empty items from list
                string[] temp;
                foreach (value; expandedArray)
                {
                    if (!value.empty)
                        temp ~= value;
                }

                trace("RPN string operand: '", token, "', expanded: '", tokenExpanded, "', expanded-splitted: ", temp);
                //if (temp.empty) temp = [" "];
                array.push(temp);*/

// FIXME: need to recursively (or not) expand enquoted expression!
                if (token == STR_DOUBLE_QUOTE)
                {
                    assert(i + 2 < values.length);
                    assert(values[i + 1] != STR_DOUBLE_QUOTE);
                    assert(values[i + 2] == STR_DOUBLE_QUOTE);

                    token = values[i + 1];
                    trace("Enquoted string detected: ", token);
                    i += 2;
                }
                
                string[] tokenExpanded = m_executionContext.expandAllVariables(m_persistentStorage, token);
//                writeln();
//                writeln("ORIGINAL: ", token);
//                writeln("EXPANDED: ", tokenExpanded);
//                writeln();
                array.push(tokenExpanded);
            }
            else
            {
                throw new Exception("Unknown token '" ~ token ~ "'");
            }
        }

        /*if (array.length != 1)
		{
            trace("RESULT:", array);
            throw new Exception("Invalid RPN expression");
        }*/

        trace("RPN expression: ", rpnExpr);
        trace("RPN result: ", array.data());
        return array.data();
    }

    private void evalReplaceFunction(ref QStack!string array)
    {
        trace("Invoking replace function '" ~ array.top() ~ "'");

        // Get replace function name
        string funcName = array.pop();
        if (funcName.empty)
            throw new Exception("Invalid replace function name");

        // Validate replace function description
        if (!ProFunction.hasReplaceFunctionDescription(funcName))
            throw new Exception("Unsupported replace function '" ~ funcName ~ "'");
        auto functionDescription = ProFunction.getReplaceFunctionDescription(funcName);

        // Get replace function actual argument count
        string operandCountStr = array.pop();
        if (!isNumeric(operandCountStr))
            throw new Exception(
                    "Invalid replace function argument count value '" ~ operandCountStr ~ "'");

        // FIXME: can throw ConvException/ConvOverflowException
        int operandCount = to!int(operandCountStr);
        trace("Operand count: ", operandCountStr, "; array count: ", array.length);

        string[] val;
        if (functionDescription.m_isVariadic)
        {
            trace("Replace function '", funcName,
                    "' is variadic: ignore argument count from RPN, just pop the entire stack");
            operandCount = cast(int)(array.length);
        }
        for (int j = 0; j < operandCount; j++)
        {
            val ~= array.pop();
        }
        val = val.reverse();
        trace("Operand values: ", val);

        // Validate arguments count for non-variadic functions
        if (!functionDescription.m_isVariadic)
        {
            if (functionDescription.m_requiredArgumentCount < 0)
            {
                throw new Exception(
                        "Invalid replace function '" ~ funcName ~ "' argument count description");
            }

            if (functionDescription.m_optionalArgumentCount < 0)
            {
                if (operandCount < functionDescription.m_requiredArgumentCount)
                {
                    throw new  /*RangeError*/ Exception(
                            "Invalid number of arguments for replace function '" ~ funcName ~ "': " ~ to!string(
                            functionDescription.m_requiredArgumentCount) ~ " expected, but " ~ to!string(
                            operandCount) ~ " given");
                }
            }
            else
            {
                int minArgCount = functionDescription.m_requiredArgumentCount;
                int maxArgCount = minArgCount + functionDescription.m_optionalArgumentCount;
                if ((operandCount < minArgCount) || (operandCount > maxArgCount))
                {
                    throw new  /*RangeError*/ Exception(
                            "Invalid number of arguments for replace function '" ~ funcName ~ "': from " ~ to!string(
                            minArgCount) ~ " to " ~ to!string(
                            maxArgCount) ~ " expected, but " ~ to!string(operandCount) ~ " given");
                }
            }
        }

        // Call function
        const(string[]) result = functionDescription.exec(m_executionContext, val);

        // Validate and save execution result
        array.push(result);

        trace("Result: ", result);
    }

    private void evalTestFunction(ref QStack!string array)
    {
        trace("Invoking test function '" ~ array.top() ~ "'");

        // Get test function name
        string funcName = array.pop();
        if (funcName.empty)
            throw new Exception("Invalid test function name");

        // Validate test function description
        if (!ProFunction.hasTestFunctionDescription(funcName))
            throw new Exception("Unsupported test function '" ~ funcName ~ "'");
        auto functionDescription = ProFunction.getTestFunctionDescription(funcName);

        // Get test function actual argument count
        string operandCountStr = array.pop();
        if (!isNumeric(operandCountStr))
            throw new Exception(
                    "Invalid test function argument count value '" ~ operandCountStr ~ "'");
        // FIXME: handle exception
        int operandCount = to!int(operandCountStr);
        /*if ((operandCount === undefined) || (operandCount < 0)) {
                            throw new Exception("Invalid test function argument count value '" ~ operandCountStr ~ "'");
                        }*/
        trace("Operand count: " ~ operandCountStr);

        string[] val;
        if (functionDescription.m_isVariadic)
        {
            trace("Replace function '", funcName,
                    "' is variadic: ignore argument count from RPN, just pop the entire stack");
            operandCount = cast(int)(array.length);
        }
        for (int j = 0; j < operandCount; j++)
        {
            val ~= array.pop();
        }
        val = val.reverse();
        trace("Operand values: ", val);

        // Validate arguments count for non-variadic functions
        if (!functionDescription.m_isVariadic)
        {
            if (functionDescription.m_requiredArgumentCount < 0)
            {
                throw new Exception(
                        "Invalid test function '" ~ funcName ~ "' argument count description");
            }

            if (functionDescription.m_optionalArgumentCount < 0)
            {
                if (operandCount < functionDescription.m_requiredArgumentCount)
                {
                    throw new  /*RangeError*/ Exception(
                            "Invalid number of arguments for test function '" ~ funcName ~ "': " ~ to!string(
                            functionDescription.m_requiredArgumentCount) ~ " expected, but " ~ to!string(
                            operandCount) ~ " given");
                }
            }
            else
            {
                int minArgCount = functionDescription.m_requiredArgumentCount;
                int maxArgCount = minArgCount + functionDescription.m_optionalArgumentCount;
                if ((operandCount < minArgCount) || (operandCount > maxArgCount))
                {
                    throw new  /*RangeError*/ Exception(
                            "Invalid number of arguments for function '" ~ funcName ~ "': from " ~ to!string(
                            minArgCount) ~ " to " ~ to!string(
                            maxArgCount) ~ " expected, but " ~ to!string(operandCount) ~ " given");
                }
            }
        }

        // Add code block
        // FIXME: implement
        /+
        if (functionDescription.isGenerator) {
            i ++;
            if (i >= values.length) {
                throw new Exception("Code block is absent");
            }
            token = values[i];
            if (token !== STR_BLOCK_BEGIN) {
                throw new Exception("Code block is absent");
            }
            i ++;

            string[] codeBlockArray;
            while ((i < values.length) && (values[i] != STR_BLOCK_END)) {
                codeBlockArray.push(values[i]);
                i ++;
            }
            /* console.log("-------------------------");
            console.log("CODE BLOCK:", codeBlockArray)
            console.log("-------------------------"); */

            auto ev = new ExpressionEvaluator(m_executionContext);
            val.push(function() {
                ev.evalRPN(codeBlockArray);
            });

            val.push(m_executionContext);
        }
		+/

        // Call function
        // NOTE: test function can return boolean or void
        const(string[]) result = functionDescription.exec(m_executionContext, val);
        
        // Validate and save execution result
        array.push(result);

        trace("Result: ", result);
    }
}
