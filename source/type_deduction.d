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

module type_deduction;


import std.experimental.logger;

import std.typecons;
import std.uni;
import std.conv;
import std.stdio;
import std.file;
import std.process;
import std.getopt;
import std.path;
import std.string;
import std.range;

import std.algorithm;
//import std.algorithm.comparison : equal;
import std.container.rbtree;

import common_const;
import common_utils;
import project_variable;
import persistent_property;
import project_function;
import project_context;
import qmakeexception;
import qmakeparser;


public alias RvalueEvalResult = Tuple!(VariableType, "type", string[], "value");

/+
public enum VariableType
{
    UNKNOWN = -1,
    VOID = 0,
    BOOLEAN = 1,
    RAW_STRING,               // string with any characters, e.g. argument of message() test function: message(I am a raw string)
    NUMBER,                   // integer number, e.g. qmake debug level: debug(1, This text will be shown under debug level 1)
    STRING,                   // string without whitespaces/commas, e.g. variable name
    STRING_LIST,    	  	  // array of strings without whitespaces/commas
    RESTRICTED_STRING,        // string without whitespaces/commas with value from the specified array, e.g. TEMPLATE = app|...|lib
    RESTRICTED_STRING_LIST,   // array of such strings
    OBJECT,                   // object with properties (e.g. host.arch)
    OBJECT_LIST,              // list of objects with properties described above
	COUNT
}
+/

VariableType deduceRvalueType(in RvalueEvalResult[] rvalueCollection)
{
    // Corner case
    if (rvalueCollection.empty)
    {
        trace("Result as rvalue collection: <empty>");
        return VariableType.UNKNOWN;
    }

    trace("Result as rvalue collection: ", rvalueCollection);

    // Collect unique data types from rvalue collection
    version(unittest)
    {
        throw new NotImplementedError("RedBlackTree must be replaced by smth that compiles in unit-test mode!");
    }
    else
    {
        auto rvalueDataTypes = redBlackTree!VariableType();
        for (int i = 0; i < rvalueCollection.length; i++)
        {
            rvalueDataTypes.insert(rvalueCollection[i].type);
        }

        // Check whether we need conversion
        immutable bool needConversion = uniq(rvalueDataTypes[]).walkLength > 1;
        // FIXME: implement implicit conversion
        assert((!needConversion));

        VariableType result = rvalueCollection[0].type;
        trace("Final result type: ", result);
        return result;
    }
}

string[] prettifyRvalue(in RvalueEvalResult[] rvalueCollection, in VariableType dataType)
{
    string[] result;

    switch (dataType)
    {
        case VariableType.STRING:
        {
            result = [""];
            for (int i = 0; i < rvalueCollection.length; i++)
            {
                if (!rvalueCollection[i].value.empty)
                    result[0] ~= rvalueCollection[i].value[0];
            }
            trace("Result final value: ", "`", result[0], "`");
            break;
        }
        case VariableType.STRING_LIST:
        case VariableType.RESTRICTED_STRING_LIST:
        {
            for (int i = 0; i < rvalueCollection.length; i++)
            {
                result ~= rvalueCollection[i].value;
            }
            trace("Result is already list, no conversion required");
            break;
        }
        default:
        {
            throw new NotImplementedException("Invalid variable type");
        }
    }

    return result;
}

/+
/**
  * Currently just check whether input expression is string or list.
  * Params:
  *      exprArray = expression to check
  * Returns: VariableType.STRING if input is a string, VariableType.STRING_LIST otherwise
  */
public VariableType deduceExpressionType(ref ParseTree statementNode, ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage)
{
    auto outputQueue = new QStack!VariableType;
    auto operatorStack = new QStack!string;
    auto arityStack = new QStack!int;

    auto pushOperator = () {
        auto op = operatorStack.pop();

        if (context.hasReplaceFunctionDescription(op))
        {
            auto functionArity = arityStack.pop();

			auto functionName = op[STR_EXPAND_MARKER.length .. $];
            if (verboseConvert) trace("replace function call (2): '" ~ functionName ~ "' with " ~ to!string(functionArity) ~ " arguments");

            VariableType returnType = context.getReplaceFunctionDescription(functionName).m_returnType;
            if (verboseConvert) trace("replace function (2) return type: ", returnType);

            outputQueue.push(returnType);
        }
        else if (context.hasTestFunctionDescription(op))
        {
            auto functionArity = arityStack.pop();

            auto functionName = op;
            if (verboseConvert) trace("test function call (2): '" ~ functionName ~ "' with " ~ to!string(functionArity) ~ " arguments");
            
            VariableType returnType = context.getTestFunctionDescription(functionName).m_returnType;
            if (verboseConvert) trace("test function (2) return type: ", returnType);

            outputQueue.push(returnType);
        }
        else
        {
            if (verboseConvert) trace("operand: '" ~ op ~ "'");

            outputQueue.push(VariableType.STRING);
        }
    };

    for (int i = 0; i < exprArray.length; i++)
	{
        string token = exprArray[i];
        trace("token[" ~ to!string(i) ~ "]: '", token, "'");

        if (context.hasReplaceFunctionDescription(token) && (i + 1 < exprArray.length) && (exprArray[i + 1] == STR_OPENING_PARENTHESIS))
        {
            if (verboseConvert) trace("replace function call (1): '" ~ token[STR_EXPAND_MARKER.length .. $] ~ "'");

            auto o1 = token;
            auto o2 = operatorStack.top();
            while (!o2.empty && context.hasReplaceFunctionDescription(o2))
			{
                pushOperator();
                o2 = operatorStack.top();
            }

            operatorStack.push(o1);
            arityStack.push(1);
        }
		else if (context.hasTestFunctionDescription(token) && (i + 1 < exprArray.length) && (exprArray[i + 1] == STR_OPENING_PARENTHESIS))
		{
            if (verboseConvert) trace("test function call (1): '" ~ token ~ "'");

            auto o1 = token;
            auto o2 = operatorStack.top();
            while (!o2.empty && context.hasTestFunctionDescription(o2))
			{
                pushOperator();
                o2 = operatorStack.top();
            }

            operatorStack.push(o1);
            arityStack.push(1);
            assert(arityStack.length() >= 1);
        }
        else if (token == STR_OPENING_PARENTHESIS)
        {
            if (verboseConvert) trace("opening parenthesis");

            operatorStack.push(token);
        }
        else if (token == STR_CLOSING_PARENTHESIS)
        {
            if (verboseConvert) trace("closing parenthesis");

            if (operatorStack.top() == STR_OPENING_PARENTHESIS)
                operatorStack.pop();

            while (!operatorStack.isEmpty() && (operatorStack.top() != STR_OPENING_PARENTHESIS))
            {
                pushOperator();
            }
        }
        else if (token == STR_COMMA)
        {
            if (verboseConvert) trace("comma");

            assert(arityStack.length() >= 1);
            arityStack.setTop(arityStack.top() + 1);

            while (operatorStack.top() != STR_OPENING_PARENTHESIS)
            {
                pushOperator();
            }
        }
        // Function argument/variable/property expansion
        // Format: '$' / '$$' ('{' / '(' / '[')? id/number
        else if (token == STR_SINGLE_EXPAND_MARKER) // '$'
        {
            if (i + 1 >= exprArray.length)
                throw new EvalLogicException("Makefile variable name is absent");

            immutable string nextToken = exprArray[++i];
            if (verboseConvert) trace("makefile variable 1 '", nextToken, "'");

            // FIXME: implement
//            outputQueue.push(context.getVariableDescription(nextToken).type);
            outputQueue.push(VariableType.STRING);
        }
        else if (token == STR_GENERATOR_EXPAND_MARKER) // '${'
        {
            if (i + 2 >= exprArray.length)
                throw new EvalLogicException("Makefile variable name is absent");
            
            immutable string nextToken = exprArray[++i];
            if (verboseConvert) trace("makefile variable 2 '", nextToken, "'");
            immutable string closingBracket = exprArray[++i];
            assert(closingBracket == STR_CLOSING_CURLY_BRACE);

            // FIXME: implement
//            outputQueue.push(context.getVariableDescription(nextToken).type);
            outputQueue.push(VariableType.STRING);
        }
        else if (token == STR_EXPAND_MARKER)    // '$$' (DecNumber / id)
        {
            if (i + 1 >= exprArray.length)
                throw new EvalLogicException("Project variable name is absent");

            immutable string nextToken = exprArray[++i];
            if (isNumeric(nextToken, 10))
            {
                if (verboseConvert) trace("function argument 1 '", nextToken, "'");

                // FIXME: implement
            }
            else
            {
                if (verboseConvert) trace("project variable 1 '", nextToken, "'");

                outputQueue.push(context.getVariableDescription(nextToken).type);
            }
        }
        else if (token == STR_VARIABLE_EXPAND_MARKER) // '$${' (DecNumber / id)
        {
            if (i + 2 >= exprArray.length)
                throw new EvalLogicException("Project variable name is absent");
            
            immutable string nextToken = exprArray[++i];
            if (isNumeric(nextToken, 10))
            {
                if (verboseConvert) trace("function argument 2 '", nextToken, "'");

                // FIXME: implement
            }
            else
            {
                if (verboseConvert) trace("project variable 2 '", nextToken, "'");

                outputQueue.push(context.getVariableDescription(nextToken).type);
            }

            immutable string closingBracket = exprArray[++i];
            assert(closingBracket == STR_CLOSING_CURLY_BRACE);
        }
        else if (token == STR_ENV_VARIABLE_EXPAND_MARKER) // '$$('
        {
            if (i + 2 >= exprArray.length)
                throw new EvalLogicException("Environment variable name is absent");
            
            immutable string nextToken = exprArray[++i];
            if (verboseConvert) trace("environment variable '", nextToken, "'");
            immutable string closingBracket = exprArray[++i];
            assert(closingBracket == STR_CLOSING_PARENTHESIS);

            // FIXME: implement
            outputQueue.push(VariableType.STRING);
        }
        else if (token == STR_PROPERTY_EXPAND_MARKER) // '$$['
        {
            if (i + 2 >= exprArray.length)
                throw new EvalLogicException("Persistent property name is absent");
            
            string name = exprArray[++i];
            if (verboseConvert) trace("persistent property '", name, "'");
            
            string nextToken = exprArray[++i];
            if ((nextToken == STR_PROPERTY_GET_SUFFIX) || (nextToken == STR_PROPERTY_SRC_SUFFIX))
                nextToken = exprArray[++i];
            assert(nextToken == STR_CLOSING_SQUARE_BRACE);

            // FIXME: implement
            outputQueue.push(VariableType.STRING);
        }
        else if ((token == STR_DOUBLE_QUOTE) || (token == STR_SINGLE_QUOTE))
        {
            if (verboseConvert) trace("Quote character: '" ~ token ~ "'");

            // Search for corresponding closing doublequote/quote
            int closingQuoteIndex = i + 1;
            for ( ; closingQuoteIndex < exprArray.length; closingQuoteIndex++)
            {
                if (exprArray[closingQuoteIndex] == token)
                    break;
            }
            if (closingQuoteIndex == exprArray.length)
                throw new EvalLogicException("Closing doublequote/quote character is absent");

            // FIXME: remove recursion here
            const string[] temp = exprArray[i + 1 .. closingQuoteIndex];
			outputQueue.push(deduceExpressionType(temp, context, persistentStorage));
            i = closingQuoteIndex;
        } 
        else
        {
            if (verboseConvert) trace("string operand: '" ~ token ~ "'");

            outputQueue.push(VariableType.STRING);
        }
    }

    while (operatorStack.length() > 0)
    {
        pushOperator();
    }

    // Analyze output queue types:
    // - check whether they are compatible with each other
    // - if yes, find the `superposition` type
    if (verboseConvert) trace();
    if (verboseConvert) trace("rvalue types: ", outputQueue.data());
    VariableType result = VariableType.UNKNOWN;
    foreach (type; outputQueue.data())
    {
        assert(type != VariableType.UNKNOWN);
        assert(type != VariableType.VOID);
        assert(type != VariableType.BOOLEAN);
        assert(type != VariableType.OBJECT);
        assert(type != VariableType.OBJECT_LIST);

        // `list` is a most powerful meta-type:
        // `string` and `number` will be converted to list implicitly, and the result become `list`;
        // so if we found at least one list entity, then the result surelu will be list too,
        // no need to watch further entities types
        if ((type == VariableType.STRING_LIST) || (type == VariableType.RESTRICTED_STRING_LIST))
        {
            result = VariableType.STRING_LIST;
            break;
        }
        // `string` is a medium-power meta-type:
        // only `number` type will be converted to `string` implicitly;
        // the result type will be `string` only if all entities will have `string` or `number` types
        else if ((type == VariableType.RAW_STRING) || (type == VariableType.STRING) || (type == VariableType.RESTRICTED_STRING))
        {
            if (result == VariableType.UNKNOWN)
                result = VariableType.STRING;
            else
            {
                trace("got second string item, so it is a list");
                result = VariableType.STRING_LIST;
                break;
            }
        }
        // `number` is a weakest meta-type:
        // the result type will be `number` only if all entities will have this type
        else if (type == VariableType.NUMBER)
        {
            if (result != VariableType.STRING)
                result = VariableType.NUMBER;
        }
        else throw new NotImplementedError("Invalid variable type");
    }
    trace("deduced type: ", result);
    if (verboseConvert) trace();

    return result;
}
+/