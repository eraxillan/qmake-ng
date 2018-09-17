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

// Internally used by Shunting-yard RPN converter and interpreter strings
// NOTE: '#' is a line-comment marker in qmake syntax, so it surely absent in expression string
private const auto STR_INTERNAL_MARKER = "#";
private const auto STR_ARITY_MARKER = "#FUNCTION_ARGUMENT_COUNT";
private const auto STR_INVOKE_REPLACE = "#FUNCTION_INVOKE_REPLACE";
private const auto STR_INVOKE_TEST = "#FUNCTION_INVOKE_TEST";
private const auto STR_BLOCK_BEGIN = "#CODE_BLOCK_BEGIN";
private const auto STR_BLOCK_END = "#CODE_BLOCK_END";

// -------------------------------------------------------------------------------------------------

//#region Private member functions
private bool isArityMarker(in string str)
{
    return (str == STR_ARITY_MARKER);
}

private bool isBooleanOperator(in string str)
{
    return (str == STR_EXCLAMATION_MARK) || (str == STR_COLON) || (str == STR_VERTICAL_BAR);
}

private bool isStringValue(in string str)
{
    return
        !isBooleanOperator(str) && !isArityMarker(str) && (str != STR_EXPAND_MARKER) &&
        (str != STR_OPENING_PARENTHESIS) && (str != STR_CLOSING_PARENTHESIS) &&
        (str != STR_OPENING_CURLY_BRACE) && (str != STR_CLOSING_CURLY_BRACE) &&
        (str != STR_COMMA);
}

alias EnquotedString = Tuple!(string, int);

private EnquotedString getEnquotedString(in int startIndex, in string str, in char quoteChar)
{
    if (str[startIndex] != quoteChar)
	{
        throw new Exception("String must begin with quote character: " ~ str[startIndex]);
    }

    int count = 1;
	int i = startIndex;
	string enquotedStr = "";
    do {
        i++;
        if (i >= str.length)
            break;

        string token; token ~= str[i];
        trace("token: '" ~ token ~ "'");

        if (token == STR_HASH)
		{
            trace("hash token (start comment)");
            break;
        }
		else if (token == STR_BACKSLASH)
		{
            trace("backslash (escape sequence)");

            auto es = new EscapeSequence();
            auto result = es.isEscapeSequence(str, i);
            if (result.result)
			{
                trace("escape sequence length = " ~ to!string(result.length));
                auto esStrExpanded = es.getEscapeSequence(str, i, result.length);
                enquotedStr ~= esStrExpanded;
                i += result.length - 1;
            }
			else
                enquotedStr ~= token;
        }
		else if (token == "" ~ quoteChar)
		{
            trace("closing quote char found");

            count ++;
            i ++;
            break;
        } else {
            enquotedStr ~= token;
        }
    } while (true); // FIXME: i < str.length

    if (count != 2)
	{
        throw new Exception("Unpaired double quote character found; count = " ~ to!string(count));
    }

    return EnquotedString(enquotedStr, i);
}


private string[] tokenizeString(in string str)
{
	struct Argument {
		string functionName;
		int argumentIndex;
	}

    trace("input: '" ~ str ~ "'");

    auto result = new QStack!string;
    string currentStr = "";
    auto parenthesisStack = new QStack!Argument;

    auto pushOperand = () {
        if (!currentStr.empty) {
            trace("token: '" ~ currentStr ~ "'");

            result.push(currentStr);
        }
        currentStr = "";
    };

    auto hasTopFunction = () {
        return !parenthesisStack.isEmpty() && !parenthesisStack.top().functionName.empty;
    };

    auto getTopFunctionName = () {
        return parenthesisStack.top().functionName;
    };

    auto getTopFunctionArgumentType = () {
        return ProFunction.getFunctionArgumentType(parenthesisStack.top().functionName, parenthesisStack.top().argumentIndex);
    };

    for (int i = 0; i < str.length; i++) {
        auto token = joinTokens(str, i, 1);

        // Single-line comment
        if (token == STR_HASH) {
            trace("hash token (start comment)");

            break;
        } else if (token == STR_BACKSLASH) {
            trace("ProParser::tokenizeString: backslash (escape sequence)");

            auto es = new EscapeSequence();
            auto result_1 = es.isEscapeSequence(str, i);
            if (result_1.result) {
                trace("escape sequence length = " ~ to!string(result_1.length));
                
				auto esStrExpanded = es.getEscapeSequence(str, i, result_1.length);
                currentStr ~= esStrExpanded;
                i += result_1.length - 1;
            } else
                  currentStr ~= token;
        } else if ((token == STR_DOUBLE_QUOTE) || (token == STR_SINGLE_QUOTE)) {
            trace("Special token: '" ~ token ~ "'");

            auto enquotedStrObj = getEnquotedString(i, str, token[0]);
            trace("enquoted string: '" ~ enquotedStrObj[0] ~ "', length: " ~ to!string(enquotedStrObj[1] - i - 2));

			result.push(enquotedStrObj[0]);
            i = enquotedStrObj[1] - 1;  // NOTE: 'i' will be incremented in 'for'
        } else if (token == STR_OPENING_PARENTHESIS) {
            trace("opening parenthesis token");

            // NOTE: "(" can be either syntax item (function call) or just part of arbitrary string value;
            //       however, it must have corresponding ")" or syntax error will be thrown
            auto functionName = currentStr;
            if (ProFunction.isReplaceFunction(functionName) || ProFunction.isTestFunction(functionName)) {
                pushOperand();

                trace("function name: '" ~ functionName ~ "'");
                trace("opening parenthesis");

                result.push(token);
            } else {
                functionName = "";
                currentStr ~= token;
            }

            parenthesisStack.push(Argument(functionName, 0));
        } else if (token == STR_CLOSING_PARENTHESIS) {
            trace("closing parenthesis token");

            if (hasTopFunction()) {
                pushOperand();

                trace("closing parenthesis");
                result.push(token);
            } else {
                currentStr ~= token;
            }

            parenthesisStack.pop();
        } else if (token == STR_OPENING_CURLY_BRACE) {
            trace("Special token: '" ~ token ~ "'");

            // NOTE: "{" can be either code block begin or part of variable expansion statement e.g. "$${MY_VAR}"
			// FIXME: check correctness
            auto prefix = str[i - 2 .. i];
			trace(prefix);

            if (prefix == STR_EXPAND_MARKER) {
                currentStr ~= STR_OPENING_CURLY_BRACE;

                do {
                    i ++;
                    if (i >= str.length)
                        break;

                    token = "" ~ str[i];
                    trace("token: '" ~ token ~ "'");

                    currentStr ~= token;
                    if (token == STR_CLOSING_CURLY_BRACE)
                        break;
                } while (true);

                if (("" ~ currentStr[currentStr.length - 1]) != STR_CLOSING_CURLY_BRACE) {
                    throw new Error("Absent closing curly brace in $${<VARIABLE_NAME>} statement: '" ~ currentStr[currentStr.length - 1] ~ "' found");
                }

                // TODO: syntax validation
                /* if (currentStr.length === STR_EXPAND_MARKER.length + STR_OPENING_CURLY_BRACE.length + STR_CLOSING_CURLY_BRACE.length) {
                    throw new Error("Empty VARIABLE_NAME in $${<VARIABLE_NAME>} statement");
                }
                if (!/[a-zA-Z]/.test(currentStr[3])) {
                    throw new Error("Invalid VARIABLE_NAME in $${<VARIABLE_NAME>} statement");
                } */
            } else {
                pushOperand();

                trace("closing parenthesis");
                result.push(token);
            }
        } else if (token == STR_CLOSING_CURLY_BRACE) {
            trace("Special token: '" ~ token ~ "'");

            result.push(token);
        } else if (token == STR_COMMA) {
            trace("Special token: '" ~ token ~ "'");

            if (hasTopFunction() && (getTopFunctionArgumentType() != VariableType.RAW_STRING)) {
                pushOperand();

				trace("comma as function argument separator");
				result.push(token);

                // NOTE: only in this case argument index should be increased
                parenthesisStack.top().argumentIndex ++;
            } else {
                currentStr ~= token;
            }
		} else if (isWhitespaceToken(token)) {
            trace("Whitespace token");

            // Add comma to whitespace-separated list for corresponding functions
            if (hasTopFunction() && (getTopFunctionArgumentType() == VariableType.STRING_LIST)) {
                trace("processing function '" ~ getTopFunctionName() ~ "' argument list...");

                auto shouldAddComma = (currentStr.length > 0);

                pushOperand();

                // Check whether this argument is the last one (next non-whitespace character must be ')')
                if (shouldAddComma) {
                    do {
                        i ++;
                        if (i >= str.length)
                            break;

                        token = "" ~ str[i];
                        if (isWhitespaceToken(token))
                            trace("Whitespace token");
                        else
                            break;
                    } while (true);

                    if (token == STR_HASH) {
                        trace("Hash token");
                        break;
                    }

                    shouldAddComma = (token != STR_CLOSING_PARENTHESIS);
                    if (shouldAddComma) {
                        trace("comma as function argument separator (added instead of whitespace)");

                        result.push(STR_COMMA);
                    }
                    else
                        i --;
                }
            // Do not split strings by whitespace inside functions with raw string arguments
            } else if ((!parenthesisStack.isEmpty()) && !parenthesisStack.top().functionName.empty &&
                (ProFunction.getFunctionArgumentType(parenthesisStack.top().functionName, parenthesisStack.top().argumentIndex) == VariableType.RAW_STRING)) {
                // NOTE: whitespaces after comma, function arguments separator, must be skipped
                if (result.top != STR_COMMA)
                    currentStr ~= token;
            } else {
                pushOperand();
            }
        } else if (token == STR_COLON) {
            trace("Special token: '" ~ token ~ "'");

            if ((!parenthesisStack.isEmpty()) && !parenthesisStack.top().functionName.empty &&
                (ProFunction.getFunctionArgumentType(parenthesisStack.top().functionName, parenthesisStack.top().argumentIndex) == VariableType.RAW_STRING)) {
                currentStr ~= token;
            } else {
                pushOperand();

                trace("colon (as single-line code block start marker)");
                result.push(STR_COLON);
            }
        } else {
            trace("Other token: '" ~ token ~ "'");

            currentStr ~= token;
        }
    }

    // NOTE: corner case "string without quotes at all"
    pushOperand();

    if (!parenthesisStack.isEmpty()) {
        throw new Error("Unbalanced parenthesis in expression");
    }

    return result.data();
}
//#endregion


//#region Assignment operators: =|+=|*=|-=|~=
alias AssignmentOperatorAction = void function(in string name, in string value, in string context);
immutable AssignmentOperatorAction[string] assignmentOperators;
//#endregion

//#region Boolean operators: AND, OR, NOT
enum AssociativityType { Invalid, Left = 0, Right }
alias BooleanOperatorAction = bool function(in bool, in bool);
struct BooleanOperator
{
	ubyte precedence;
	AssociativityType associativity;
	ubyte operandCount;
	BooleanOperatorAction action;
	
	this(ubyte _precedence, AssociativityType _associativity, ubyte _operandCount, BooleanOperatorAction _action)
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
	temp2[STR_EXCLAMATION_MARK] = BooleanOperator(4, AssociativityType.Left, 1, (in bool val1, in bool val2) { return !val1; });
	temp2[STR_COLON] = BooleanOperator(3, AssociativityType.Left, 2, (in bool val1, in bool val2) { return (val1 && val2); });
	temp2[STR_VERTICAL_BAR] = BooleanOperator(3, AssociativityType.Left, 2, (in bool val1, in bool val2) { return (val1 || val2); });
	temp2.rehash;
	booleanOperators = assumeUnique(temp2);
}



// -------------------------------------------------------------------------------------------------

class ExpressionEvaluator
{
	// FIXME: replace with QStack!ProExecutionContext
	ProExecutionContext m_executionContext;
	
	this(ProExecutionContext context)
	{
		m_executionContext = context;
	}

    static string[] convertToRPN(in string expr)
	{
        auto outputQueue = new QStack!string;
        auto operatorStack = new QStack!string;
        auto arityStack = new QStack!int;

        auto exprArray = tokenizeString(expr);
        trace("Raw expression: '" ~ expr ~ "'");
        trace("Tokenized expression: ", exprArray);

        auto pushOperator = () {
            auto op = operatorStack.pop();
            if (ProFunction.isReplaceFunction(op) || ProFunction.isTestFunction(op)) {
                auto functionArity = arityStack.pop();
                outputQueue.push(STR_INTERNAL_MARKER ~ to!string(functionArity));
                outputQueue.push(STR_ARITY_MARKER);

                if (ProFunction.isReplaceFunction(op)) {
//                    auto functionName = op.substring(STR_EXPAND_MARKER.length);
					// FIXME: check
					auto functionName = op[STR_EXPAND_MARKER.length .. $];
                    trace("replace function call (2): '" ~ functionName ~ "' with " ~ to!string(functionArity) ~ " arguments");
					
                    outputQueue.push(STR_INTERNAL_MARKER ~ functionName);
                    outputQueue.push(STR_INVOKE_REPLACE);
                } else {
                    trace("test function call (2): '" ~ op ~ "' with " ~ to!string(functionArity) ~ " arguments");
					
                    outputQueue.push(STR_INTERNAL_MARKER ~ op);
                    outputQueue.push(STR_INVOKE_TEST);
                }
            } else {
                trace("operand: '" ~ op ~ "'");

                outputQueue.push(op);
            }
        };

        for (int i = 0; i < exprArray.length; i++)
		{
            auto token = exprArray[i];
            trace("token[" ~ to!string(i) ~ "]:", token);

            if (ProFunction.isReplaceFunction(token) && (exprArray[i + 1] == STR_OPENING_PARENTHESIS)) {
                trace("replace function call (1): '" ~ token[STR_EXPAND_MARKER.length .. $] ~ "'");

                auto o1 = token;
                auto o2 = operatorStack.top();
                while (!o2.empty && ProFunction.isReplaceFunction(o2))
				{
                    pushOperator();
                    o2 = operatorStack.top();
                }

                operatorStack.push(o1);
                arityStack.push(1);
            }
			else if (ProFunction.isTestFunction(token) && (exprArray[i + 1] == STR_OPENING_PARENTHESIS))
			{
                trace("test function call (1): '" ~ token ~ "'");

                auto o1 = token;
                auto o2 = operatorStack.top();
                while (!o2.empty && ProFunction.isTestFunction(o2))
				{
                    pushOperator();
                    o2 = operatorStack.top();
                }

                operatorStack.push(o1);
                arityStack.push(1);
            } else if (token == STR_OPENING_PARENTHESIS) {
                trace("opening parenthesis");

                operatorStack.push(token);
            } else if (token == STR_CLOSING_PARENTHESIS) {
                trace("closing parenthesis");

                while (operatorStack.top() != STR_OPENING_PARENTHESIS) {
                    pushOperator();
                }
                operatorStack.pop();
            } else if (token == STR_COMMA) {
                trace("comma");

                arityStack.setTop(arityStack.top() + 1);

                while (operatorStack.top() != STR_OPENING_PARENTHESIS) {
                    pushOperator();
                }
            } else if (token == STR_COLON) {
                trace("colon");

                while (operatorStack.length > 0) {
                    pushOperator();
                }
                outputQueue.push(STR_BLOCK_BEGIN);
                operatorStack.push(STR_BLOCK_END);
            } else if (token == STR_OPENING_CURLY_BRACE) {
                trace("opening curly brace");

                while (operatorStack.length > 0) {
                    pushOperator();
                }
                operatorStack.push(STR_BLOCK_BEGIN);
            } else if (token == STR_CLOSING_CURLY_BRACE) {
                trace("closing curly brace");

                while (operatorStack.length > 0) {
                    pushOperator();
                }
                operatorStack.push(STR_BLOCK_END);
            } else if (isStringValue(token)) {
                trace("string operand: '" ~ token ~ "'");

                outputQueue.push(token);
            } else {
                throw new Exception("Invalid token '" ~ token ~ "'");
            }
        }

        while (operatorStack.length() > 0) {
            pushOperator();
        }

        trace();
        trace("RPN: ", outputQueue.data());
        trace();

        return outputQueue.data();
    }

    /*static*/ string[] evalRPN(in string[] rpnExpr)
	in
	{
		assert(!rpnExpr.empty, "RPN cannot be empty");
	}
	do
	{
        string[] values = rpnExpr.dup;
        auto array = new QStack!string;
//        int operandCount = -1;

        for (int i = 0; i < values.length; i++)
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
                        throw new Exception("Must be already processed");
                    }
                    case STR_INVOKE_REPLACE:
					{
                        trace("Invoking replace function '" ~ array.top() ~ "'");

                        // Get replace function name
                        string funcName = array.pop();
                        if (funcName.empty)
                            throw new Exception("Invalid replace function name");

                        // Validate replace function description
						if (!ProFunction.hasReplaceFunctionDescription(funcName))
                            throw new Exception("Unsupported replace function '" ~ funcName ~ "'");

                        // Get replace function actual argument count
                        string operandCountStr = array.pop();
                        if (!isNumeric(operandCountStr))
						{
                            throw new Exception("Invalid replace function argument count value '" ~ operandCountStr ~ "'");
                        }
						// FIXME: handle exception
                        int operandCount = to!int(operandCountStr);
                        /+if ((operandCount === undefined) || (operandCount < 0)) {
                            throw new Exception("Invalid replace function argument count value '" ~ operandCountStr ~ "'");
                        }+/
                        trace("Operand count: " ~ operandCountStr);

                        string[] val;
                        for (int j = 0; j < operandCount; j++)
						{
                            val ~= array.pop();  // val = val.concat(array.pop());
                        }
                        val = val.reverse();
                        trace("Operand values: ", val);

                        // Validate arguments count for non-variadic functions
						auto functionDescription = ProFunction.getReplaceFunctionDescription(funcName);
                        if (!functionDescription.m_isVariadic)
						{
                            if (functionDescription.m_requiredArgumentCount < 0)
							{
                                throw new Exception("Invalid replace function '" ~ funcName ~ "' argument count description");
                            }

                            if (functionDescription.m_optionalArgumentCount < 0)
							{
                                if (operandCount < functionDescription.m_requiredArgumentCount)
								{
                                    throw new /*RangeError*/Exception("Invalid number of arguments for replace function '" ~ funcName ~ "': " ~
                                        to!string(functionDescription.m_requiredArgumentCount) ~ " expected, but " ~ to!string(operandCount) ~ " given");
                                }
                            }
							else
							{
                                int minArgCount = functionDescription.m_requiredArgumentCount;
                                int maxArgCount = minArgCount + functionDescription.m_optionalArgumentCount;
                                if ((operandCount < minArgCount) || (operandCount > maxArgCount))
								{
                                    throw new /*RangeError*/Exception("Invalid number of arguments for replace function '" ~ funcName ~ "': from " ~
                                        to!string(minArgCount) ~ " to " ~ to!string(maxArgCount) ~ " expected, but " ~ to!string(operandCount) ~ " given");
                                }
                            }
                        }

                        // Call function
                        const(string[]) result = functionDescription.m_action(val);

                        // Validate and save execution result
                        /*if (!typeUtils.isArray(result)) {
                            result = [result];
                        }*/
						array.push(result);

                        trace("Result: ", result);
                        break;
                    }
                    case STR_INVOKE_TEST:
					{
                        trace("Invoking test function '" ~ array.top() ~ "'");

                        // Get test function name
                        string funcName = array.pop();
                        if (funcName.empty)
                            throw new Exception("Invalid test function name");

                        // Validate test function description
						if (!ProFunction.hasTestFunctionDescription(funcName))
                            throw new Exception("Unsupported test function '" ~ funcName ~ "'");

                        // Get test function actual argument count
                        string operandCountStr = array.pop();
                        if (!isNumeric(operandCountStr))
                            throw new Exception("Invalid test function argument count value '" ~ operandCountStr ~ "'");
						// FIXME: handle exception
                        int operandCount = to!int(operandCountStr);
                        /*if ((operandCount === undefined) || (operandCount < 0)) {
                            throw new Exception("Invalid test function argument count value '" ~ operandCountStr ~ "'");
                        }*/
                        trace("OPERAND COUNT: " ~ operandCountStr);

                        string[] val;
                        for (int j = 0; j < operandCount; j++)
                            val ~= array.pop();
                        val = val.reverse();
                        trace("OPERANDS: ", val);

                        // Validate arguments count for non-variadic functions
						auto functionDescription = ProFunction.getTestFunctionDescription(funcName);
                        if (!functionDescription.m_isVariadic)
						{
                            if (functionDescription.m_requiredArgumentCount < 0)
							{
                                throw new Exception("Invalid test function '" ~ funcName ~ "' argument count description");
                            }

                            if (functionDescription.m_optionalArgumentCount < 0)
							{
                                if (operandCount < functionDescription.m_requiredArgumentCount)
								{
                                    throw new /*RangeError*/Exception("Invalid number of arguments for test function '" ~ funcName ~ "': " ~
                                        to!string(functionDescription.m_requiredArgumentCount) ~ " expected, but " ~ to!string(operandCount) ~ " given");
                                }
                            }
							else
							{
                                int minArgCount = functionDescription.m_requiredArgumentCount;
                                int maxArgCount = minArgCount + functionDescription.m_optionalArgumentCount;
                                if ((operandCount < minArgCount) || (operandCount > maxArgCount))
								{
                                    throw new /*RangeError*/Exception("Invalid number of arguments for function '" ~ funcName ~ "': from " ~
                                        to!string(minArgCount) ~ " to " ~ to!string(maxArgCount) ~ " expected, but " ~ to!string(operandCount) ~ " given");
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
                        const(string[]) result = functionDescription.m_action(val);

                        // Validate and save execution result
                        /*if (!(result instanceof Array)) {
                            result = [result];
                        }*/
						
                        array.push(result);
                        break;
                    }
                    case STR_BLOCK_BEGIN: {
                        // FIXME: implement
                        break;
                    }
                    case STR_BLOCK_END: {
                        // FIXME: implement
                        break;
                    }
                    default:
					{
                        // Currently supported operand types: function arity number, replace/test function name
                        string operand = token[STR_INTERNAL_MARKER.length .. $];
                        if (isNumeric(operand))
						{
                            // 1) Function arity number
                            array.push(operand);

                            i ++;
                            if (i >= values.length)
                                throw new Exception("Function arity command is absent");

                            token = values[i];
                            if (token != STR_ARITY_MARKER)
                                throw new Exception("Function arity command is absent");
                        }
						else if (ProFunction.isReplaceFunction(STR_EXPAND_MARKER ~ operand) || ProFunction.isTestFunction(operand))
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
//                assert.isNotEmpty(this.executionContext);
                string tokenExpanded = m_executionContext.expandVariables(token);
                if (token != tokenExpanded)
                    token = tokenExpanded;

                array.push(token);
			} else {
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
}
