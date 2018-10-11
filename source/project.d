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

module project;

import std.file;
import std.path;
import std.string;
import std.algorithm;

import std.experimental.logger;

import preprocessor;
import qmakeparser;

import common_const;
import eval;
import project_variable;
import project_context;
import rpn;

public class Project
{
    private ProExecutionContext m_context;

    public this(ref ProExecutionContext context)
    {
        m_context = context;
    }

    public bool tryParse(in string fileName) const
    {
        trace("Trying to parse project file '" ~ fileName ~ "'...");

        auto proFileContents = std.file.readText(fileName);
        LineInfo[] result;
        proFileContents = preprocessLines(splitLines(proFileContents), result);

        auto parseTree = QMakeProject(proFileContents);
        if (!parseTree.successful)
        {
            trace(parseTree);
            trace("Parsing project file '" ~ fileName ~ "' FAILED:");
        }
        else
            trace("Project file '" ~ fileName ~ "' successfully parsed");

        return parseTree.successful;
    }

    public bool eval(in string fileName) /*const*/
    {
        trace("Trying to parse project file '" ~ fileName ~ "'...");

        // NOTE: use the previously eval'd state, because we can need already defined variables
        m_context.setupPaths(fileName);

        auto proFileContents = std.file.readText(fileName);
        LineInfo[] result;
        proFileContents = preprocessLines(splitLines(proFileContents), result);

        auto parseTree = QMakeProject(proFileContents);
        if (!parseTree.successful)
        {
            trace(parseTree);
            trace("Parsing project file '" ~ fileName ~ "' FAILED:");
            return false;
        }

        trace("Project file '" ~ fileName ~ "' successfully parsed");

        trace("Trying to evaluate project file '" ~ fileName ~ "'...");

        // Get the root project node (must be the only child of parse tree)
        auto projectNode = parseTree.children[0];
        foreach (ref child; projectNode.children)
        {
            auto statementNode = child.children[0];
            trace("STATEMENT: " ~ statementNode.name ~ ": [" ~ statementNode.matches.join(" ") ~ "]");

            evalStatementNode(m_context, statementNode);
        }

        // Output all built-in and user-defined variables
        /*trace("\n\nqmake built-in variable values:");
        foreach (variableName; m_context.getBuiltinVariableNames())
        {
            string[] variableValue = m_context.getVariableRawValue(variableName);
            if (!variableValue.empty)
                trace(variableName, " = ", variableValue);
        }
        trace("\n\nqmake user-defined variable values:");
        foreach (variableName; m_context.getUserDefinedVariableNames())
        {
            string[] variableValue = m_context.getVariableRawValue(variableName);
            if (!variableValue.empty)
                trace(variableName, " = ", variableValue);
        }
        trace("\n\n");*/

        trace("Project file '" ~ fileName ~ "' successfully evaluated");
        return true;
    }

    // ----------------------------------------------------------------------------------------------------------------------------------------------

    private void evalStatementNode(ref ProExecutionContext context, ref ParseTree statementNode) /+const+/
    {
        switch (statementNode.name)
        {
        case "QMakeProject.EmptyStatement":
            trace("Ignore QMakeProject.EmptyStatement");
            break;
        case "QMakeProject.Assignment":
            evalVariableAssignmentNode(context, statementNode);
            break;
        case "QMakeProject.Scope":
            evalScopeNode(context, statementNode);
            break;
        case "QMakeProject.BooleanExpression":
            evalScopeConditionNode(context, statementNode);
            break;
        case "QMakeProject.FunctionDeclaration":
            error("NOT IMPLEMENTED YET");
            break;
        default:
            error("Invalid statement type '" ~ statementNode.name ~ "'");
            throw new Exception("Invalid statement type '" ~ statementNode.name ~ "'");
        }
    }

    private void evalVariableAssignmentNode(ref ProExecutionContext context,
            ref ParseTree statementNode) const
    {
        auto variableName = statementNode.matches[0];
        auto variableOperator = statementNode.matches[1];
        auto variableValue = statementNode.matches[2 .. $];
        trace("Variable name: '" ~ variableName ~ "'");
        trace("Variable assignment operator: '" ~ variableOperator ~ "'");
        trace("Variable old raw value: ", context.getVariableRawValue(variableName));
        trace("Variable new raw value: ", variableValue);

        assignVariable(context, variableName, variableOperator, variableValue);
    }

    private void assignVariable(ref ProExecutionContext context, in string name,
            in string operator, in string[] value) const
    {
        string[] result;
        if (!value.empty)
        {
            auto evaluator = new ExpressionEvaluator(context);
            auto rpnExpression = convertToRPN(value.join(" "));
            result = evaluator.evalRPN(rpnExpression);
            trace("Variable new value: ", result);
        }

        switch (operator)
        {
        case STR_EQUALS:
            context.assignVariable(name, result, VariableType.STRING_LIST /*FIXME:*/ );
            break;
        case STR_PLUS_EQUALS:
            context.appendAssignVariable(name, result);
            break;
        case STR_ASTERISK_EQUALS:
            context.appendUniqueAssignVariable(name, result);
            break;
        case STR_MINUS_EQUALS:
            context.removeAssignVariable(name, result);
            break;
        case STR_TILDE_EQUALS:
            throw new Exception("Not implemented yet");
        default:
            throw new Exception("Invalid assignment operator '" ~ operator ~ "'");
        }
    }

    private void evalScopeNode(ref ProExecutionContext context, ref ParseTree statementNode) /+const+/
    {
        trace("\n");

        ParseTree scopeConditionNode = statementNode.children[0];
        ParseTree scopeIfTrueBranch = statementNode.children[1];
        trace("Condition: ", scopeConditionNode);
        trace("If-true: ", scopeIfTrueBranch);

        // 1) Eval main scope branch condition (true or false)
        if (evalScopeConditionNode(context, scopeConditionNode))
        {
            trace("Scope condition '", scopeConditionNode.matches.join(" "), "' eval'd to true");

            // 2) Eval main scope branch statements (single- or multi-line code block)
            switch (scopeIfTrueBranch.matches[0])
            {
            case STR_DOG:
                // trace("Single-line code block: ", scopeIfTrueBranch.matches[1 .. $]);
                auto singlelineBlockNode = scopeIfTrueBranch.children[0].children[0];
                assert(singlelineBlockNode.children.length == 1);
                assert(singlelineBlockNode.name == "QMakeProject.SingleLineBlock");
                auto singlelineBlockStatementNode = singlelineBlockNode.children[0];
                assert(singlelineBlockStatementNode.name == "QMakeProject.Statement");
                assert(singlelineBlockStatementNode.children.length == 1);
                auto singlelineBlockRawStatementNode = singlelineBlockStatementNode.children[0];
                evalStatementNode(context, singlelineBlockRawStatementNode);
                break;
            case STR_OPENING_CURLY_BRACE:
                if (scopeIfTrueBranch.matches[$ - 1] != STR_CLOSING_CURLY_BRACE)
                    throw new Exception(
                            "Invalid multi-line code block: closing curly brace is absent");

                auto multilineBlockNode = scopeIfTrueBranch.children[0].children[0];
                assert(multilineBlockNode.name == "QMakeProject.MultiLineBlock");
                trace("Multi-line block statement count: ", multilineBlockNode.children.length);
                for (int i = 0; i < multilineBlockNode.children.length; i++)
                {
                    auto blockStatementNode = multilineBlockNode.children[i];
                    trace("BLOCK_STATEMENT_NAME: ", blockStatementNode.name);
                    trace("Statement [", i, "]: ", blockStatementNode.matches);
                    evalStatementNode(context, blockStatementNode.children[0]);
                }

                //trace("Multi-line code block: ", multilineBlockNode.matches);
                break;
            default:
                throw new Exception("Invalid code block start token");
            }
        }
        else
        {
            // FIXME: 3) Eval optional else-if branch conditions
            // FIXME: 4) Eval optional else-branch statements
            /+ParseTree scopeElseIfBranch;
            ParseTree scopeElseBranch;
            if (statementNode.children.length >= 3)
                throw new Exception("Not implemented yet");+/
            if (statementNode.children.length >= 3)
                error("\nELSE-IF/ELSE: NOT IMPLEMENTED YET\n");
            else
                trace("Scope condition '", scopeConditionNode.matches.join(" "), "' eval'd to FALSE");
        }
    }

    private bool evalScopeConditionNode(ref ProExecutionContext context, ref ParseTree statementNode) /+const+/
    {
        assert(statementNode.name == "QMakeProject.BooleanExpression");
        assert(statementNode.children.length == 1);

        auto orStatementNode = statementNode.children[0];
        assert(orStatementNode.children.length >= 1);

        bool orResult /*= false*/;
        for (int i /*= 0*/; i < orStatementNode.children.length; i++)
        {
            auto andStatementNode = orStatementNode.children[i];
            assert(andStatementNode.name == "QMakeProject.LogicalANDExpression");
            assert(andStatementNode.children.length >= 1);

            bool andResult /*= false*/;
            for (int j /*= 0*/; j < andStatementNode.children.length; j++)
            {
                auto notStatementNode = andStatementNode.children[j];
                assert(notStatementNode.name == "QMakeProject.LogicalNOTExpression");
                assert(notStatementNode.children.length == 1);

                // Apply NOT boolean operator
                bool notMarker;
                //assert(notStatementNode.matches.length == 1 || notStatementNode.matches.length == 2);
                trace(notStatementNode.matches);
                if (notStatementNode.matches[0] == STR_EXCLAMATION_MARK)
                {
                    trace("NOT-expression found");
                    notMarker = true;
                }

                auto boolMetaExprNode = notStatementNode.children[0];
                assert(boolMetaExprNode.name == "QMakeProject.PrimaryBooleanExpression");
                assert(boolMetaExprNode.children.length == 1);

                auto boolExprNode = boolMetaExprNode.children[0];
                if (j == 0)
                {
                    andResult = evalBooleanExressionNode(context, boolExprNode);
                    // FIXME:
                    if (notMarker)
                    {
                        trace("Apply NOT 1: '", andResult, "' --> '", !andResult, "'");
                        andResult = !andResult;
                    }
                }
                else
                {
                    andResult = andResult && evalBooleanExressionNode(context, boolExprNode);
                    if (!andResult)
                    {
                        trace("operand [", j, "] of AND-expression is FALSE, aborting");
                        // FIXME:
                        if (notMarker)
                        {
                            trace("Apply NOT 2: '", andResult, "' --> '", !andResult, "'");
                            andResult = !andResult;
                        }
                        break;
                    }
                }
            }
            trace("AND-expression result: ", andResult);

            orResult = orResult || andResult;
        }
        trace("OR-expression result: ", orResult);

        return orResult;
    }

    private bool evalBooleanExressionNode(ref ProExecutionContext context, ref ParseTree boolExprNode) /+const+/
    {
        assert(boolExprNode.children.length == 1);
        switch (boolExprNode.name)
        {
        case "QMakeProject.ParenthesedBooleanExpression":
            throw new Exception("Not implemented");
        case "QMakeProject.IfTestFunctionCall":
            throw new Exception("Not implemented");
        case "QMakeProject.BooleanAtom":
            // BooleanAtom <- ReplaceFunctionCall / TestFunctionCall / QualifiedIdentifier / BooleanConst
            auto boolAtomNode = boolExprNode.children[0];
            switch (boolAtomNode.name)
            {
            case "QMakeProject.ReplaceFunctionCall":
                throw new Exception("Not implemented yet");
            case "QMakeProject.TestFunctionCall":
                return evalTestFunctionNode(context, boolAtomNode);
            case "QMakeProject.QualifiedIdentifier":
                return evalBooleanVariableNode(context, boolAtomNode);
            case "QMakeProject.BooleanConst":
                throw new Exception("Not implemented yet");
            default:
                throw new Exception("Unknown boolean atom type");
            }
            //break;
        default:
            throw new Exception("Unknown boolean meta-expression type");
        }

       // return false;
    }

    private bool evalTestFunctionNode(ref ProExecutionContext context, ref ParseTree testFunctionNode) /+const+/
    {
        assert(testFunctionNode.name == "QMakeProject.TestFunctionCall");
        assert(testFunctionNode.children.length == 1);

        // FunctionCall <- FunctionId OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS :space* :eol*
        auto functionNode = testFunctionNode.children[0];
        assert(functionNode.name == "QMakeProject.FunctionCall"
            || functionNode.name == "QMakeProject.ContainsTestFunctionCall");
        // assert(functionNode.children.length == 4);
        string functionName = functionNode.children[0].matches[0];
        if (functionNode.name == "QMakeProject.ContainsTestFunctionCall")
        {
            functionName = "contains";
        }
        trace("Eval function '", functionName, "'...");

        // FIXME: looks hacky :( is there another way to break circular dependency project_function <--> eval?
        if (functionName == "include")
        {
            assert(functionNode.children.length == 4);
            assert(functionNode.children[1].matches[0] == STR_OPENING_PARENTHESIS);
            assert(functionNode.children[3].matches[0] == STR_CLOSING_PARENTHESIS);

            string projectFileName = functionNode.children[2].matches[0];
			string projectDirectory = context.getVariableRawValue("PWD")[0];
			if (!isAbsolute(projectFileName))
				projectFileName = buildNormalizedPath(absolutePath(projectFileName, projectDirectory));
			trace("absolute project path: '", projectFileName, "'");
			if (!exists(projectFileName) || !isFile(projectFileName)) {
				error("project file '", projectFileName, "' was not found, so return 'false'");
				return false;
			}

			auto pro = new Project(context);
    		if (pro.eval(projectFileName))
    		{
        		info("qmake project file '" ~ projectFileName ~ "' was successfully parsed");
        		return true;
    		}
            return false;
        }
        else if (functionName == "load")
        {
            // FIXME: implement
            trace("include: --- NOT IMPLEMENTED YET ---");
            return false;
        }
        else if (functionName == "contains"
              || functionName == "message" || functionName == "warning" || functionName == "error")
        {
            auto evaluator = new ExpressionEvaluator(context);
            auto rpnExpression = convertToRPN(functionNode.matches.join("") /*.replace(",", ", ")*/ );
            auto rpnResult = evaluator.evalRPN(rpnExpression);
            trace("Function '", functionName, "' result = ", rpnResult);
            if (functionName == "error")
                throw new Exception("User-defined error raised");
            return (rpnResult[0] == "true");
        }
        else
        {
            auto evaluator = new ExpressionEvaluator(context);
            // FIXME: whitespaces must not change during conversion! but here we just eliminate them all
            auto rpnExpression = convertToRPN(functionNode.matches.join(" "));
            auto rpnResult = evaluator.evalRPN(rpnExpression);
            trace("Function '", functionName, "' result = ", rpnResult);
            return (rpnResult[0] == "true");
        }
    }

    private bool evalBooleanVariableNode(ref ProExecutionContext context, ref ParseTree boolAtomNode) const
    {
        assert(boolAtomNode.children.length == 0);
        assert(boolAtomNode.matches.length == 1);

        string variableValue = boolAtomNode.matches[0];

        // Boolean const "true"/"false" as in C++
        if (variableValue == "true")
        {
            trace("'true' boolean constant --> condition is true");
            return true;
        }
        if (variableValue == "false")
        {
            trace("'false' boolean constant --> condition is FALSE");
            return false;
        }

        string[] platformValue = context.getVariableRawValue("QMAKE_PLATFORM");
        string[] configValue = context.getVariableRawValue("CONFIG");
        string[] specValue = context.getVariableRawValue("QMAKESPEC");
        immutable(string) spec = baseName(specValue[0]);
        if (platformValue.countUntil(variableValue) >= 0)
        {
            trace("'", variableValue, "' belongs to QMAKE_PLATFORM --> condition is true");
            return true;
        }
        if (configValue.countUntil(variableValue) >= 0)
        {
            trace("'", variableValue, "' belongs to CONFIG --> condition is true");
            return true;
        }

        if (spec == variableValue)
        {
            trace("'", variableValue, "' belongs to QMAKESPEC --> condition is true");
            return true;
        }

        trace("'", variableValue,
                "' not belongs to QMAKE_PLATFORM/CONFIG/QMAKESPEC --> condition is FALSE");
        return false;
    }
}

unittest
{
    auto pro = new Project();

    assert(pro.tryParse("tests/variables.pro"));
    assert(pro.tryParse("tests/contains.pro"));
    assert(pro.tryParse("tests/eval.pro"));
    assert(pro.tryParse("tests/test_function_call_1.pro"));
    assert(pro.tryParse("tests/test_function_call_2.pro"));
    assert(pro.tryParse("tests/block_1.pro"));
    assert(pro.tryParse("tests/block_2.pro"));
    assert(pro.tryParse("tests/block_3.pro"));
    assert(pro.tryParse("tests/block_4.pro"));
    assert(pro.tryParse("tests/scope_1.pro"));
    assert(pro.tryParse("tests/scope_2.pro"));
    assert(pro.tryParse("tests/scope_3.pro"));
    assert(pro.tryParse("tests/scope_4.pro"));
    assert(pro.tryParse("tests/scope_5.pro"));
    assert(pro.tryParse("tests/scope_6.pro"));
    assert(pro.tryParse("tests/qt_parts.prf"));
    assert(pro.tryParse("tests/default_post.prf"));
    assert(pro.tryParse("tests/qml_module.prf"));
    assert(pro.tryParse("tests/android-base-head.conf"));
    assert(pro.tryParse("tests/qt_module_headers.prf"));
    assert(pro.tryParse("tests/qt_functions.prf"));
    assert(pro.tryParse("tests/uikit_qt.prf"));
    assert(pro.tryParse("tests/compositor_api.pri"));
    assert(pro.tryParse("tests/linux-clang-qmake.conf"));
}
