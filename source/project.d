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
import qmakeexception;
import qmakeparser;

import common_const;
import eval;
import project_variable;
import project_context;
import persistent_property;
import rpn;

public class Project
{
    private ProExecutionContext m_context;
    private PersistentPropertyStorage m_persistentStorage;

    public this(ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage)
    {
        m_context = context;
        m_persistentStorage = persistentStorage;
    }

    public void dump() /*const*/
    {
        // FIXME: distinguish unset variables from empty ones!!!

        trace("\n\nqmake built-in variable values:");
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
            {
                trace(variableName, " = ", variableValue);
            }
        }
        trace("\n\n");
    }

    public bool tryParseSnippet(in string snippet)
    {
        LineInfo[] li;
        string preprocessedSnippet = preprocessLines(splitLines(snippet), li);

        auto parseTree = QMakeProject(preprocessedSnippet);
        if (!parseTree.successful)
        {
            trace(parseTree);
//            trace("Parsing project file '" ~ fileName ~ "' FAILED");
        }
//        else
//            trace("Project file '" ~ fileName ~ "' successfully parsed");

        return parseTree.successful;
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
            trace("Parsing project file '" ~ fileName ~ "' FAILED");
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
            // FIXME: implement
            error("NOT IMPLEMENTED YET");
            break;
        case "QMakeProject.ForStatement":
            evalForStatement(context, statementNode);
            break;
        default:
            trace(statementNode);
            error("Invalid statement type '" ~ statementNode.name ~ "'");
            throw new Exception("Invalid statement type '" ~ statementNode.name ~ "'");
        }
    }

    private void evalForStatement(ref ProExecutionContext context, ref ParseTree forNode)
    {
        trace("");

        assert(forNode.name == "QMakeProject.ForStatement");
        assert(forNode.children.length == 6);

        // Get iterator variable name
        auto variableNameMetaNode = forNode.children[1];
        assert(variableNameMetaNode.name == "QMakeProject.ForIteratorVariableName");
        assert(variableNameMetaNode.children.length == 1);
        auto variableNameNode = variableNameMetaNode.children[0];
        assert(variableNameNode.name == "QMakeProject.QualifiedIdentifier");
        assert(variableNameNode.children.length == 0);
        assert(variableNameNode.matches.length == 1);
        string variableName = variableNameNode.matches[0];
        trace("For iterator variable name: ", variableName);

        // Eval string list to iterate on
        string[] iterableList;
        auto listMetaNode = forNode.children[3];
        assert(listMetaNode.name == "QMakeProject.ForIterableList");
        assert(listMetaNode.children.length == 1);
        auto listNode = listMetaNode.children[0];
        assert(listNode.name == "QMakeProject.FunctionFirstArgument" || listNode.name == "QMakeProject.List");
        switch (listNode.name)
        {
            case "QMakeProject.FunctionFirstArgument":
                string value = listNode.matches.join("");
                auto evaluator = new ExpressionEvaluator(context, m_persistentStorage);
                auto rpnExpression = convertToRPN(value);
                auto result = evaluator.evalRPN(rpnExpression);
                trace("List variable name: ", result);
                if (!context.isVariableDefined(result[0]))
                    throw new Exception("Undefined list variable '" ~ result[0] ~ "', aborting");
                iterableList = context.getVariableRawValue(result[0]);
                assert(iterableList.length >= 1);
                trace("List to iterate on: ", iterableList);
                break;
            default:
                throw new Exception("Unknown type of for-iterable list");
        }

        auto blockMetaNode = forNode.children[5];
        assert(blockMetaNode.name == "QMakeProject.Block");

        foreach (listValue; iterableList)
        {
            // Declare local counter variable
            context.assignVariable(variableName, [listValue], VariableType.STRING);

            evalBlock(context, blockMetaNode);
        }

        // Unset the already unneeded variable
        context.unsetVariable(variableName);

        trace("");
    }

    private void evalBlock(ref ProExecutionContext context, ref ParseTree bodyNode)
    {
        assert(bodyNode.name == "QMakeProject.Block");
        assert(bodyNode.children.length == 1);

        /*immutable*/ auto blockNode = bodyNode.children[0];
        if (blockNode.name == "QMakeProject.SingleLineBlock")
            // FIXME: add checks for children count
            evalStatementNode(context, blockNode.children[0].children[0]);
        else if (blockNode.name == "QMakeProject.MultiLineBlock")
            evalMultilineBlockNode(context, blockNode);
        else
            throw new Exception("Unknown code block type");
    }

    private void evalMultilineBlockNode(ref ProExecutionContext context, ref ParseTree multilineBlockNode) /+const+/
    {
        assert(multilineBlockNode.name == "QMakeProject.MultiLineBlock");
        assert(multilineBlockNode.children.length >= 1);
        trace("Multi-line block statement count: ", multilineBlockNode.children.length);
        trace("{");
        for (int i; i < multilineBlockNode.children.length; i++)
        {
            auto blockStatementNode = multilineBlockNode.children[i];
            trace("Eval statement [", i, "] of type ", blockStatementNode.children[0].name);

            evalStatementNode(context, blockStatementNode.children[0]);
        }
        trace("}");
    }

    private void evalVariableAssignmentNode(ref ProExecutionContext context,
            ref ParseTree statementNode) /+const+/
    {
        auto variableName = statementNode.matches[0];
        auto variableOperator = statementNode.matches[1];
        auto variableValue = statementNode.matches[2 .. $];
        trace("Variable name: '" ~ variableName ~ "'");
        trace("Variable assignment operator: '" ~ variableOperator ~ "'");

        if (context.isVariableDefined(variableName))
            trace("Variable old raw value: ", context.getVariableRawValue(variableName));
        trace("Variable new raw value: ", variableValue);

        assignVariable(context, variableName, variableOperator, variableValue);
    }

    private void assignVariable(ref ProExecutionContext context, in string name,
            in string operator, in string[] value) /+const+/
    {
        string[] result;
        if (!value.empty)
        {
            auto evaluator = new ExpressionEvaluator(context, m_persistentStorage);
            auto rpnExpression = convertToRPN(value.join(" "));
            result = evaluator.evalRPN(rpnExpression);
            trace("Variable '", name, "' new value: ", result);
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
            // FIXME: implement
            throw new Exception("Not implemented yet");
        default:
            throw new Exception("Invalid assignment operator '" ~ operator ~ "'");
        }
    }

    private void evalScopeNode(ref ProExecutionContext context, ref ParseTree statementNode) /+const+/
    {
        ParseTree scopeConditionNode = statementNode.children[0];
        ParseTree scopeIfTrueBranch = statementNode.children[1];

        if (evalScopeConditionNode(context, scopeConditionNode))
        {
            trace("Scope main condition '", scopeConditionNode.matches.join(" "),
                "' evaluated to TRUE, select main branch code path");
            evalBlock(context, scopeIfTrueBranch.children[0]);
            return;
        }

        for (int i = 2; i < statementNode.children.length; i++)
        {
            // Scope             <- BooleanExpression ScopeMainBranch ScopeElseIfBranch* ScopeElseBranch?
            // ScopeMainBranch   <- Block
            // ScopeElseIfBranch <- "else@" :space* BooleanExpression Block
            // ScopeElseBranch   <- "else@" :space* Statement
            //                    / "else"  MultiLineBlock
            // Block             <- SingleLineBlock / MultiLineBlock
            switch(statementNode.children[i].name)
            {
                case "QMakeProject.ScopeElseIfBranch":
                    assert(statementNode.children[i].children.length == 2);

                    auto conditionNode = statementNode.children[i].children[0];
                    assert(conditionNode.name == "QMakeProject.BooleanExpression");

                    // FIXME: eval and use condition
                    if (evalScopeConditionNode(context, conditionNode))
                    {
                        trace("Scope else-if condition '", conditionNode.matches.join(" "),
                            "' evaluated to TRUE, select else-if branch code path");
                        evalBlock(context, statementNode.children[i].children[1]);
                        return;
                    }
                    break;
                case "QMakeProject.ScopeElseBranch":
                    // NOTE: else-branch must be the last one
                    assert(i == statementNode.children.length - 1);
                    assert(statementNode.children[i].children.length == 1);

                    trace("Scope main and all else-if conditions '", scopeConditionNode.matches.join(" "),
                        "' evaluated to FALSE, select else branch code path");

                    auto bodyNode = statementNode.children[i].children[0];
                    if (bodyNode.name == "QMakeProject.Statement")
                    {
                        evalStatementNode(context, bodyNode.children[0]);
                    }
                    else if (bodyNode.name == "QMakeProject.MultiLineBlock")
                    {
                        evalMultilineBlockNode(context, bodyNode);
                    }
                    else
                    {
                        throw new Exception("Invalid node type in else-if branch");
                    }
                    break;
                default:
                    throw new Exception("Invalid scope node type");
            }
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
            // FIXME: implement
            throw new Exception("Not implemented");
        case "QMakeProject.IfTestFunctionCall":
            // FIXME: implement
            throw new Exception("Not implemented");
        case "QMakeProject.BooleanAtom":
            // BooleanAtom <- ReplaceFunctionCall / TestFunctionCall / QualifiedIdentifier / BooleanConst
            auto boolAtomNode = boolExprNode.children[0];
            switch (boolAtomNode.name)
            {
            case "QMakeProject.ReplaceFunctionCall":
                // FIXME: implement
                throw new Exception("Not implemented yet");
            case "QMakeProject.TestFunctionCall":
                return evalTestFunctionNode(context, boolAtomNode);
            case "QMakeProject.QualifiedIdentifier":
                return evalBooleanVariableNode(context, boolAtomNode);
            case "QMakeProject.BooleanConst":
                // FIXME: implement
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
        if (functionName == "include" || functionName == "load")
        {
            assert(functionNode.children.length == 4);
            assert(functionNode.children[1].matches[0] == STR_OPENING_PARENTHESIS);
            assert(functionNode.children[3].matches[0] == STR_CLOSING_PARENTHESIS);

            string projectFileName = functionNode.children[2].matches[0];
            if (functionName == "include")
            {
                auto argumentsNode = functionNode.children[2];
                assert(argumentsNode.name == "QMakeProject.FunctionArgumentList");
                assert(argumentsNode.children.length == 1);
                auto listNode = argumentsNode.children[0];
                assert(listNode.children.length >= 1);
                projectFileName = listNode.children[0].matches.join("");
                auto evaluator = new ExpressionEvaluator(context, m_persistentStorage);
                auto rpnExpression = convertToRPN(projectFileName);
                auto rpnResult = evaluator.evalRPN(rpnExpression);
                projectFileName = rpnResult[0];

                // FIXME: implement other argument usage
                if (listNode.children.length >= 2)
                    warning("'include' test function optional arguments not implemented yet");

			    string projectDirectory = context.getVariableRawValue("PWD")[0];
			    if (!isAbsolute(projectFileName))
				    projectFileName = buildNormalizedPath(absolutePath(projectFileName, projectDirectory));
			    trace("absolute project path: '", projectFileName, "'");
			    if (!exists(projectFileName) || !isFile(projectFileName))
                {
				    error("project file '", projectFileName, "' was not found, so return FALSE");
				    return false;
			    }
            }
            else
            {
                // FIXME: use PlatformInfo class, need to extract it from app.d first
                string featureDirectory = "/opt/Qt/5.11.1/gcc_64/mkspecs/features";
                projectFileName = buildNormalizedPath(featureDirectory, projectFileName);
                projectFileName = std.path.setExtension(projectFileName, "prf");
                if (!std.file.exists(projectFileName) || !std.file.isFile(projectFileName))
                {
                    error("feature file '", projectFileName, "' was not found or not a file, so return FALSE");
                    assert(false);
                    //return false;
                }
            }

			auto pro = new Project(context, m_persistentStorage);
    		if (!pro.eval(projectFileName))
    		{
                error("qmake project file '", projectFileName, "' evaluation failed");
        		return false;
    		}
            info("qmake project file '" ~ projectFileName ~ "' was successfully parsed");
        	return true;
        }
        else if (functionName == "contains"
              || functionName == "debug"   || functionName == "message"
              || functionName == "warning" || functionName == "error")
        {
            auto evaluator = new ExpressionEvaluator(context, m_persistentStorage);
            auto rpnExpression = convertToRPN(functionNode.matches.join("") /*.replace(",", ", ")*/ );
            auto rpnResult = evaluator.evalRPN(rpnExpression);
            trace("Function '", functionName, "' result = ", rpnResult);
            if (functionName == "error")
                throw new Exception("User-defined error raised");
            return (rpnResult[0] == "true");
        }
        else
        {
            auto evaluator = new ExpressionEvaluator(context, m_persistentStorage);
            auto rpnExpression = convertToRPN(functionNode.matches);
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
    auto context = new ProExecutionContext();
    auto storage = new PersistentPropertyStorage();
    auto pro = new Project(context, storage);

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

     assert(pro.tryParseSnippet(
`
for(_, $$list(_)) { # just a way to break easily
    isEmpty(FORCE_MINGW_QDOC_BUILD): FORCE_MINGW_QDOC_BUILD = $$(FORCE_MINGW_QDOC_BUILD)
    equals(QMAKE_HOST.os, Windows):gcc:isEmpty(FORCE_MINGW_QDOC_BUILD) {
            log("QDoc build is disabled on MinGW in Qt 5.11.0, because of a missing feature in the release infrastructure.(\\n)")
            log("You can enable it by setting FORCE_MINGW_QDOC_BUILD")
            break()
    }
}
`));

    // Qt projects
    // FIXME: detect Qt path during run-time
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qt3d/tests/auto/render/render.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qttools/mkspecs/features/qt_find_clang.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtwebengine/mkspecs/features/configure.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtwebengine/mkspecs/features/platform.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtmultimedia/examples/multimedia/multimedia.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtremoteobjects/tests/auto/integration_multiprocess/server/server.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtbase/tests/auto/network/ssl/ssl.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtbase/src/plugins/sqldrivers/configure.pri"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtbase/src/widgets/util/util.pri"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtbase/mkspecs/features/spec_post.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtbase/mkspecs/features/winrt/package_manifest.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtbase/mkspecs/features/qt_configure.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtdeclarative/tests/auto/quick/pointerhandlers/pointerhandlers.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.1/Src/qtdeclarative/tests/auto/qml/qml.pro"));

    int parseQtSourceProjects(string qtDir)
    {
        import std.stdio : writefln;
        import std.conv : to;

        auto projectFiles = std.file.dirEntries(qtDir, std.file.SpanMode.depth).filter!(
            f => f.name.endsWith(".pro") || f.name.endsWith(".pri") || f.name.endsWith(".prf") || f.name.endsWith(".conf")
        );
    
        int successfulCount, failedCount;
        string[] failedProjects;
        foreach (d; projectFiles)
        {
            if (d.name.indexOf("qtbase/qmake/doc/snippets/") != -1
                || d.name.indexOf("qtdoc/doc/src/snippets/") != -1
                || d.name.indexOf("qtdoc/doc/snippets/") != -1
                || d.name.indexOf("activeqt/doc/snippets/") != -1
                || d.name.indexOf("qtxmlpatterns/src/xmlpatterns/doc/snippets/") != -1
                || d.name.indexOf("qtscript/src/script/doc/snippets/") != -1
                || d.name.indexOf("qtscript/src/scripttools/doc/snippets/") != -1
                || d.name.indexOf("qttools/src/designer/src/designer/doc/snippets/") != -1
                || d.name.indexOf("qttools/src/designer/src/uitools/doc/snippets/") != -1
                || d.name.indexOf("qttools/src/linguist/linguist/doc/snippets/") != -1
                || d.name.indexOf("qttools/examples/designer/doc/snippets/") != -1
                || d.name.indexOf("qtdatavis3d/src/datavisualization/doc/snippets/") != -1
                || d.name.indexOf("qtsvg/src/svg/doc/snippets/") != -1
                || d.name.indexOf("qtquickcontrols2.conf") != -1
                || d.name.indexOf("Sensors.conf") != -1
                || d.name.indexOf("3rdparty/chromium") != -1
                || d.name.indexOf("shared/deviceskin/") != -1
                )
            {
                trace("Skipping documentation snippet...");
                continue;
            }

            if (d.name.indexOf("tests/auto/qquickstyle/data/") != -1)
            {
                trace("Skipping Qt Quick style configuration file...");
                continue;
            }

            auto context = new ProExecutionContext();
            auto prop = new PersistentPropertyStorage();
		    auto pro = new Project(context, prop);
            if (pro.tryParse(d.name))
            {
                successfulCount++;
            }
            else
            {
                failedCount++;
                failedProjects ~= d.name;
            }
        }

        immutable int totalCount = successfulCount + failedCount;
        writefln("Total file count: " ~ to!string(totalCount));
        writefln("Successfully parsed: " ~ to!string(successfulCount));
        writefln("Failed to parse: " ~ to!string(failedCount) ~ " or "
            ~ to!string(100 * failedCount / totalCount) ~ "%%");
        if (failedCount > 0)
        {
            writefln("");
            writefln("Failed projects:");
            foreach (project; failedProjects)
                writefln(project);
        }

        return (failedCount == 0) ? 0 : 1;
    }
    assert(parseQtSourceProjects("/opt/Qt/5.11.1/Src") == 0);
}
