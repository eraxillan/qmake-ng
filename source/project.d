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

import std.typecons;
import std.conv;
import std.file;
import std.path;
import std.string;
import std.algorithm;
import std.process;

import std.experimental.logger;

import preprocessor;
import qmakeexception;
import qmakeparser;

import common_const;
import common_utils;
import project_variable;
import project_function;
import project_context;
import persistent_property;
import type_deduction;


public class Project
{
    // NOTE: variables defined in user-defined qmake functions must be local to them
    alias ContextStack = QStack!ProExecutionContext;
    private ContextStack m_contextStack;

    private PersistentPropertyStorage m_persistentStorage;

    public this(ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage)
    {
        m_contextStack = new ContextStack;
        m_contextStack.push(context);

        m_persistentStorage = persistentStorage;
    }

    public void dump() /*const*/
    {
        // FIXME: distinguish unset variables from empty ones!!!

        trace("\n\nqmake built-in variable values:");
        foreach (variableName; m_contextStack.top().getBuiltinVariableNames())
        {
            string[] variableValue = m_contextStack.top().getVariableRawValue(variableName);
            if (!variableValue.empty)
                trace(variableName, " = ", variableValue);
        }
        
        trace("\n\nqmake user-defined variable values:");
        foreach (variableName; m_contextStack.top().getUserDefinedVariableNames())
        {
            string[] variableValue = m_contextStack.top().getVariableRawValue(variableName);
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
        m_contextStack.top().setupPaths(fileName);

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

            evalStatementNode(statementNode);
        }

        // Output all built-in and user-defined variables
        //dump();

        trace("Project file '" ~ fileName ~ "' successfully evaluated");
        return true;
    }

    // ----------------------------------------------------------------------------------------------------------------------------------------------

    private void evalStatementNode(ref ParseTree statementNode) /+const+/
    {
        /*
        Statement <- | FunctionDeclaration
                     | Assignment
                     | ForStatement
                     | Scope
                     | Block
                     | ReplaceFunctionCall
                     | TestFunctionCall
                     | BooleanExpression
                     | Comment
                     | EmptyStatement
        */
        switch (statementNode.name)
        {
            case "QMakeProject.FunctionDeclaration":
            {
                evalFunctionDeclarationNode(statementNode);
                break;
            }
            case "QMakeProject.Assignment":
            {
                evalVariableAssignmentNode(statementNode);
                break;
            }
            case "QMakeProject.ForStatement":
            {
                evalForStatementNode(statementNode);
                break;
            }
            case "QMakeProject.Scope":
            {
                evalScopeNode(statementNode);
                break;
            }
            case "QMakeProject.Block":
            {
                evalBlock(statementNode);
                break;
            }
            case "QMakeProject.ReplaceFunctionCall":
            {
                // NOTE: replace function designed to be used as rvalue;
                //       however, it result can be ignored with just a warning
                //
                // FIXME: get function name and line/column in source
                warning("Replace function result ignored");
                evalReplaceFunctionNode(statementNode);
                break;
            }
            case "QMakeProject.TestFunctionCall":
            {
                // NOTE: also ignore function result, but without warning
                evalTestFunctionNode(statementNode);
                break;
            }
            case "QMakeProject.BooleanExpression":
            {
                evalScopeConditionNode(statementNode);
                break;
            }
            case "QMakeProject.Comment":
            {
                trace("Comment was ignored");
                break;
            }
            case "QMakeProject.EmptyStatement":
            {
                trace("Empty statement was ignored");
                break;
            }
            default:
            {
                trace(statementNode);
                error("Invalid statement type '" ~ statementNode.name ~ "'");
                throw new Exception("Invalid statement type '" ~ statementNode.name ~ "'");
            }
        }
    }

    private void evalFunctionDeclarationNode(ref ParseTree declNode)
    in
    {
        assert(declNode.name.startsWith("QMakeProject.FunctionDeclaration"));
        assert(declNode.children.length == 1);
    }
    do
    {
        auto concreteDeclNode = declNode.children[0];
        assert(concreteDeclNode.children.length == 4);
        switch (concreteDeclNode.name)
        {
            case "QMakeProject.ReplaceFunctionDeclaration":
            {
                // alias Action = const(string[]) function(ref ProExecutionContext context, in string[] arguments);
                auto nameNode = concreteDeclNode.children[1];
                assert(nameNode.children.length == 0);
                assert(nameNode.matches.length == 1);

                string name = nameNode.matches[0];
                trace("Declare user-defined replace function ", "`", name, "`");

                auto functionBlock = concreteDeclNode.children[3];
                const(string[]) replaceAction(ref ProExecutionContext /*context*/, in string[] arguments)
                {
                    trace("Invoking user-defined replace function ", "`", name, "`");

                    const(string[]) result = [""];

                    m_contextStack.push(new ProExecutionContext());

                    // Declare function arguments like $${1} etc.
                    for (int i = 0; i < arguments.length; i++)
                    {
                        immutable string argName = to!string(i + 1);
                        trace("Declare function argument variable `$${", argName, "}` = `", arguments[i], "`");
                        m_contextStack.top().assignVariable(argName, [arguments[i]], VariableType.STRING);
                    }
                    // NOTE: return() call is optional in vanilla qmake;
                    //       so we need to add an implicit `return(true)` for test functions
                    //       and `return("")` for replace ones
                    //
                    // FIXME: implement this stuff
                    //

                    assert(functionBlock.name == "QMakeProject.Block");
                    assert(functionBlock.children.length == 1);

                    auto blockNode = functionBlock.children[0];
                    if (blockNode.name == "QMakeProject.SingleLineBlock")
                        evalStatementNode(blockNode.children[0].children[0]);
                    else if (blockNode.name == "QMakeProject.MultiLineBlock")
                        evalMultilineBlockNode(blockNode);
                    else
                        throw new Exception("Unknown code block type");
                    
                    //
                    // FIXME: merge all global variables from internal function context
                    //
                    m_contextStack.pop();

                    trace("User-defined replace function result: ", result);
                    return result;
                }
                m_contextStack.top().addReplaceFunctionDescription(name.dup, &replaceAction);
                break;
            }
            case "QMakeProject.TestFunctionDeclaration":
            {
                auto nameNode = concreteDeclNode.children[1];
                assert(nameNode.children.length == 0);
                assert(nameNode.matches.length == 1);
                string name = nameNode.matches[0];
                trace("Declare user-defined test function ", "`", name, "`");

                auto functionBlock = concreteDeclNode.children[3];
                const(string[]) testAction(ref ProExecutionContext /*context*/, in string[] arguments)
                {
                    trace("Invoking user-defined test function ", "`", name, "`");

                    const(string[]) result = ["true"];

                    m_contextStack.push(new ProExecutionContext());

                    // Declare function arguments like $${1} etc.
                    for (int i = 0; i < arguments.length; i++)
                    {
                        immutable string argName = to!string(i + 1);
                        trace("Declare function argument variable `$${", argName, "}` = `", arguments[i], "`");
                        m_contextStack.top().assignVariable(argName, [arguments[i]], VariableType.STRING);
                    }
                    // NOTE: return() call is optional in vanilla qmake;
                    //       so we need to add an implicit `return(true)` for test functions
                    //       and `return("")` for replace ones
                    //
                    // FIXME: implement this stuff
                    //

                    assert(functionBlock.name == "QMakeProject.Block");
                    assert(functionBlock.children.length == 1);

                    auto blockNode = functionBlock.children[0];
                    if (blockNode.name == "QMakeProject.SingleLineBlock")
                        evalStatementNode(blockNode.children[0].children[0]);
                    else if (blockNode.name == "QMakeProject.MultiLineBlock")
                        evalMultilineBlockNode(blockNode);
                    else
                        throw new Exception("Unknown code block type");
                    
                    //
                    // FIXME: merge all global variables from internal function context
                    //
                    m_contextStack.pop();

                    trace("User-defined test function result: ", result);
                    return result;
                }
                m_contextStack.top().addTestFunctionDescription(name.dup, &testAction);
                break;
            }
            default: throw new NotImplementedException("");
        }
    }

    private void evalForStatementNode(ref ParseTree forNode)
    in
    {
        assert(forNode.name == "QMakeProject.ForStatement");
        assert(forNode.children.length == 1);
    }
    do
    {
        trace("");

        auto forSubtypeNode = forNode.children[0];
        assert(forSubtypeNode.name == "QMakeProject.ForEachInListStatement"
            || forSubtypeNode.name == "QMakeProject.ForEverStatement");

        if (forSubtypeNode.name == "QMakeProject.ForEachInListStatement")
        {
            assert(forSubtypeNode.children.length == 6);

            // Get iterator variable name
            auto variableNameMetaNode = forSubtypeNode.children[1];
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
            auto listMetaNode = forSubtypeNode.children[3];
            assert(listMetaNode.name == "QMakeProject.ForIterableList");
            assert(listMetaNode.children.length == 1);
            auto listNode = listMetaNode.children[0];
            assert(listNode.name.startsWith("QMakeProject.FunctionArgument")
                || listNode.name.startsWith("QMakeProject.List"));

            if (listNode.name.startsWith("QMakeProject.FunctionArgument"))
            {
                string[] result = evalFunctionArgument(listNode);
                assert(result.length == 1);
                trace("List variable name: ", result[0]);
                if (!m_contextStack.top().isVariableDefined(result[0]))
                    throw new Exception("Undefined list variable '" ~ result[0] ~ "', aborting");
                iterableList = m_contextStack.top().getVariableRawValue(result[0]);
                // NOTE: list can be empty
                //assert(iterableList.length >= 1);
                trace("List to iterate on: ", iterableList);
            }
            else if (listNode.name.startsWith("QMakeProject.List"))
            {
                // FIXME: implement
                bool b = true; if (b) assert(0);
            }
            else
            {
                throw new Exception("Unknown type of for-iterable list");
            }

            auto blockNode = forSubtypeNode.children[5];
            assert(blockNode.name == "QMakeProject.Block");

            if (!iterableList.empty)
            {
                trace("foreach(", variableName, ", ", iterableList);
                foreach (listValue; iterableList)
                {
                    // Declare local counter variable
                    m_contextStack.top().assignVariable(variableName, [listValue], VariableType.STRING);

                    evalBlock(blockNode);
                }
                trace("foreach() done");

                // Unset the already unneeded variable
                m_contextStack.top().unsetVariable(variableName);
            }
            else
                trace("skipping foreach() - list is empty");
        }
        else if (forSubtypeNode.name == "QMakeProject.ForEverStatement")
        {
            /*
            ForStatement <- ForEachInListStatement / ForEverStatement
            ForEachInListStatement <- "for" OPEN_PAR_WS ForIteratorVariableName COMMA_WS ForIterableList CLOSE_PAR_WS Block
            ForIteratorVariableName <- QualifiedIdentifier
            ForIterableList <- List(:space+, :space) / FunctionArgument(space / COMMA)
            ForEverStatement <- "for" OPEN_PAR_WS "ever" CLOSE_PAR_WS Block
            */
            assert(forSubtypeNode.children.length == 3);
            auto blockNode = forSubtypeNode.children[2];
            assert(blockNode.name == "QMakeProject.Block");
            //trace(blockNode);

            // FIXME: analyze forever loop for exit conditions (return/break/next test functions)

            /*trace("while(true) begin");
            while (true)
            {
                evalBlock(blockNode);
            }
            trace("while(true) end");*/

            bool b = true; if (b) assert(0);
        }
        else
        {
            throw new Exception("Unknown type of 'for' list");
        }

        trace("");
    }

    private void evalBlock(ref ParseTree bodyNode)
    {
        assert(bodyNode.name == "QMakeProject.Block");
        assert(bodyNode.children.length == 1);

        /*immutable*/ auto blockNode = bodyNode.children[0];
        if (blockNode.name == "QMakeProject.SingleLineBlock")
            // FIXME: add checks for children count
            evalStatementNode(blockNode.children[0].children[0]);
        else if (blockNode.name == "QMakeProject.MultiLineBlock")
            evalMultilineBlockNode(blockNode);
        else
            throw new Exception("Unknown code block type");
    }

    private void evalMultilineBlockNode(ref ParseTree multilineBlockNode) /+const+/
    {
        assert(multilineBlockNode.name == "QMakeProject.MultiLineBlock");
        assert(multilineBlockNode.children.length >= 1);
        trace("Multi-line block statement count: ", multilineBlockNode.children.length);
        trace("{");
        for (int i; i < multilineBlockNode.children.length; i++)
        {
            auto blockStatementNode = multilineBlockNode.children[i];
            trace("Eval statement [", i, "] of type ", blockStatementNode.children[0].name);

            evalStatementNode(blockStatementNode.children[0]);
        }
        trace("}");
    }

    private void evalVariableAssignmentNode(ref ParseTree statementNode)
    in
    {
        assert(statementNode.name == "QMakeProject.Assignment");
        assert(statementNode.children.length == 1);
    }
    do
    {
        auto assignmentTypeNode = statementNode.children[0];
        assert(assignmentTypeNode.name == "QMakeProject.StandardAssignment"
            || assignmentTypeNode.name == "QMakeProject.ReplaceAssignment"
        );
        assert(assignmentTypeNode.children.length >= 1);

        auto variableNameNode = assignmentTypeNode.children[0];
        assert(variableNameNode.name == "QMakeProject.QualifiedIdentifier");
        assert(variableNameNode.children.length == 0);
        assert(variableNameNode.matches.length == 1);
        string variableName = variableNameNode.matches[0];

        auto variableOperatorNode = assignmentTypeNode.children[1];
        assert(variableOperatorNode.name == "QMakeProject.StandardAssignmentOperator"
            || variableOperatorNode.name == "QMakeProject.ReplaceAssignmentOperator");
        string variableOperator = variableOperatorNode.matches[0];

        // 1) Detect rvalue compile-time and run-time type (string or list)
        // NOTE: default type is list
        VariableType variableRuntimeType = VariableType.STRING_LIST;
        string[] variableValue;
        if (assignmentTypeNode.children.length >= 3)
        {
            auto rvalueExpr = assignmentTypeNode.children[2];
            assert(rvalueExpr.name == "QMakeProject.RvalueExpression");
            auto rvalueNode = rvalueExpr.children[0];
            trace(rvalueNode.name);
            assert(rvalueNode.name.startsWith("QMakeProject.RvalueList")
                || rvalueNode.name.startsWith("QMakeProject.RvalueChain"));

            if (rvalueNode.name.startsWith("QMakeProject.RvalueList"))
            {
                trace("Parsing rvalue list...");
                RvalueEvalResult[] rvalueCollection;
                for (int i = 0; i < rvalueNode.children.length; i++)    // RvalueList
                {
                    auto rvalueChainNode = rvalueNode.children[i];
                    RvalueEvalResult rvalueResult = evalRvalueChain(rvalueChainNode);
                    rvalueCollection ~= rvalueResult;
                }
                // NOTE: no need in call `deduceRvalueType(rvalueCollection)`:
                //       we already know that we got list
                variableRuntimeType = VariableType.STRING_LIST;
                variableValue = prettifyRvalue(rvalueCollection, variableRuntimeType);
            }
            else if (rvalueNode.name.startsWith("QMakeProject.RvalueChain"))
            {
                trace("Parsing rvalue chain...");
                RvalueEvalResult rvalueResult = evalRvalueChain(rvalueNode);
                variableRuntimeType = rvalueResult.type;
                variableValue = rvalueResult.value;
            }
            else
                throw new NotImplementedException("Unknown rvalue node type " ~ rvalueNode.name);
        }
        else
        {
            warning("empty rvalue detected, please use clear(var) test function call instead");
        }

        trace("Variable name: '" ~ variableName ~ "'");
        trace("Variable assignment operator: '" ~ variableOperator ~ "'");
        if (m_contextStack.top().isVariableDefined(variableName))
            trace("Variable old raw value: ", m_contextStack.top().getVariableRawValue(variableName));
        trace("Variable new raw value: ", variableValue);
        trace("Variable data type: ", variableRuntimeType);

        assignVariable(variableName, variableOperator, variableValue, variableRuntimeType);
    }

    private void assignVariable(in string name, in string operator, in string[] value, in VariableType type)
    {
        switch (operator)
        {
        case STR_EQUALS:
            m_contextStack.top().assignVariable(name, value, type);
            break;
        case STR_PLUS_EQUALS:
            m_contextStack.top().appendAssignVariable(name, value);
            break;
        case STR_ASTERISK_EQUALS:
            m_contextStack.top().appendUniqueAssignVariable(name, value);
            break;
        case STR_MINUS_EQUALS:
            m_contextStack.top().removeAssignVariable(name, value);
            break;
        case STR_TILDE_EQUALS:
            // FIXME: implement
            throw new NotImplementedException("Not implemented yet");
        default:
            throw new Exception("Invalid assignment operator '" ~ operator ~ "'");
        }
    }

    private void evalScopeNode(ref ParseTree statementNode) /+const+/
    {
        ParseTree scopeConditionNode = statementNode.children[0];
        ParseTree scopeIfTrueBranch = statementNode.children[1];

        if (evalScopeConditionNode(scopeConditionNode))
        {
            trace("Scope main condition '", scopeConditionNode.matches.join(" "),
                "' evaluated to TRUE, select main branch code path");
            evalBlock(scopeIfTrueBranch.children[0]);
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
                    if (evalScopeConditionNode(conditionNode))
                    {
                        trace("Scope else-if condition '", conditionNode.matches.join(" "),
                            "' evaluated to TRUE, select else-if branch code path");
                        evalBlock(statementNode.children[i].children[1]);
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
                        evalStatementNode(bodyNode.children[0]);
                    }
                    else if (bodyNode.name == "QMakeProject.MultiLineBlock")
                    {
                        evalMultilineBlockNode(bodyNode);
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

    private bool evalScopeConditionNode(ref ParseTree statementNode) /+const+/
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
                    andResult = evalBooleanExressionNode(boolExprNode);
                    if (notMarker)
                    {
                        trace("Apply NOT 1: '", andResult, "' --> '", !andResult, "'");
                        andResult = !andResult;
                    }
                }
                else
                {
                    bool tempAndResult = evalBooleanExressionNode(boolExprNode);
                    if (notMarker)
                    {
                        trace("Apply NOT 2: '", andResult, "' --> '", !andResult, "'");
                        tempAndResult = !tempAndResult;
                    }
                    andResult = andResult && tempAndResult;
                    if (!tempAndResult)
                    {
                        trace("operand [", j, "] of AND-expression is FALSE, aborting");
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

    private bool evalBooleanExressionNode(ref ParseTree boolExprNode) /+const+/
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
                return evalTestFunctionNode(boolAtomNode);
            case "QMakeProject.QualifiedIdentifier":
                return evalBooleanVariableNode(boolAtomNode);
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

    private RvalueEvalResult evalReplaceFunctionNode(ref ParseTree replaceFunctionNode)
    in
    {
        assert(replaceFunctionNode.name == "QMakeProject.ReplaceFunctionCall");
        assert(replaceFunctionNode.children.length == 2);
    }
    do
    {
        auto testFunctionNode = replaceFunctionNode.children[1];
        assert(testFunctionNode.name == "QMakeProject.TestFunctionCall");
        assert(testFunctionNode.children.length == 1);

        // FunctionCall <- FunctionId OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS :space* :eol*
        auto functionNode = testFunctionNode.children[0];
        assert(functionNode.name == "QMakeProject.EvalTestFunctionCall"
            || functionNode.name == "QMakeProject.CacheTestFunctionCall"
            || functionNode.name == "QMakeProject.ContainsTestFunctionCall"
            || functionNode.name == "QMakeProject.ReturnFunctionCall"
            || functionNode.name == "QMakeProject.RequiresFunctionCall"
            || functionNode.name == "QMakeProject.FunctionCall"
        );

        RvalueEvalResult result = evalConcreteFunctionNode(functionNode, ProFunctionType.Replace);
        return result;
    }

    private bool evalTestFunctionNode(ref ParseTree testFunctionNode) /+const+/
    in
    {
        assert(testFunctionNode.name == "QMakeProject.TestFunctionCall");
        assert(testFunctionNode.children.length == 1);
    }
    do
    {
        // FunctionCall <- FunctionId OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS :space* :eol*
        auto functionNode = testFunctionNode.children[0];
        assert(functionNode.name == "QMakeProject.EvalTestFunctionCall"
            || functionNode.name == "QMakeProject.CacheTestFunctionCall"
            || functionNode.name == "QMakeProject.ContainsTestFunctionCall"
            || functionNode.name == "QMakeProject.ReturnFunctionCall"
            || functionNode.name == "QMakeProject.RequiresFunctionCall"
            || functionNode.name == "QMakeProject.FunctionCall"
        );

        bool result = true;
        RvalueEvalResult resultAsRaw = evalConcreteFunctionNode(functionNode, ProFunctionType.Test);
        // NOTE: test function must return boolean as string `true`/`false`
        assert(resultAsRaw.value.length == 1);
        switch (resultAsRaw.type)
        {
            case VariableType.BOOLEAN:
            {
                switch (resultAsRaw.value[0])
                {
                    case "true":
                        result = true;
                        break;
                    case "false":
                        result = false;
                        break;
                    default:
                        throw new EvalLogicException("boolean constant must be true or false, not "
                            ~ resultAsRaw.value[0]);
                }
                break;
            }
            default: throw new EvalLogicException("test function must return boolean, not "
                ~ to!string(resultAsRaw.type));
        }
        return result;
    }

    private RvalueEvalResult evalConcreteFunctionNode(ref ParseTree functionNode, ProFunctionType functionType)
    in
    {
        assert(functionNode.name == "QMakeProject.EvalTestFunctionCall"
            || functionNode.name == "QMakeProject.CacheTestFunctionCall"
            || functionNode.name == "QMakeProject.ContainsTestFunctionCall"
            || functionNode.name == "QMakeProject.ReturnFunctionCall"
            || functionNode.name == "QMakeProject.RequiresFunctionCall"
            || functionNode.name == "QMakeProject.FunctionCall"
        );
        assert((functionType >= ProFunctionType.Replace)
            && (functionType <= ProFunctionType.Test));
    }
    do
    {
        RvalueEvalResult result;
        switch (functionNode.name)
        {
            case "QMakeProject.EvalTestFunctionCall":
            {
                // EvalTestFunctionCall <- "eval" OPEN_PAR_WS EvalArg CLOSE_PAR_WS
                // EvalArg <- (QualifiedIdentifier :space* "=" :space* Statement) / Statement
                throw new NotImplementedException("EVAL");
                //break;
            }
            case "QMakeProject.CacheTestFunctionCall":
            {
                // CacheTestFunctionCall       <- "cache" OPEN_PAR_WS CacheTestFunctionCallParams? CLOSE_PAR_WS
                // CacheTestFunctionCallParams <- QualifiedIdentifier? (COMMA_WS CacheTestFunctionParam2)? (COMMA_WS FunctionArgument(space / COMMA))?
                // CacheTestFunctionParam2     <- ("set" / "add" / "sub")? :space* ("transient")? :space* ("super" / "stash")?
                
                // FIXME: implement
                result.type = VariableType.BOOLEAN;
                result.value = ["true"];
                break;
            }
            case "QMakeProject.ContainsTestFunctionCall":
            {
                // ContainsTestFunctionCall <- "contains" OPEN_PAR_WS QualifiedIdentifier (COMMA_WS EnquotedString) CLOSE_PAR_WS
                string functionName = "contains";

                assert(functionNode.children[0].name == "QMakeProject.OPEN_PAR_WS");
                assert(functionNode.children[1].name == "QMakeProject.QualifiedIdentifier");
                assert(functionNode.children[2].name == "QMakeProject.COMMA_WS");
                assert(functionNode.children[3].name == "QMakeProject.EnquotedString");
                assert(functionNode.children[4].name == "QMakeProject.CLOSE_PAR_WS");

                assert(functionNode.children[1].matches.length == 1);
                string variableName = functionNode.children[1].matches[0];

                assert(functionNode.children[3].matches.length == 3);
                assert(functionNode.children[3].matches[0] == "\"");
                assert(functionNode.children[3].matches[2] == "\"");
                string expression = functionNode.children[3].matches[1];
                trace("Calling contains(", variableName, ", ", expression, ")");

                // Get function description
                ProFunction functionDescription;
                if (m_contextStack.top().hasTestFunctionDescription(functionName))
                    functionDescription = m_contextStack.top().getTestFunctionDescription(functionName);
                else
                    throw new Exception("Unsupported function '" ~ functionName ~ "'");
                
                // Invoke function
                const(string[]) resultAsList = functionDescription.m_action(m_contextStack.top(), [variableName, expression]);
                if (functionDescription.m_returnType == VariableType.STRING_LIST)
                {
                    trace("Function ", "`", functionName, "`", " returns list: ", resultAsList);
                }
                else
                {
                    trace("Function ", "`", functionName, "`", " returns string: ", "`", resultAsList[0], "`");
                }
                result.type = functionDescription.m_returnType;
                result.value = resultAsList.dup;
                break;
            }
            case "QMakeProject.ReturnFunctionCall":
            {
                // ReturnFunctionCall <- "return" OPEN_PAR_WS (List(:space+, :space) / FunctionArgument(space / COMMA) / Statement)? CLOSE_PAR_WS
                // FIXME: implement
                //break;
                throw new NotImplementedException("RETURN");
            }
            case "QMakeProject.RequiresFunctionCall":
            {
                // RequiresFunctionCall <- "requires" OPEN_PAR_WS BooleanExpression CLOSE_PAR_WS
                // FIXME: implement
                break;
            }
            case "QMakeProject.FunctionCall":
            {
                result = evalFunctionNode(functionNode, functionType);
                break;
            }
            default: throw new NotImplementedException("Unsupported function call statement");
        }
        return result;
    }

    private RvalueEvalResult evalFunctionNode(ref ParseTree functionCallNode, ProFunctionType functionType)
    in
    {
        assert(functionCallNode.name == "QMakeProject.FunctionCall");
        assert(functionCallNode.children.length == 3
            || functionCallNode.children.length == 4);
    }
    do
    {
        RvalueEvalResult result;

        // Get function name            
        auto functionNameNode = functionCallNode.children[0];
        assert(functionNameNode.name == "QMakeProject.FunctionId");
        assert(functionNameNode.children.length == 1);
        string functionName = functionNameNode.matches[0];
        assert(!functionName.empty);
        //assert(isValidFunctionName(functionName));
        trace("Invoking function ", "`", functionName, "`");

        // Get function description
        ProFunction functionDescription;
        switch (functionType)
        {
            case ProFunctionType.Replace:
                if (m_contextStack.top().hasReplaceFunctionDescription(functionName))
                    functionDescription = m_contextStack.top().getReplaceFunctionDescription(functionName);
                else
                    throw new Exception("Unsupported replace function '" ~ functionName ~ "'");
                break;
            case ProFunctionType.Test:
                if (m_contextStack.top().hasTestFunctionDescription(functionName))
                    functionDescription = m_contextStack.top().getTestFunctionDescription(functionName);
                else
                    throw new Exception("Unsupported test function '" ~ functionName ~ "'");
                break;
            default:
                throw new Exception("Unsupported function type'" ~ to!string(functionType) ~ "'");
        }
        
        auto openParNode = functionCallNode.children[1];
        assert(openParNode.name == "QMakeProject.OPEN_PAR_WS");
        assert(openParNode.children.length == 0);

        // Get function actual argument count
        // FunctionArgumentList <- List(COMMA_WS, COMMA) / List(:space+, :space) / FunctionArgument(space / COMMA)
        int actualOperandCount = 0;
        auto functionArgumentListNode = functionCallNode.children[2];
        if (functionArgumentListNode.name.startsWith("QMakeProject.FunctionArgumentList"))
        {
            assert(functionArgumentListNode.name == "QMakeProject.FunctionArgumentList");
            assert(functionArgumentListNode.children.length == 1);
            if (functionArgumentListNode.children[0].name.startsWith("QMakeProject.List!")) // comma or whitespace-separated list
            {
                auto listNode = functionArgumentListNode.children[0];
                for (int i = 0; i < listNode.children.length; i++)
                {
                    if (listNode.children[i].name.startsWith("QMakeProject.FunctionArgument"))
                        actualOperandCount ++;
                }
            }
            else if (functionArgumentListNode.children[0].name.startsWith("QMakeProject.FunctionArgument!"))    // single argument
            {
                actualOperandCount = 1;
            }
            else
                throw new EvalLogicException("Unknown function argument type");
        }
        else if (functionArgumentListNode.name == "QMakeProject.CLOSE_PAR_WS")
        {
        }
        else
            throw new EvalLogicException("function argument list or closing parenthehis expected");

        trace("Actual function ", "`", functionName, "`", " argument count: ", actualOperandCount);

        validateFunctionCall(actualOperandCount, functionDescription);
        
        // Get function argument values
        string[] actualArguments;
        if (actualOperandCount > 0)
        {
            if (functionArgumentListNode.children[0].name.startsWith("QMakeProject.List!")) // comma or whitespace-separated list
            {
                auto listNode = functionArgumentListNode.children[0];
                actualArguments = evalFunctionArgumentList(listNode);
            }
            else if (functionArgumentListNode.children[0].name.startsWith("QMakeProject.FunctionArgument!"))    // single argument
            {
                actualArguments = evalFunctionArgument(functionArgumentListNode.children[0]);
            }
            else throw new EvalLogicException("Unknown function argument type");
        }
        trace("Actual function ", "`", functionName, "`", " arguments: ", actualArguments);

        // Invoke function
        const(string[]) resultAsList = functionDescription.m_action(m_contextStack.top(), actualArguments);
        if (functionDescription.m_returnType == VariableType.STRING_LIST)
        {
            trace("Function ", "`", functionName, "`", " returns list: ", resultAsList);
        }
        else
        {
            trace("Function ", "`", functionName, "`", " returns string: ", "`", resultAsList[0], "`");
        }
        result.type = functionDescription.m_returnType;
        result.value = resultAsList.dup;

        return result;
    }

    private string[] evalFunctionArgumentList(ref ParseTree listNode)
    in
    {
        assert(listNode.name.startsWith("QMakeProject.List!")); // comma or whitespace-separated list
    }
    do
    {
        string[] result;
        for (int i = 0; i < listNode.children.length; i++)
        {
            if (listNode.children[i].name.startsWith("QMakeProject.FunctionArgument!"))
            {
                string[] temp = evalFunctionArgument(listNode.children[i]);
                if (!temp.empty)
                    result ~= temp;
            }
        }
        return result;
    }

    // NOTE: in compile-time function argument is a single token, but in runtime in can expand to list;
    //       message($$QMAKE_PLATFORM) ---> message(linux unix posix)
    //       must not be confused with chain statements:
    //       message($${var}/dir/file.txt) ---> message(path/dir/file.txt)
    //       i.e. one token stay one
    private string[] evalFunctionArgument(ref ParseTree argumentNode)
    {
        string[] result;

        assert(argumentNode.name.startsWith("QMakeProject.FunctionArgument!"));
        assert(argumentNode.children.length >= 1);
        bool hasLeftovers = false;
        for (int i = 0; i < argumentNode.children.length; i++)
        {
            auto argumentImplNode = argumentNode.children[i];
            assert(argumentImplNode.name.startsWith("QMakeProject.FunctionArgumentImpl!"));
            assert(argumentImplNode.children.length == 1);

            auto argumentValueNode = argumentImplNode.children[0];
            if (argumentValueNode.name.startsWith("QMakeProject.RvalueAtom"))
            {
                auto r = evalRvalueAtomNode(argumentValueNode);
                if (r.type == VariableType.STRING)
                    trace("type = ", r.type, ", value = ", "`", (r.value.empty ? "<empty>" : r.value[0]), "`");
                else
                    trace("type = ", r.type, ", value = ", "`", r.value, "`");
                result ~= r.value;
            }
            else if (argumentValueNode.name.startsWith("QMakeProject.EnquotedFunctionArgument"))
            {
                assert(argumentValueNode.children.length == 1);
                auto enquotedNote = argumentValueNode.children[0];
                assert(enquotedNote.name.startsWith("QMakeProject.DoubleEnquotedFunctionArgument!")
                    || enquotedNote.name.startsWith("QMakeProject.SingleEnquotedFunctionArgument!"));
                if (enquotedNote.children.empty)
                {
                    result ~= "";
                    continue;
                }

                assert(enquotedNote.children.length == 1);

                auto chainNode = enquotedNote.children[0];
                assert(chainNode.name.startsWith("QMakeProject.EnquotedFunctionArgumentChain!"));
                assert(chainNode.children.length >= 1);

                string tempResult;
                for (int j = 0; j < chainNode.children.length; j++)
                {
                    auto implNode = chainNode.children[j];
                    assert(implNode.name.startsWith("QMakeProject.FunctionArgumentImpl_2"));
                    assert(implNode.children.length == 1);

                    auto stringNode = implNode.children[0];
                    if (stringNode.name.startsWith("QMakeProject.RvalueAtom"))
                    {
                        RvalueEvalResult r = evalRvalueAtomNode(stringNode);
                        trace("Rvalue atom: ", "`", r.value, "`");

                        // FIXME: use type information somehow?
                        tempResult ~= r.value.join("");
                    }
                    else if (stringNode.name.startsWith("QMakeProject.FunctionArgumentString!"))
                    {
                        assert(stringNode.matches.length == 1);
                        trace("Enquoted argument: ", "`", stringNode.matches[0], "`");

                        tempResult ~= stringNode.matches[0];
                        hasLeftovers = true;
                    }
                    else if (stringNode.name.startsWith("QMakeProject.Leftover!"))
                    {
                        assert(stringNode.matches.length == 1);
                        trace("Leftover: ", "`", stringNode.matches[0], "`");

                        tempResult ~= stringNode.matches[0];
                        hasLeftovers = true;
                    }
                    else
                        throw new NotImplementedException("Invalid function argument implementation node " ~ stringNode.name);
                }
                result ~= tempResult;
            }
            else if (argumentValueNode.name.startsWith("QMakeProject.FunctionArgumentString!"))
            {
                assert(argumentValueNode.matches.length == 1);
                trace("Leftover argument: ", "`", argumentValueNode.matches[0], "`");
                result ~= argumentValueNode.matches[0];
                hasLeftovers = true;
            }
            else throw new NotImplementedException("Invalid function argument type");
        }

        if (hasLeftovers)
        {
            trace("Function argument contains leftovers, so implicitly convert result from list to single string");
            string tempResult = result.join("");
            result = [];
            assert(result.length == 0);
            result ~= tempResult;
            assert(result.length == 1);
        }
        return result;
    }

    private bool evalBooleanVariableNode(ref ParseTree boolAtomNode)
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

        string[] platformValue = m_contextStack.top().getVariableRawValue("QMAKE_PLATFORM");
        string[] configValue = m_contextStack.top().getVariableRawValue("CONFIG");
        string[] specValue = m_contextStack.top().getVariableRawValue("QMAKESPEC");
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

    private RvalueEvalResult evalRvalueChain(ref ParseTree rvalueChainNode)
    in
    {
        assert(rvalueChainNode.name.startsWith("QMakeProject.RvalueChain"));
    }
    do
    {
        trace("Parsing rvalue chain...");

        RvalueEvalResult[] resultCollection;
        for (int i = 0; i < rvalueChainNode.children.length; i++)
        {
            trace("Chain node: ", rvalueChainNode.children[i].name);
            auto rvalueChainNode_2 = rvalueChainNode.children[i];
            assert(rvalueChainNode_2.name.startsWith("QMakeProject.Rvalue!"));
            assert(rvalueChainNode_2.children.length == 1);

            auto rvalueAtomMetaNode = rvalueChainNode_2.children[0];
            if (rvalueAtomMetaNode.name.startsWith("QMakeProject.RvalueAtom"))
            {
                RvalueEvalResult result = evalRvalueAtomNode(rvalueAtomMetaNode);
                trace("Atom expanded value: ", result);
                resultCollection ~= result;
            }
            else if (rvalueAtomMetaNode.name.startsWith("QMakeProject.EnquotedRvalue"))
            {
                assert(rvalueAtomMetaNode.children.length == 1);
                auto enquotedNote = rvalueAtomMetaNode.children[0];
                assert(enquotedNote.name.startsWith("QMakeProject.DoubleEnquotedRvalue")
                    || enquotedNote.name.startsWith("QMakeProject.SingleEnquotedRvalue"));
                assert(enquotedNote.children.length == 1);
                auto chainNode = enquotedNote.children[0];
                RvalueEvalResult result = evalRvalueChain(chainNode);
                trace("Enquoted value = ", result);
                resultCollection ~= result;
            }
            else if (rvalueAtomMetaNode.name.startsWith("QMakeProject.Leftover"))
            {
                assert(rvalueAtomMetaNode.children.length == 0);
                assert(rvalueAtomMetaNode.matches.length == 1);
                string leftover = rvalueAtomMetaNode.matches[0];
                trace("Leftover: ", "`", leftover, "`");
                resultCollection ~= RvalueEvalResult(VariableType.STRING, [leftover]);
            }
            else throw new NotImplementedException("");
        }

        VariableType finalType = deduceRvalueType(resultCollection);
        string[] variableValue = prettifyRvalue(resultCollection, finalType);

        return RvalueEvalResult(finalType, variableValue);
    }

    private RvalueEvalResult evalRvalueAtomNode(ref ParseTree rvalueAtomMetaNode)
    {
        assert(rvalueAtomMetaNode.name.startsWith("QMakeProject.RvalueAtom"));

        RvalueEvalResult result;

        // RvalueAtom <- ReplaceFunctionCall / ExpandStatement / TestFunctionCall
        auto rvalueAtomNode = rvalueAtomMetaNode.children[0];
        assert(rvalueAtomNode.children.length >= 1);

        if (rvalueAtomNode.name == "QMakeProject.ReplaceFunctionCall")
        {
            result = evalReplaceFunctionNode(rvalueAtomNode);
        }
        else if (rvalueAtomNode.name == "QMakeProject.ExpandStatement")
        {
            assert(rvalueAtomNode.children.length == 1);

            auto expandNode = rvalueAtomNode.children[0];
            assert(expandNode.children.length == 1);
            switch (expandNode.name)
            {
                case "QMakeProject.FunctionArgumentExpandStatement":
                {
                    string variableName = expandNode.children[0].matches[0];
                    trace("Function argument name: ", variableName);

                    // Unlike project variable, function argument must be defined earlier
                    if (!m_contextStack.top().isVariableDefined(variableName))
                        throw new EvalLogicException("Undefined project variable found");
                    
                    assert(m_contextStack.top().getVariableType(variableName) == VariableType.STRING);

                    string[] variableValue = m_contextStack.top().getVariableRawValue(variableName);
                    assert(variableValue.length == 1);
                    trace("Function argument value: ", "`", variableValue[0], "`");

                    result.type = VariableType.STRING;
                    result.value = variableValue;
                    break;
                }
                case "QMakeProject.ProjectVariableExpandStatement":
                {
                    string variableName = expandNode.children[0].matches[0];
                    trace("Project variable name: ", variableName);
                    if (!m_contextStack.top().isVariableDefined(variableName))
                    {
                        // FIXME: looks like undefined variable is not an error in qmake;
                        //        but is it always or just in some cases?
                        warning("undefined variable ", "`", variableName, "`");
                        result.type = VariableType.STRING_LIST;
                        result.value = [""];
                        //throw new EvalLogicException("Undefined project variable found");
                    }
                    else
                    {
                        VariableType variableType = m_contextStack.top().getVariableType(variableName);
                        assert(isStringType(variableType) || isStringListType(variableType));
                        result.type = variableType;
                        trace("Variable type: ", variableType);

                        string[] variableValue = m_contextStack.top().getVariableRawValue(variableName);
                        if (isStringType(variableType))
                        {
                            assert(variableValue.empty || variableValue.length == 1);
                            trace("Variable value: ", variableValue.empty ? "<empty>" : variableValue[0]);
                        }
                        else if (isStringListType(variableType))
                        {
                            assert(variableValue.empty || variableValue.length >= 1);
                            trace("Variable value: ", variableValue.empty ? ["<empty>"] : variableValue);
                        }
                        result.value = variableValue;
                    }

                    break;
                }
                case "QMakeProject.MakefileVariableExpandStatement":
                {
                    // FIXME: implement
                    //bool b = true; if (b) assert(0);
                    string variableName = expandNode.children[0].matches[0];
                    trace("Makefile variable name: ", variableName);
                    //if (!m_context.isVariableDefined(variableName))
                    //    throw new EvalLogicException("Undefined project variable found");
                    result.type = VariableType.STRING;
                    result.value = [variableName];
                    break;
                }
                case "QMakeProject.EnvironmentVariableExpandStatement":
                {
                    // FIXME: implement
                    string environmentVariableName = expandNode.children[0].matches[0];
                    trace("Environment variable name: ", environmentVariableName);
                    auto environmentVariableValue = environment.get(environmentVariableName);
                    bool isEmpty = false;
                    if (environmentVariableValue is null)
                    {
                        warning("Expand undefined environment variable '", environmentVariableName, "' to empty string");

                        environmentVariableValue = "";
                        isEmpty = true;
                    }
                    trace("Environment variable value: ", "`", environmentVariableValue, "`");
                    
                    result.type = VariableType.STRING;
                    result.value = isEmpty ? [] : [environmentVariableValue];
                    break;
                }
                case "QMakeProject.PropertyVariableExpandStatement":
                {
                    string propertyName = expandNode.children[0].matches[0];
                    trace("Persistent property name: ", propertyName);
                    if (!m_persistentStorage.hasValue(propertyName))
                        throw new EvalLogicException("Undefined persistent property found");

                    // FIXME: investigate whether properties can be lists
                    result.type = VariableType.STRING;
                    trace("Persistent property type: ", result.type);

                    string propertyValue = m_persistentStorage.value(propertyName);
                    trace("Persistent property value: ", "`", propertyValue, "`");
                    result.value = [propertyValue];
                    break;
                }
                default: throw new NotImplementedException("Unknown expand statement type: " ~ expandNode.name);
            }
        }
        else if (rvalueAtomNode.name == "QMakeProject.TestFunctionCall")
        {
            // FIXME: implement
            bool b = true; if (b) assert(0);
        }
        else throw new NotImplementedException("");

        return result;
    }

    private void validateFunctionCall(in int actualOperandCount, ref ProFunction functionDescription)
    {
        // Report function expected and actual argument count (if applicable)
        // FIXME: implement user-defined functions support and add condition
        // `&& !functionDescription.m_isUserDefined`
        if (!functionDescription.m_isVariadic)
        {
            string temp;
            if (functionDescription.m_requiredArgumentCount > 0)
                temp ~= to!string(functionDescription.m_requiredArgumentCount) ~ " " ~ "required";
            if (functionDescription.m_optionalArgumentCount)
                temp ~= ", " ~ to!string(functionDescription.m_optionalArgumentCount) ~ " " ~ "optional";
            trace("Expected replace function count: ", temp);
        }
        else
        {
            trace("Replace function is variadic one, no required/optional argument count was specified");
        }

        // Validate arguments count for non-variadic functions
        if (!functionDescription.m_isVariadic)
        {
            if (functionDescription.m_requiredArgumentCount < 0)
            {
                throw new Exception(
                        "Invalid function '" ~ functionDescription.m_name ~ "' argument count description");
            }

            if (functionDescription.m_optionalArgumentCount < 0)
            {
                if (actualOperandCount < functionDescription.m_requiredArgumentCount)
                {
                    throw new  /*RangeError*/ Exception(
                            "Invalid number of arguments for function '" ~ functionDescription.m_name ~ "': " ~ to!string(
                            functionDescription.m_requiredArgumentCount) ~ " expected, but " ~ to!string(
                            actualOperandCount) ~ " given");
                }
            }
            else
            {
                int minArgCount = functionDescription.m_requiredArgumentCount;
                int maxArgCount = minArgCount + functionDescription.m_optionalArgumentCount;
                if ((actualOperandCount < minArgCount) || (actualOperandCount > maxArgCount))
                {
                    throw new  /*RangeError*/ Exception(
                            "Invalid number of arguments for function '" ~ functionDescription.m_name ~ "': from " ~ to!string(
                            minArgCount) ~ " to " ~ to!string(
                            maxArgCount) ~ " expected, but " ~ to!string(actualOperandCount) ~ " given");
                }
            }
        }
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
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qt3d/tests/auto/render/render.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qttools/mkspecs/features/qt_find_clang.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtwebengine/mkspecs/features/configure.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtwebengine/mkspecs/features/platform.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtmultimedia/examples/multimedia/multimedia.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtremoteobjects/tests/auto/integration_multiprocess/server/server.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtbase/tests/auto/network/ssl/ssl.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtbase/src/plugins/sqldrivers/configure.pri"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtbase/src/widgets/util/util.pri"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtbase/mkspecs/features/spec_post.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtbase/mkspecs/features/winrt/package_manifest.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtbase/mkspecs/features/qt_configure.prf"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtdeclarative/tests/auto/quick/pointerhandlers/pointerhandlers.pro"));
    assert(pro.tryParse("/opt/Qt/5.11.3/Src/qtdeclarative/tests/auto/qml/qml.pro"));

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
    assert(parseQtSourceProjects("/opt/Qt/5.11.3/Src") == 0);
}
