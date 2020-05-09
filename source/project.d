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

module source.project;

import std.experimental.logger;

import std.typecons;
import std.conv;
import std.file;
import std.path;
import std.string;
import std.algorithm;
import std.process;

import qmakeparser;

import source.preprocessor;
import source.qmakeexception;
import source.common_const;
import source.utils.text_utils;
import source.utils.io_utils;
import source.project_variable;
import source.project_function;
import source.project_context;
import source.persistent_property;
import source.type_deduction;

public:

class Project
{
private:
    // NOTE: variables defined in user-defined qmake functions must be local to them
    alias ContextStack = QStack!ProExecutionContext;
    ContextStack m_contextStack;
    PersistentPropertyStorage m_persistentStorage;

public:
    this(ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage)
    {
        m_contextStack = new ContextStack;
        m_contextStack.push(context);

        m_persistentStorage = persistentStorage;
    }

    void dump() /*const*/
    {
        trace("\n\nqmake built-in variable values:");
        foreach (variableName; m_contextStack.top().getBuiltinVariableNames())
        {
            if (!m_contextStack.top().isVariableValueEmpty(variableName))
            {
                string[] variableValue = m_contextStack.top().getVariableRawValue(variableName);
                trace(variableName, " = ", variableValue);
            }
        }
        
        trace("\n\nqmake user-defined variable values:");
        foreach (variableName; m_contextStack.top().getUserDefinedVariableNames())
        {
            if (!m_contextStack.top().isVariableValueEmpty(variableName))
            {
                string[] variableValue = m_contextStack.top().getVariableRawValue(variableName);
                trace(variableName, " = ", variableValue);
            }
        }
        trace("\n\n");
    }

    bool tryParseSnippet(const string snippet)
    {
        LineInfo[] li;
        string preprocessedSnippet = preprocessLines(splitString(snippet, "\n", false), li);

        auto parseTree = QMakeProject(preprocessedSnippet);
        if (!parseTree.successful)
        {
            error("\n===============================================================================================");
            error(parseTree);
            error("\n===============================================================================================");
            error("Parsing snippet FAILED!");
            error("\n===============================================================================================");
        }
        else
        {
            info("\n===============================================================================================");
            info("Snippet was successfully parsed");
            info("\n===============================================================================================");
        }

        return parseTree.successful;
    }

    bool tryParse(const string fileName) const
    {
        trace("Trying to parse project file '" ~ fileName ~ "'...");

        auto proFileContents = std.file.readText(fileName);
        LineInfo[] result;
        proFileContents = preprocessLines(splitString(proFileContents, "\n", false), result);

        auto parseTree = QMakeProject(proFileContents);
        if (!parseTree.successful)
        {
            error("\n===============================================================================================");
            error(parseTree);
            error("\n===============================================================================================");
            error("Parsing project file '" ~ fileName ~ "' FAILED!");
            error("\n===============================================================================================");
        }
        else
        {
            info("\n===============================================================================================");
            info("Project file '" ~ fileName ~ "' was successfully parsed");
            info("\n===============================================================================================");
        }

        return parseTree.successful;
    }

    bool eval(const string fileName) /*const*/
    {
        trace("Trying to parse project file '" ~ fileName ~ "'...");

        // Save parent project path variables
        string currentProjectFileName;
        if (!m_contextStack.top().isVariableValueEmpty("_PRO_FILE_"))
        {
            currentProjectFileName = m_contextStack.top().getVariableRawValue("_PRO_FILE_")[0];
            assert(isValidFilePath(currentProjectFileName));
        }
        // NOTE: use the previously eval'd state, because we can need already defined variables
        m_contextStack.top().setupPaths(fileName);

        auto proFileContents = std.file.readText(fileName);
        LineInfo[] result;
        proFileContents = preprocessLines(splitString(proFileContents, "\n", false), result);

        auto parseTree = QMakeProject(proFileContents);
        if (!parseTree.successful)
        {
            error("\n===============================================================================================");
            error(parseTree);
            error("\n===============================================================================================");
            error("Parsing project file '" ~ fileName ~ "' FAILED!");
            error("\n===============================================================================================");

            // Restore parent project path variables
            if (!currentProjectFileName.empty)
                m_contextStack.top().setupPaths(currentProjectFileName);

            return false;
        }

        info("\n===============================================================================================");
        info("Project file '" ~ fileName ~ "' was successfully parsed");
        info("\n===============================================================================================");

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

        info("\n===============================================================================================");
        info("Project file '" ~ fileName ~ "' was successfully evaluated");
        info("\n===============================================================================================");

        // Restore parent project path variables
        if (!currentProjectFileName.empty)
            m_contextStack.top().setupPaths(currentProjectFileName);

        return true;
    }

    // ----------------------------------------------------------------------------------------------------------------------------------------------
private:

    void evalStatementNode(ref ParseTree statementNode) /+const+/
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
                     | :EmptyStatement
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
            default:
            {
                trace(statementNode);
                error("Invalid statement type '" ~ statementNode.name ~ "'");
                throw new EvalLogicalException("Invalid statement type '" ~ statementNode.name ~ "'");
            }
        }
    }

    void evalFunctionDeclarationNode(ref ParseTree declNode)
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
                // alias Action = const(string[]) function(ref ProExecutionContext context, const string[] arguments);
                auto nameNode = concreteDeclNode.children[1];
                assert(nameNode.children.length == 0);
                assert(nameNode.matches.length == 1);

                string name = nameNode.matches[0];
                trace("Declare user-defined replace function ", "`", name, "`");

                auto functionBlock = concreteDeclNode.children[3];
                const(string[]) replaceAction(ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments)
                {
                    trace("Invoking user-defined replace function ", "`", name, "`");

                    m_contextStack.push(context);

                    // Declare function arguments like $${1} etc.
                    for (int i = 0; i < arguments.length; i++)
                    {
                        immutable string argName = to!string(i + 1);
                        trace("Declare function argument variable `$${", argName, "}` = `", arguments[i], "`");
                        m_contextStack.top().assignVariable(argName, [arguments[i]], VariableType.STRING);
                    }

                    assert(functionBlock.name == "QMakeProject.Block");
                    assert(functionBlock.children.length == 1);

                    auto blockNode = functionBlock.children[0];
                    evalBlock(functionBlock);
                    
                    //
                    // FIXME: merge all global already defined variables from internal function context
                    //
                    const(string[]) result = m_contextStack.top().popFunctionResult();
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
                const(string[]) testAction(ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage, const string[] arguments)
                {
                    trace("Invoking user-defined test function ", "`", name, "`");

                    const(string[]) result = ["true"];

                    m_contextStack.push(context);

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
                    evalBlock(functionBlock);

                    
                    //
                    // FIXME: merge all global variables from internal function context
                    //
                    m_contextStack.pop();

                    trace("User-defined test function result: ", result);
                    bool b = true; if (b) assert(0);
                    return result;
                }
                m_contextStack.top().addTestFunctionDescription(name.dup, &testAction);
                break;
            }
            default: throw new NotImplementedException("");
        }
    }

    void evalForStatementNode(ref ParseTree forNode)
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
                    throw new EvalLogicalException("Undefined list variable '" ~ result[0] ~ "', aborting");
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
                throw new EvalLogicalException("Unknown type of for-iterable list");
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

            // FIXME: analyze forever loop for exit conditions existance (return/break/next test functions);
            //        if there is no such statements detected, show warning or error

            trace("\n\n", "while(true) begin");
            while (!m_contextStack.top().hasFlowControlStatement())
            {
                evalBlock(blockNode);
            }
            trace("loop finish reason: ", m_contextStack.top().getFlowControlStatement());
            trace("while(true) end", "\n\n");
        }
        else
        {
            throw new EvalLogicalException("Unknown type of 'for' list");
        }

        trace("");
    }

    void evalBlock(ref ParseTree bodyNode)
    {
        assert(bodyNode.name == "QMakeProject.Block");
        assert(bodyNode.children.length == 1);

        /*immutable*/ auto blockNode = bodyNode.children[0];
        if ((blockNode.name == "QMakeProject.SingleLineBlock") || (blockNode.name == "QMakeProject.Statement"))
            // FIXME: add checks for children count
            evalStatementNode(blockNode.children[0].children[0]);
        else if (blockNode.name == "QMakeProject.MultiLineBlock")
            evalMultilineBlockNode(blockNode);
        else
            throw new EvalLogicalException("Unknown code block type");
    }

    void evalMultilineBlockNode(ref ParseTree multilineBlockNode) /+const+/
    {
        assert(multilineBlockNode.name == "QMakeProject.MultiLineBlock");
        assert(multilineBlockNode.children.length >= 1);
        trace("Multi-line block statement count: ", multilineBlockNode.children.length);
        trace("{");
        for (int i = 0; i < multilineBlockNode.children.length; i++)
        {
            auto blockStatementNode = multilineBlockNode.children[i].children[0];

            if (m_contextStack.top().hasFlowControlStatement())
            {
                trace("flow control statement ", m_contextStack.top().getFlowControlStatement(), " found: stop eval block");
                break;
            }

            trace("Eval statement [", i, "] of type ", blockStatementNode.name);
            trace("{");
            evalStatementNode(blockStatementNode);
            trace("}");
        }
        trace("}");
    }

    void evalVariableAssignmentNode(ref ParseTree statementNode)
    in
    {
        assert(statementNode.name == "QMakeProject.Assignment");
        assert(statementNode.children.length == 1);
    }
    do
    {
        RvalueEvalResult result;

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

        if (assignmentTypeNode.name == "QMakeProject.StandardAssignment")
        {
            result = evalVariableStandardAssignmentNode(assignmentTypeNode);
        }
        else if (assignmentTypeNode.name == "QMakeProject.ReplaceAssignment")
        {
            result = evalVariableReplaceAssignmentNode(variableName, assignmentTypeNode);
        }
        else
        {
            throw new EvalFunctionException("Unknown assignment type " ~ assignmentTypeNode.name);
        }

        trace("Variable name: '" ~ variableName ~ "'");
        trace("Variable assignment operator: '" ~ variableOperator ~ "'");
        if (m_contextStack.top().isVariableDefined(variableName))
            trace("Variable old raw value: ", m_contextStack.top().getVariableRawValue(variableName));
        trace("Variable new raw value: ", result.value);
        trace("Variable data type: ", result.type);

        assignVariable(variableName, variableOperator, result.value, result.type);
    }

    RvalueEvalResult evalVariableStandardAssignmentNode(ref ParseTree assignmentTypeNode)
    {
        RvalueEvalResult result;
        result.type = VariableType.STRING_LIST;

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
                result.type = VariableType.STRING_LIST;
                result.value = prettifyRvalue(rvalueCollection, result.type);
            }
            else if (rvalueNode.name.startsWith("QMakeProject.RvalueChain"))
            {
                trace("Parsing rvalue chain...");
                RvalueEvalResult rvalueResult = evalRvalueChain(rvalueNode);
                result.type = rvalueResult.type;
                result.value = rvalueResult.value;
            }
            else
                throw new NotImplementedException("Unknown rvalue node type " ~ rvalueNode.name);
        }
        else
        {
            warning("empty rvalue detected, please use clear(var) test function call instead");
        }

        return result;
    }

    RvalueEvalResult evalVariableReplaceAssignmentNode(const string variableName, ref ParseTree assignmentTypeNode)
    {
        RvalueEvalResult result;
        result.type = VariableType.STRING;

        // DEFINES ~= s/QT_[DT].+/QT
        // any values in the list that start with QT_D or QT_T are replaced with QT.

        // e.g.
        // ICONS_FOUND ~= s/.*\\\$\\\$\\{WINRT_MANIFEST\\.((logo|tile)_[^\}]+)\\}.*/\\1/g
        // parameter ~= s/^-L//
        // line ~= s/^[ \\t]*//  # remove leading spaces
        // line ~= s/^LIBRARY_PATH=//  # remove leading LIBRARY_PATH=
        // line ~= s,^libraries: ,,
        // v ~= s/ .*//
        //  # -MD becomes -MT, -MDd becomes -MTd
        // QMAKE_CFLAGS ~= s,^-MD(d?)$,-MT\1,g
        // QMAKE_CXXFLAGS ~= s,^-MD(d?)$,-MT\1,g
        //  modules ~= s,-private$,_private,g
        // qt_plugin_deps ~= s,-private$,_private,g
        // # Insert the major version of Qt in the library name
        // # unless it's a framework build.
        // LIBRARY_NAME ~= s,^Qt,Qt$$QT_MAJOR_VERSION,
        // # Re-insert the major version in the library name (cf qt5LibraryTarget above)
        // MODULE_NAME ~= s,^Qt,Qt$$QT_MAJOR_VERSION,
        // $$dir ~= s/$${exclusive_affix}/$${build_affix}/gi
        // QMAKE_LINK ~= s/(\\S*g\\+\\+|\\S*gcc)/cs\\1/
        // QMAKE_YACC_HEADER      ~= s/\\$base/${QMAKE_FILE_BASE}/g
        // QMAKE_LINK_SHLIB_CMD ~= s/^$$re_escape($$QMAKE_LINK_SHLIB)$/$$QMAKE_LINK_C_SHLIB/
        // rcc.commands ~= s/&&/$$escape_expand(\\n\\t)/g
        // module_module ~= s,^Qt,Qt$$QT_MAJOR_VERSION,

        // line ~= s/^[ \\t]*//
        //        "s/^[ \\\\t]*//"

        auto regexNode = assignmentTypeNode.children[2];
        assert(regexNode.name == "QMakeProject.RegularExpression");
        
        assert(regexNode.matches.length == 1);
        string regexStr = regexNode.matches[0];
        assert(regexStr.length >= 4);
        assert(regexStr.startsWith("s"));
        trace("Regular expression (raw): ", regexStr);

        string splitterChar = "" ~ regexStr[1];
        trace("Regular expression splitter: ", "`", splitterChar, "`");

        string[] regexParts = splitString(regexStr, splitterChar, false);
        trace("Regular expession (splitted): ", regexParts);
        assert(regexParts.length == 4);
        assert(regexParts[0] == "s");

        if (m_contextStack.top().isVariableDefined(variableName))
        {
            string[] variableValue = m_contextStack.top().getVariableRawValue(variableName);
            assert(variableValue.length == 1);
            string variableValueAsString = variableValue[0];

            trace("Source: ", "`", variableValueAsString, "`");
            trace("Regular expression: ", "`", regexParts[1], "`");
            trace("Regular expression modifier: ", "`", regexParts[3], "`");
            trace("Replace string: ", "`", regexParts[2], "`");

            import std.regex : regex, replaceAll;
            result.value = [replaceAll(variableValueAsString, regex(regexParts[1], regexParts[3]), regexParts[2])];

            trace("Result: ", "`", result.value, "`");
        }
        else 
        {
            warning("Variable ", "`", variableName, "`", " value is empty: nothing to replace");
        }

        return result;
    }

    void assignVariable(const string name, const string operator, const string[] value, const VariableType type)
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
            // FIXME: test
            m_contextStack.top().assignVariable(name, value, type);
            break;
        default:
            throw new EvalLogicalException("Invalid assignment operator '" ~ operator ~ "'");
        }
    }

    void evalScopeNode(ref ParseTree statementNode) /+const+/
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

                    auto blockNode = statementNode.children[i].children[0];
                    evalBlock(blockNode);
                    break;
                default:
                    throw new EvalLogicalException("Invalid scope node type");
            }
        }
    }

    bool evalScopeConditionNode(ref ParseTree statementNode) /+const+/
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

    bool evalBooleanExressionNode(ref ParseTree boolExprNode) /+const+/
    {
        assert(boolExprNode.children.length == 1);
        switch (boolExprNode.name)
        {
        case "QMakeProject.ParenthesedBooleanExpression":
            // FIXME: implement
            throw new EvalLogicalException("Not implemented");
        case "QMakeProject.IfTestFunctionCall":
            // FIXME: implement
            throw new EvalLogicalException("Not implemented");
        case "QMakeProject.BooleanAtom":
            // BooleanAtom <- ReplaceFunctionCall / TestFunctionCall / QualifiedIdentifier / BooleanConst
            auto boolAtomNode = boolExprNode.children[0];
            switch (boolAtomNode.name)
            {
            case "QMakeProject.ReplaceFunctionCall":
                // FIXME: implement
                throw new EvalLogicalException("Not implemented yet");
            case "QMakeProject.TestFunctionCall":
                return evalTestFunctionNode(boolAtomNode);
            case "QMakeProject.QualifiedIdentifier":
                return evalBooleanVariableNode(boolAtomNode);
            case "QMakeProject.BooleanConst":
                // FIXME: implement
                throw new EvalLogicalException("Not implemented yet");
            default:
                throw new EvalLogicalException("Unknown boolean atom type");
            }
            //break;
        default:
            throw new EvalLogicalException("Unknown boolean meta-expression type");
        }

       // return false;
    }

    RvalueEvalResult evalReplaceFunctionNode(ref ParseTree replaceFunctionNode)
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

    bool evalTestFunctionNode(ref ParseTree testFunctionNode) /+const+/
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
                        throw new EvalLogicalException("boolean constant must be true or false, not "
                            ~ resultAsRaw.value[0]);
                }
                break;
            }
            default: throw new EvalLogicalException("test function must return boolean, not "
                ~ to!string(resultAsRaw.type));
        }
        return result;
    }

    RvalueEvalResult evalConcreteFunctionNode(ref ParseTree functionNode, ProFunctionType functionType)
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
                trace("FIXME: eval cache() test function stub...");
                result.type = VariableType.BOOLEAN;
                result.value = ["true"];
                break;
            }
            case "QMakeProject.ContainsTestFunctionCall":
            {
                trace("eval contains() test function call...");
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
                    throw new EvalLogicalException("Unsupported function '" ~ functionName ~ "'");
                
                // Invoke function
                const(string[]) resultAsList = functionDescription.action(m_contextStack.top(), m_persistentStorage, [variableName, expression]);
                if (functionDescription.fti.returnType == VariableType.STRING_LIST)
                {
                    trace("Function ", "`", functionName, "`", " returns list: ", resultAsList);
                }
                else
                {
                    trace("Function ", "`", functionName, "`", " returns string: ", "`", resultAsList[0], "`");
                }
                result.type = functionDescription.fti.returnType;
                result.value = resultAsList.dup;
                break;
            }
            case "QMakeProject.ReturnFunctionCall":
            {
                assert(functionNode.children.length == 2 || functionNode.children.length == 3);
                if (functionNode.children.length == 3)
                {
                    auto expressionNode = functionNode.children[1];
                    assert(expressionNode.name == "QMakeProject.ReturnFunctionArguments");
                    string[] actualArguments = evalFunctionActualArguments(/*actualOperandCount*/ 1, expressionNode);
                    m_contextStack.top().pushFunctionResult(actualArguments);
                    trace("return() result: ", actualArguments);
                }
                m_contextStack.top().setFlowControlStatement(FlowControlStatement.Return);
                
                result.type = VariableType.BOOLEAN;
                result.value = ["true"];
                
                trace("return() statement eval'd");
                break;
            }
            case "QMakeProject.BreakFunctionCall":
            {
                // BreakFunctionCall <- "break" OPEN_PAR_WS CLOSE_PAR_WS
                // FIXME: implement
                throw new NotImplementedException("break()");
                //break;
            }
            case "QMakeProject.NextFunctionCall":
            {
                // NextFunctionCall <- "next" OPEN_PAR_WS CLOSE_PAR_WS
                // FIXME: implement
                throw new NotImplementedException("next()");
                //break;
            }
            case "QMakeProject.ErrorFunctionCall":
            {
                // ErrorFunctionCall <- "error" OPEN_PAR_WS (List(:space+, :space) / FunctionArgument(FunctionArgumentStopRule) / Statement)? CLOSE_PAR_WS
                // FIXME: implement
                throw new NotImplementedException("error()");
                //break;
            }
            case "QMakeProject.RequiresFunctionCall":
            {
                // RequiresFunctionCall <- "requires" OPEN_PAR_WS BooleanExpression CLOSE_PAR_WS
                // FIXME: implement
                //throw new NotImplementedException("requires()");
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

    int evalFunctionActualArgumentCount(ref ParseTree functionArgumentListNode)
    in
    {
        assert(functionArgumentListNode.name == "QMakeProject.FunctionArgumentList");
        assert(functionArgumentListNode.children.length == 1);
    }
    do
    {
        int result = 0;

        if (functionArgumentListNode.children[0].name.startsWith("QMakeProject.List!")) // comma or whitespace-separated list
        {
            auto listNode = functionArgumentListNode.children[0];
            for (int i = 0; i < listNode.children.length; i++)
            {
                if (listNode.children[i].name.startsWith("QMakeProject.FunctionArgument"))
                    result ++;
            }
        }
        else if (functionArgumentListNode.children[0].name.startsWith("QMakeProject.FunctionArgument!"))    // single argument
        {
            result = 1;
        }
        else
            throw new EvalLogicalException("Unknown function argument type");
        
        return result;
    }

    string[] evalFunctionActualArguments(const int actualOperandCount, ref ParseTree functionArgumentListNode)
    in
    {
        assert(functionArgumentListNode.name == "QMakeProject.FunctionArgumentList"
            || functionArgumentListNode.name == "QMakeProject.ReturnFunctionArguments");
        assert(functionArgumentListNode.children.length == 1);
    }
    do
    {
        string[] result;

        if (functionArgumentListNode.children[0].name.startsWith("QMakeProject.List!")) // comma or whitespace-separated list
        {
            auto listNode = functionArgumentListNode.children[0];

            trace("Compile-time function argument count: ", evalFunctionArgumentListLength(listNode));
            auto actualArgumentsTemp = evalFunctionArgumentList(listNode);
            trace("Run-time function argument count: ", actualArgumentsTemp.length);
            
            assert(actualOperandCount == actualArgumentsTemp.length);
            foreach (v; actualArgumentsTemp)
            {
                if (v.length == 1)
                {
                    trace("1-to-1 argument value mapping");
                    result ~= [v[0]];
                }
                else if (!v.empty)
                {
                    trace("1-to-many argument value mapping - join(whitespace)");
                    result ~= [v.join(" ")];
                }
            }
            assert(actualOperandCount == result.length);
        }
        else if (functionArgumentListNode.children[0].name.startsWith("QMakeProject.FunctionArgument!"))    // single argument
        {
            trace("Compile-time function argument count: 1");
            string[] expandArgumentValue = evalFunctionArgument(functionArgumentListNode.children[0]);
            trace("Run-time function argument count: ", expandArgumentValue.length);

            if (expandArgumentValue.length == 1)
            {
                trace("1-to-1 argument value mapping");
                result = [expandArgumentValue[0]];
            }
            else if (!expandArgumentValue.empty)
            {
                trace("1-to-many argument value mapping - join(whitespace)");
                result = [expandArgumentValue.join(" ")];
            }
        }
        else
            throw new EvalLogicalException("Unknown function argument type");
        
        return result;
    }

    RvalueEvalResult evalFunctionNode(ref ParseTree functionCallNode, ProFunctionType functionType)
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
                    throw new EvalLogicalException("Unsupported replace function '" ~ functionName ~ "'");
                break;
            case ProFunctionType.Test:
                if (m_contextStack.top().hasTestFunctionDescription(functionName))
                    functionDescription = m_contextStack.top().getTestFunctionDescription(functionName);
                else
                    throw new EvalLogicalException("Unsupported test function '" ~ functionName ~ "'");
                break;
            default:
                throw new EvalLogicalException("Unsupported function type'" ~ to!string(functionType) ~ "'");
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
            actualOperandCount = evalFunctionActualArgumentCount(functionArgumentListNode);
        }
        else if (functionArgumentListNode.name == "QMakeProject.CLOSE_PAR_WS")
        {
        }
        else
            throw new EvalLogicalException("function argument list or closing parenthehis expected");

        trace("Actual function ", "`", functionName, "`", " argument count: ", actualOperandCount);

        validateFunctionCall(actualOperandCount, functionDescription);
        
        // Get function argument values
        string[] actualArguments;
        if (actualOperandCount > 0)
        {
            actualArguments = evalFunctionActualArguments(actualOperandCount, functionArgumentListNode);
        }
        trace("Actual function ", "`", functionName, "`", " arguments: ", actualArguments);

        // Invoke function
        const(string[]) resultAsList = functionDescription.action(m_contextStack.top(), m_persistentStorage, actualArguments);
        if (functionDescription.fti.returnType == VariableType.STRING_LIST)
        {
            trace("Function ", "`", functionName, "`", " returns list: ", resultAsList);
        }
        else
        {
            trace("Function ", "`", functionName, "`", " returns string: ", "`", resultAsList[0], "`");
        }
        result.type = functionDescription.fti.returnType;
        result.value = resultAsList.dup;

        return result;
    }

    long evalFunctionArgumentListLength(ref ParseTree listNode)
    in
    {
        assert(listNode.name.startsWith("QMakeProject.List!")); // comma or whitespace-separated list
    }
    do
    {
        return listNode.children.length;
    }

    string[][] evalFunctionArgumentList(ref ParseTree listNode)
    in
    {
        assert(listNode.name.startsWith("QMakeProject.List!")); // comma or whitespace-separated list
    }
    do
    {
        auto result = new string[][](evalFunctionArgumentListLength(listNode));
        for (int i = 0; i < listNode.children.length; i++)
        {
            assert(listNode.children[i].name.startsWith("QMakeProject.FunctionArgument!"));

            string[] temp = evalFunctionArgument(listNode.children[i]);
            if (!temp.empty)
                result[i] ~= temp;
        }
        return result;
    }

    // NOTE: in compile-time function argument is a single token, but in runtime in can expand to list;
    //       message($$QMAKE_PLATFORM) ---> message(linux unix posix)
    //       must not be confused with chain statements:
    //       message($${var}/dir/file.txt) ---> message(path/dir/file.txt)
    //       i.e. one token stay one
    string[] evalFunctionArgument(ref ParseTree argumentNode)
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

    bool evalBooleanVariableNode(ref ParseTree boolAtomNode)
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

    RvalueEvalResult evalRvalueChain(ref ParseTree rvalueChainNode)
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

    RvalueEvalResult evalRvalueAtomNode(ref ParseTree rvalueAtomMetaNode)
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
                        throw new EvalLogicalException("Undefined project variable found");
                    
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
                        //throw new EvalLogicalException("Undefined project variable found");
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
                    //    throw new EvalLogicalException("Undefined project variable found");
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
                        throw new EvalLogicalException("Undefined persistent property found");

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

    void validateFunctionCall(const int actualOperandCount, ref ProFunction functionDescription)
    {
        // Report function expected and actual argument count (if applicable)
        // FIXME: implement user-defined functions support and add condition
        // `&& !functionDescription.m_isUserDefined`
        if (!functionDescription.fti.isVariadic)
        {
            string temp;
            if (functionDescription.fbi.requiredArgumentCount > 0)
                temp ~= to!string(functionDescription.fbi.requiredArgumentCount) ~ " " ~ "required";
            if (functionDescription.fbi.optionalArgumentCount)
                temp ~= ", " ~ to!string(functionDescription.fbi.optionalArgumentCount) ~ " " ~ "optional";
            trace("Expected replace function count: ", temp);
        }
        else
        {
            trace("Replace function is variadic one, no required/optional argument count was specified");
        }

        // Validate arguments count for non-variadic functions
        if (!functionDescription.fti.isVariadic)
        {
            if (functionDescription.fbi.requiredArgumentCount < 0)
            {
                throw new EvalLogicalException(
                        "Invalid function '" ~ functionDescription.fbi.functionName ~ "' argument count description");
            }

            if (functionDescription.fbi.optionalArgumentCount < 0)
            {
                if (actualOperandCount < functionDescription.fbi.requiredArgumentCount)
                {
                    throw new  /*RangeError*/ Exception(
                            "Invalid number of arguments for function '" ~ functionDescription.fbi.functionName ~ "': " ~ to!string(
                            functionDescription.fbi.requiredArgumentCount) ~ " expected, but " ~ to!string(
                            actualOperandCount) ~ " given");
                }
            }
            else
            {
                long minArgCount = functionDescription.fbi.requiredArgumentCount;
                long maxArgCount = minArgCount + functionDescription.fbi.optionalArgumentCount;
                if ((actualOperandCount < minArgCount) || (actualOperandCount > maxArgCount))
                {
                    throw new  /*RangeError*/ Exception(
                            "Invalid number of arguments for function '" ~ functionDescription.fbi.functionName ~ "': from " ~ to!string(
                            minArgCount) ~ " to " ~ to!string(
                            maxArgCount) ~ " expected, but " ~ to!string(actualOperandCount) ~ " given");
                }
            }
        }
    }
}

unittest
{
    import std.stdio : writeln, writefln, readln, stdin;
    import std.conv : to;
    static import std.file, std.path;
    import source.qt;

    immutable(QtVersionInfo) qtInfo = chooseQtSourceVersion();
    immutable(string) mkspec = QtMakeSpecification.detectHostMakeSpec();
    assert(!mkspec.empty);

    auto context = new ProExecutionContext();
    auto storage = new PersistentPropertyStorage(qtInfo, mkspec);
    auto pro = new Project(context, storage);

    string qtPath = qtInfo.qtRootDir;
    assert(isValidDirectoryPath(qtPath), "Invalid Qt source directory path");

    assert(pro.tryParseSnippet(
`
    error( "Couldn't find the manual.pri file!" )
    error("Hunspell dictionaries are missing! Please copy .dic and .aff" \
                  "files to src/virtualkeyboard/3rdparty/hunspell/data directory.")
`
    ));

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
            log("QDoc build is disabled on MinGW in Qt, because of a missing feature in the release infrastructure.(\\n)")
            log("You can enable it by setting FORCE_MINGW_QDOC_BUILD")
            break()
    }
}
`));

    // Qt projects
    assert(pro.tryParse(buildPath(qtPath, "qt.pro")));
    assert(pro.tryParse(buildPath(qtPath, "qtbase/mkspecs/features/qt_configure.prf")));
    assert(pro.tryParse(buildPath(qtPath, "qt3d/qt3d.pro")));
    assert(pro.tryParse(buildPath(qtPath, "qt3d/tests/auto/render/render.pro")));
    assert(pro.tryParse(buildPath(qtPath, "qtmultimedia/examples/multimedia/multimedia.pro")));
    assert(pro.tryParse(buildPath(qtPath, "qtremoteobjects/tests/auto/integration_multiprocess/server/server.pro")));
    assert(pro.tryParse(buildPath(qtPath, "qtbase/tests/auto/network/ssl/ssl.pro")));
    assert(pro.tryParse(buildPath(qtPath, "qtbase/src/plugins/sqldrivers/configure.pri")));
    assert(pro.tryParse(buildPath(qtPath, "qtbase/src/widgets/util/util.pri")));
    assert(pro.tryParse(buildPath(qtPath, "qtbase/mkspecs/features/spec_post.prf")));
    assert(pro.tryParse(buildPath(qtPath, "qtbase/mkspecs/features/winrt/package_manifest.prf")));
    assert(pro.tryParse(buildPath(qtPath, "qtbase/mkspecs/features/qt_configure.prf")));
    assert(pro.tryParse(buildPath(qtPath, "qtdeclarative/tests/auto/quick/pointerhandlers/pointerhandlers.pro")));
    assert(pro.tryParse(buildPath(qtPath, "qtdeclarative/tests/auto/qml/qml.pro")));
    assert(pro.tryParse(buildPath(qtPath, "qtremoteobjects/src/repparser/repparser.pro")));

    string[] successfulProjects;
    string[] failedProjects;
    
    bool parseQtSourceProjects(string qtDir, ref Project qmakeProject)
    {
        auto projectFiles = std.file.dirEntries(qtDir, std.file.SpanMode.depth).filter!(
            f => f.name.endsWith(".pro") || f.name.endsWith(".pri") || f.name.endsWith(".prf")  || f.name.endsWith(".conf")
        );
    
        int successfulCount, failedCount;
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
                || d.name.indexOf("tests/testserver/") != -1
                || d.name.indexOf("qtquickcontrols2/tests/auto/qquickmaterialstyleconf/") != -1
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

            if (qmakeProject.tryParse(d.name))
            {
                successfulCount++;
                successfulProjects ~= d.name;
            }
            else
            {
                failedCount++;
                failedProjects ~= d.name;
            }

            writefln("Project: " ~ d.name);
        }

        immutable int totalCount = successfulCount + failedCount;
        writeln("\n===============================================================================================");
        writefln("[%s] Total file count: " ~ to!string(totalCount), qtDir);
        writefln("[%s] Successfully parsed: " ~ to!string(successfulCount), qtDir);
        writefln("[%s] Failed to parse: " ~ to!string(failedCount) ~ " or "
            ~ to!string(100.0f * failedCount / totalCount) ~ "%%", qtDir);
        if (failedCount > 0)
        {
            writefln("");
            writefln("Failed projects:");
            foreach (project; failedProjects)
                writefln(project);
        }
        writeln("\n===============================================================================================");

        return ((successfulCount > 0) && (failedCount == 0)) ? true : false;
    }

    bool result = parseQtSourceProjects(buildPath(qtPath), pro);

    writeln("\n===============================================================================================");
    writeln("===============================================================================================");
    writeln("===============================================================================================\n");

    if (failedProjects.length > 0)
    {
        writefln("");
        writefln("[FINAL] Failed to parse projects:");
        foreach (project; failedProjects)
            writefln(project);
    }
    else
    {
        writefln("[FINAL] All %d projects were successfully parsed!", successfulProjects.length);
    }
    assert(result);
}
