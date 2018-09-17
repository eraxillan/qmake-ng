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
import std.experimental.logger;

import preprocessor;
import qmakeparser;

import common_const;
import eval;
import project_variable;
import project_context;

class Project
{
    private string[][string] m_variables;

    const bool tryParse(in string fileName)
    {
        import qmakeparser;
        
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
    
    const bool eval(in string fileName)
    {       
        trace("Trying to parse project file '" ~ fileName ~ "'...");

        // FIXME: replace to QStack!ProExecutionContext to handle inner code blocks
        auto context = new ProExecutionContext();
        
        //
        context.assignVariable("PWD", [dirName(fileName)], VariableType.STRING);
        context.assignVariable("OUT_PWD", [dirName(fileName)], VariableType.STRING);
        context.assignVariable("_PRO_FILE_", [fileName], VariableType.STRING);
        context.assignVariable("_PRO_FILE_PWD_", [dirName(fileName)], VariableType.STRING);
        //

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
        // FIXME: check parseTree.children count

        foreach(ref child; projectNode.children)
        {
            auto statementNode = child.children[0];
            // FIXME: check child.children count

            trace("STATEMENT: " ~ statementNode.name ~ ": [" ~ statementNode.matches.join(" ") ~ "]");
            
            switch (statementNode.name)
            {
                case "QMakeProject.Assignment":
                {
                    auto variableName = statementNode.matches[0];
                    auto variableOperator = statementNode.matches[1];
                    auto variableValue = statementNode.matches[2 .. $];
                    trace("Variable name: '" ~ variableName ~ "'");
                    trace("Variable assignment operator: '" ~ variableOperator ~ "'");
                    trace("Variable value: '" ~ variableValue.join(" ") ~ "'");
                    
                    auto evaluator = new ExpressionEvaluator(context);
                    auto rpnExpression = ExpressionEvaluator.convertToRPN(variableValue.join(" "));
                    auto rpnResult = evaluator.evalRPN(rpnExpression);

                    switch (variableOperator)
                    {
                        case STR_EQUALS:
                            context.assignVariable(variableName, rpnResult, VariableType.STRING_LIST /*FIXME:*/);
                            break;
                        case STR_PLUS_EQUALS:
                            context.appendAssignVariable(variableName, rpnResult);
                            break;
                        case STR_ASTERISK_EQUALS:
                            context.appendUniqueAssignVariable(variableName, rpnResult);
                            break;
                        case STR_MINUS_EQUALS:
                            context.removeAssignVariable(variableName, rpnResult);
                            break;
                        case STR_TILDE_EQUALS:
                            throw new Exception("Not implemented yet");
                        default:
                            throw new Exception("Invalid assignment operator");
                    }

                    break;
                }
                case "QMakeProject.Scope":
                {
                    break;
                }
                default:
                {
                    error("Invalid statement type '" ~ statementNode.name ~ "'");
                    break;
                }
            }
        }

        trace("Project file '" ~ fileName ~ "' successfully evaluated");
        return true;
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
