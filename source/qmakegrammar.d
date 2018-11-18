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

module qmakegrammar;

enum QMakeGrammar = `
    QMakeProject:
        Project <- Statement* eoi
        Statement <- FunctionDeclaration / Assignment / ForStatement / Scope / Block / BooleanExpression / ReplaceFunctionCall / TestFunctionCall / Comment / EmptyStatement

        # No input
        EmptyStatement <- eps :eol*

        # Code block
        Block           <- SingleLineBlock / MultiLineBlock
        SingleLineBlock <- :space* "@" :space* Statement
        MultiLineBlock  <- :space* "{" :space* :eol* :space* Statement+ :space* :eol* "}" :space* :eol*

        # Comment, single-line and multi-line (extension)
        Comment                         <~ MultiLineComment / SingleLineComment
        MultiLineComment                <~ :"#*" (!"*#" SourceCharacter)* :"*#"
        SingleLineComment               <- :"#" SingleLineCommentChars? :eol
        SingleLineCommentChars          <- SingleLineCommentChar+
        SingleLineCommentChar           <- !LineTerminator SourceCharacter

        # Variable or variable property assignment
        Assignment <- (DirectAssignment / AppendAssignment / AppendUniqueAssignment / RemoveAssignment / ReplaceAssignment)
 
        StringList              <- String (:space* String)*
        String                  <- StringChars?
        StringChars             <- ~(StringChar+)
        StringChar              <- !(blank / LineTerminator) SourceCharacter
        
        DirectAssignment        <- QualifiedIdentifier :space* "="  :space* StringList? :eol*
        AppendAssignment        <- QualifiedIdentifier :space* "+=" :space* StringList? :eol*
        AppendUniqueAssignment  <- QualifiedIdentifier :space* "*=" :space* StringList? :eol*
        RemoveAssignment        <- QualifiedIdentifier :space* "-=" :space* StringList? :eol*
        ReplaceAssignment       <- QualifiedIdentifier :space* "~=" :space* StringList? :eol*

        # Test function call
        # E.g.:
        # message("Starting project build...")
        TestFunctionCall <- EvalTestFunctionCall / CacheTestFunctionCall / ContainsTestFunctionCall
                          / ReturnFunctionCall / RequiresFunctionCall
                          / FunctionCall

        # Replace function call
        # E.g.:
        # $$escape_expand("One\nTwo\nThree")
        ReplaceFunctionCall <- EXPAND_MARKER TestFunctionCall

        # NOTE: "$${call}($$opt, $$val, $$nextok)" is also a valid function call statement;
        #       also is \$\$"$$call"()
        FunctionCall <- FunctionId OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS :space* :eol*
        FunctionId   <- (!("defineReplace" / "defineTest" / "eval" / "cache" / "contains" / "return" / "requires")
                        ("{" :space* QualifiedIdentifier :space* "}" / QualifiedIdentifier / EnquotedString))

        FunctionArgumentList       <- List(COMMA_WS, COMMA) / List(:space+, :space) / FunctionFirstArgument
        List(delimRule, delimChar) <- FunctionFirstArgument (delimRule (FunctionNextArgument(delimChar))?)+
        
        FunctionFirstArgument           <- FunctionFirstArgumentImpl FunctionFirstArgumentImpl*
        FunctionFirstArgumentImpl       <- ReplaceFunctionCall / ExpandStatement / TestFunctionCall / EnquotedString / FunctionFirstArgumentString
        FunctionFirstArgumentString     <- ~(FunctionFirstArgumentStringChar+)
        FunctionFirstArgumentStringChar <- !(eol / EXPAND_MARKER / space / COMMA / quote / doublequote / BACKSLASH / EndOfFunction) SourceCharacter
                                           / BACKSLASH EscapeSequence

        FunctionNextArgument(delim)           <- FunctionNextArgumentImpl(delim) (FunctionNextArgumentImpl(delim))*
        FunctionNextArgumentImpl(delim)       <- ReplaceFunctionCall / ExpandStatement / TestFunctionCall / EnquotedString / FunctionNextArgumentString(delim)
        FunctionNextArgumentString(delim)     <- ~(FunctionNextArgumentStringChar(delim)+)
        FunctionNextArgumentStringChar(delim) <- !(eol / EXPAND_MARKER / delim / quote / doublequote / BACKSLASH / EndOfFunction) SourceCharacter
                                               / BACKSLASH EscapeSequence

        # NOTE: function arguments can contain "("/")" themselves, so we need special rule to detect function argument list end
        EndOfFunction <- ")" :space* (
            [a-zA-Z_0-9\-\+\*/]
            / eoi / eol
            / "=" / "+=" / "*=" / "-=" / "~="
            / "," / "." / "_"
            / "(" / ")"
            / EXPAND_MARKER
            / "@" / "{" / "}" / ":" / "|"
        )

        FunctionDeclaration        <- ReplaceFunctionDeclaration / TestFunctionDeclaration
        ReplaceFunctionDeclaration <- "defineReplace" OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS :space* :eol* Block
        TestFunctionDeclaration    <- "defineTest"    OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS :space* :eol* Block

        # Conditional statement
        # E.g.:
        # CONFIG(debug, debug|release):buildmode = debug
        Scope            <- BooleanExpression ScopeMainBranch ScopeElseIfBranch* ScopeElseBranch?
        ScopeMainBranch   <- Block
        ScopeElseIfBranch <- "else@" :space* BooleanExpression Block
        ScopeElseBranch   <- "else@" :space* Statement
                           / "else"  MultiLineBlock

        # E.g.:
        # var1: message("scope 1.1")
        # !exists($$QMAKE_QT_CONFIG)|!include($$QMAKE_QT_CONFIG, "", true) {
        BooleanExpression            <- LogicalORExpression
        LogicalORExpression          <- LogicalANDExpression (:space* "|" :space* LogicalANDExpression)*
        LogicalANDExpression         <- LogicalNOTExpression (:space* ":" :space* LogicalNOTExpression)*
        LogicalNOTExpression         <- (:space* "!" :space*)? PrimaryBooleanExpression
        PrimaryBooleanExpression     <- ParenthesedBooleanExpression
                                      / IfTestFunctionCall
                                      / BooleanAtom
        ParenthesedBooleanExpression <- '(' BooleanExpression ')'
# FIXME: support CONFIG test function
        IfTestFunctionCall           <- "if" OPEN_PAR_WS BooleanExpression CLOSE_PAR_WS
        BooleanAtom                  <- ReplaceFunctionCall / TestFunctionCall / QualifiedIdentifier / BooleanConst
        BooleanConst                 <- "true" / "false"

        # FIXME: move built-in test and replace function to separate module

        ForStatement <- "for" OPEN_PAR_WS ForIteratorVariableName COMMA_WS ForIterableList CLOSE_PAR_WS Block
        ForIteratorVariableName <- QualifiedIdentifier
        ForIterableList <- List(:space+, :space) / FunctionFirstArgument #/ Statement

        # eval(string)
        EvalTestFunctionCall <- "eval" OPEN_PAR_WS EvalArg CLOSE_PAR_WS
        EvalArg <- (QualifiedIdentifier :space* "=" :space* Statement) / Statement

        # cache(variablename, [set|add|sub] [transient] [super|stash], [source variablename])
        #
        # E.g.:
        # cache(CONFIG, add, $$list(config_clang))
        # cache(QMAKE_MAC_SDK.$${sdk}.$${info}, set stash, QMAKE_MAC_SDK.$${sdk}.$${info})
        # cache(QT.$${mod}.$$var, transient)
        #
        # Special case:
        # cache(, super)
        CacheTestFunctionCall       <- "cache" OPEN_PAR_WS CacheTestFunctionCallParams? CLOSE_PAR_WS
        CacheTestFunctionCallParams <- QualifiedIdentifier? (COMMA_WS CacheTestFunctionParam2)? (COMMA_WS FunctionFirstArgument)?
        CacheTestFunctionParam2     <- ("set" / "add" / "sub")? :space* ("transient")? :space* ("super" / "stash")?
        
        # contains(variablename, value)
        ContainsTestFunctionCall <- "contains" OPEN_PAR_WS QualifiedIdentifier (COMMA_WS EnquotedString) CLOSE_PAR_WS

        # return(expression)
        ReturnFunctionCall <- "return" OPEN_PAR_WS (List(:space+, :space) / FunctionFirstArgument / Statement)? CLOSE_PAR_WS
        
        # requires(condition)
        RequiresFunctionCall <- "requires" OPEN_PAR_WS BooleanExpression CLOSE_PAR_WS

        # Regular string: can contain any character except of spacing/EOL/quotes
        RegularString       <- RegularStringChars?
        RegularStringChars  <- ~(RegularStringChar+)
        RegularStringChar   <- !(blank / quote / doublequote) SourceCharacter

        # Enquoted string: can contain any character except of quote
        EnquotedString            <- DoubleEnquotedString / SingleEnquotedString
        DoubleEnquotedString      <- doublequote ~(NonDoubleQuoteCharacter*) doublequote
        NonDoubleQuoteCharacter   <- !(doublequote / BACKSLASH) SourceCharacter / BACKSLASH EscapeSequence
        SingleEnquotedString      <- quote ~(NonSingleQuoteCharacter*) quote
        NonSingleQuoteCharacter   <- !(quote / BACKSLASH) SourceCharacter / BACKSLASH EscapeSequence

        # NOTE: "a-b" and "c++11" are valid qmake identifiers too
        #Identifier <- identifier
        Identifier      <~ [a-zA-Z_] [a-zA-Z_0-9\-\+\*]*
        QMakeIdentifier <~ [a-zA-Z_0-9\-\+\*]+

        ExpandStatement                    <- ProjectVariableExpandStatement / EnvironmentVariableExpandStatement / PropertyVariableExpandStatement
        ProjectVariableExpandStatement     <- EXPAND_MARKER :space* QualifiedIdentifier
                                            / EXPAND_MARKER :space* "{" :space* QualifiedIdentifier :space* "}"
                                            # E.g. result = \$\$"$$call"
                                            / EXPAND_MARKER doublequote ExpandStatement doublequote
        EnvironmentVariableExpandStatement <- (EXPAND_MARKER / SINGLE_EXPAND_MARKER) OPEN_PAR_WS QualifiedIdentifier CLOSE_PAR_WS
        PropertyVariableExpandStatement    <- EXPAND_MARKER :space* "[" :space* QualifiedIdentifier ("/get" / "/src")? :space* "]"

        # lvalue
        LValueImpl          <- ExpandStatement / QMakeIdentifier
        LValue              <- LValueImpl LValueImpl*
        QualifiedIdentifier <~ "."? LValue ("." LValue)*

        EscapeSequence
           <- quote
            / doublequote
            / BACKSLASH
            / "$"
            / "."
            / "?"
            / "a"
            / "x" ~(HexDigit HexDigit HexDigit HexDigit)
            / ~(OctDigit OctDigit OctDigit)
            / "x" ~(HexDigit HexDigit)
            / "b"
            / "f"
            / "n"
            / "r"
            / "t"
            / "v"
            / "+"
            / "-"
            / "*"
            / "("
            / ")"
            / "["
            / "]"
            / "{"
            / "}"

        OctDigit <- [0-7]
        DecDigit <- [0-9]
        HexDigit <- [0-9a-fA-F]

        SINGLE_EXPAND_MARKER <- "$"
        # FIXME: implement second rule as escape sequence
        EXPAND_MARKER <- "$$" / "\\$\\$"

        COMMA        <- ","
        COMMA_WS     <- :space* "," :space*
        OPEN_PAR_WS  <- :space* "(" :space*
        CLOSE_PAR_WS <- :space* ")"
        
        BACKSLASH <- "\\"

        SourceCharacter <- [\u0000-\uFFFC]
        LineTerminator  <- "\u000A" / "\u000D" / "\u2028" / "\u2029"
    `;

