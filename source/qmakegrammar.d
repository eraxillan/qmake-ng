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

private enum QMakeProjectEmptyStatement =
`
    # No input
    EmptyStatement <- eps :LineTerminator*
`;

private enum QMakeProjectComment =
`
    # Comment, single-line and multi-line (extension)
    Comment                         <~ MultiLineComment / SingleLineComment
    MultiLineComment                <~ :"#*" (!"*#" SourceCharacter)* :"*#"
    SingleLineComment               <- :"#" SingleLineCommentChars? :LineTerminator
    SingleLineCommentChars          <- SingleLineCommentChar+
    SingleLineCommentChar           <- !LineTerminator SourceCharacter
`;

private enum QMakeProjectCodeBlock =
`
    # Code block
    Block           <- SingleLineBlock / MultiLineBlock
    SingleLineBlock <- :space* "@" :space* Statement
    MultiLineBlock  <- :space* "{" :space* :LineTerminator* :space* Statement+ :space* :LineTerminator* "}" :space* :LineTerminator*
`;

private enum QMakeProjectAssignment =
`
    # Variable or variable property assignment
    Assignment <- StandardAssignment / ReplaceAssignment

    StandardAssignment <- QualifiedIdentifier :space* StandardAssignmentOperator :space* RvalueExpression? :LineTerminator*
    ReplaceAssignment  <- QualifiedIdentifier :space* ReplaceAssignmentOperator :space* RegularExpression? :LineTerminator*

    StandardAssignmentOperator <- "+=" / "*=" / "-=" / "="
    ReplaceAssignmentOperator  <- "~="
`;

private enum QMakeProjectRvalue =
`
    # Base rvalue statement: function call or variable/property expand
    RvalueAtom <- ReplaceFunctionCall / ExpandStatement / TestFunctionCall

    RvalueStopRule <- space / quote / doublequote

    RvalueExpression       <- RvalueList / RvalueChain(RvalueStopRule)
    RvalueList             <- RvalueChain(RvalueStopRule) (:space+ RvalueChain(RvalueStopRule))+
    RvalueChain(T)         <- Rvalue(T) Rvalue(T)*
    Rvalue(T)              <- RvalueAtom / EnquotedRvalue / Leftover(T)

    EnquotedRvalue         <- DoubleEnquotedRvalue / SingleEnquotedRvalue
    DoubleEnquotedRvalue   <- doublequote RvalueChain(doublequote)? doublequote
    SingleEnquotedRvalue   <- quote RvalueChain(quote)? quote

    Leftover(StopPattern)         <- ~(LeftoverChar(StopPattern)+)
    LeftoverStopChar(StopPattern) <- LineTerminator / ExpandStatement / BACKSLASH / StopPattern
    LeftoverChar(StopPattern)     <- / !LeftoverStopChar(StopPattern) SourceCharacter
                                     / BACKSLASH EscapeSequence

    RegularExpression         <- ~(RegularExpressionChar+)
    RegularExpressionStopChar <- LineTerminator
    RegularExpressionChar     <- !RegularExpressionStopChar SourceCharacter
`;

private enum QMakeProjectFunctionCall =
`
    # Test function call
    # E.g.:
    # message("Starting project build...")
    TestFunctionCall <- / EvalTestFunctionCall
                        / CacheTestFunctionCall
                        / ContainsTestFunctionCall
                        / ReturnFunctionCall
                        / BreakFunctionCall
                        / NextFunctionCall
                        / ErrorFunctionCall
                        / RequiresFunctionCall
                        / FunctionCall

    # Replace function call
    # E.g.:
    # $$escape_expand("One\nTwo\nThree")
    ReplaceFunctionCall <- EXPAND_MARKER TestFunctionCall

    # NOTE: "$${call}($$opt, $$val, $$nextok)" is also a valid function call statement;
    #       also is \$\$"$$call"()
    FunctionCall <- FunctionId OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS
    CustomFunctionId <- / "defineReplace" / "defineTest"
                        / "eval" / "cache" / "contains" / "requires"
                        / "return" / "break" / "next" / "error"
    FunctionId   <- !CustomFunctionId
                    ("{" :space* QualifiedIdentifier :space* "}" / QualifiedIdentifier / EnquotedString)
`;

private enum QMakeProjectFunctionArguments =
`
    FunctionArgumentStopRule   <- space / COMMA

    FunctionArgumentList       <- List(:COMMA_WS, :COMMA) / List(:space+, :space) / FunctionArgument(FunctionArgumentStopRule)
    List(delimRule, delimChar) <- FunctionArgument(FunctionArgumentStopRule) (delimRule (FunctionArgument(delimChar))?)+

    FunctionArgument(delim)           <- FunctionArgumentImpl(delim) (FunctionArgumentImpl(delim))*
    FunctionArgumentImpl(delim)       <- / RvalueAtom
                                         / EnquotedFunctionArgument(delim)
                                         / FunctionArgumentString(delim)

    EnquotedFunctionArgument(delim)         <- DoubleEnquotedFunctionArgument(delim) / SingleEnquotedFunctionArgument(delim)
    DoubleEnquotedFunctionArgument(delim)   <- doublequote EnquotedFunctionArgumentChain(delim, doublequote)? doublequote
    SingleEnquotedFunctionArgument(delim)   <- quote EnquotedFunctionArgumentChain(delim, quote)? quote
    EnquotedFunctionArgumentChain(delim, Q) <- FunctionArgumentImpl_2(delim, Q) FunctionArgumentImpl_2(delim, Q)*
    FunctionArgumentImpl_2(delim, Q)        <- / RvalueAtom
                                               / EnquotedFunctionArgument(delim)
                                               / FunctionArgumentString(delim)
                                               / Leftover(Q)

    FunctionArgumentString(delim)         <- ~(FunctionArgumentStringChar(delim)+)
    FunctionArgumentStringStopChar(delim) <- :(delim / LineTerminator / ExpandStatement / quote / doublequote / BACKSLASH / EndOfFunction)
    FunctionArgumentStringChar(delim)     <- !FunctionArgumentStringStopChar(delim) SourceCharacter
                                           / BACKSLASH EscapeSequence

    # NOTE: function arguments can contain "("/")" themselves, so we need special rule to detect function argument list end
    EndOfFunction <- ")" :space* (
        / [a-zA-Z_0-9\-\+\*/]
        / eoi / LineTerminator
        / "=" / "+=" / "*=" / "-=" / "~="
        / "," / "." / "_"
        / "(" / ")"
        / EXPAND_MARKER
        / "@" / "{" / "}" / ":" / "|"
        / "\""
    )
`;

private enum QMakeProjectFunctionDeclaration =
`
    FunctionDeclaration        <- ReplaceFunctionDeclaration / TestFunctionDeclaration
    ReplaceFunctionDeclaration <- "defineReplace" OPEN_PAR_WS QualifiedIdentifier CLOSE_PAR_WS Block
    TestFunctionDeclaration    <- "defineTest"    OPEN_PAR_WS QualifiedIdentifier CLOSE_PAR_WS Block
`;

private enum QMakeProjectScope =
`
    # Conditional statement
    # E.g.:
    # CONFIG(debug, debug|release)@ buildmode = debug
    #
    # NOTE: preprocessor replace terminal ":" after condition with "@" to eliminate ambiguity
    #
    Scope             <- BooleanExpression ScopeMainBranch ScopeElseIfBranch* ScopeElseBranch?
    ScopeMainBranch   <- Block
    ScopeElseIfBranch <- "else@" :space* BooleanExpression Block
    ScopeElseBranch   <- / "else@" :space* Statement
                         / "else"  MultiLineBlock
`;

private enum QMakeProjectBooleanExpression =
`
    # Compound boolean expression
    # E.g.:
    # (win32:release)|!exists($$QMAKE_QT_CONFIG)|!include($$QMAKE_QT_CONFIG, "", true): message("fail")
    BooleanExpression            <- LogicalORExpression
    LogicalORExpression          <- LogicalANDExpression (:space* "|" :space* LogicalANDExpression)*
    LogicalANDExpression         <- LogicalNOTExpression (:space* ":" :space* LogicalNOTExpression)*
    LogicalNOTExpression         <- (:space* "!" :space*)? PrimaryBooleanExpression
    PrimaryBooleanExpression     <- ParenthesedBooleanExpression
                                  / IfTestFunctionCall
                                  / BooleanAtom
    ParenthesedBooleanExpression <- '(' BooleanExpression ')'
    
    # FIXME: support CONFIG test function
    
    IfTestFunctionCall <- "if" OPEN_PAR_WS BooleanExpression CLOSE_PAR_WS
    BooleanAtom        <- / ReplaceFunctionCall
                          / TestFunctionCall
                          / QualifiedIdentifier
                          / BooleanConst
    BooleanConst       <- "true" / "false"
`;

private enum QMakeProjectForStatement =
`
    # Loop statement (foreach and while(true) idioms)
    ForStatement <- ForEachInListStatement / ForEverStatement
    ForEachInListStatement <- "for" OPEN_PAR_WS ForIteratorVariableName COMMA_WS ForIterableList CLOSE_PAR_WS Block
    ForIteratorVariableName <- QualifiedIdentifier
    ForIterableList <- List(:space+, :space) / FunctionArgument(FunctionArgumentStopRule)
    ForEverStatement <- "for" OPEN_PAR_WS "ever" CLOSE_PAR_WS Block
`;

private enum QMakeProjectExpandStatement =
`
    ExpandStatement                    <- / FunctionArgumentExpandStatement
                                          / ProjectVariableExpandStatement
                                          / EnvironmentVariableExpandStatement
                                          / PropertyVariableExpandStatement
                                          / MakefileVariableExpandStatement
                                          
    FunctionArgumentExpandStatement    <- / "$$" DecNumber
                                          / "$${" DecNumber "}"
    MakefileVariableExpandStatement    <- / "$" QualifiedIdentifier
                                          / "${" QualifiedIdentifier "}"
    ProjectVariableExpandStatement     <- / "$$" QualifiedIdentifier
                                          / "$${" QualifiedIdentifier "}"
                                          # E.g. result = \$\$"$$call"
                                          / "$$" doublequote ExpandStatement doublequote
    EnvironmentVariableExpandStatement <- ("$$(" / "$(") QualifiedIdentifier ")"
    PropertyVariableExpandStatement    <- "$$[" QualifiedIdentifier ("/get" / "/src")? "]"
`;

private enum QMakeProjectBuiltinFunctions =
`
    # Some built-in replace and test functions that cause difficulties during parsing

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
    CacheTestFunctionCallParams <- QualifiedIdentifier? (COMMA_WS CacheTestFunctionParam2)? (COMMA_WS FunctionArgument(FunctionArgumentStopRule))?
    CacheTestFunctionParam2     <- ("set" / "add" / "sub")? :space* ("transient")? :space* ("super" / "stash")?
    
    # contains(variablename, value)
    ContainsTestFunctionCall <- "contains" OPEN_PAR_WS QualifiedIdentifier (COMMA_WS EnquotedString) CLOSE_PAR_WS

    # Program flow control statements: return, break, next (aka continue)
    # return(expression)
    ReturnFunctionCall <- "return" OPEN_PAR_WS ReturnFunctionArguments? CLOSE_PAR_WS
    ReturnFunctionArguments <- List(:space+, :space) / FunctionArgument(FunctionArgumentStopRule)

    # break()
    BreakFunctionCall <- "break" OPEN_PAR_WS CLOSE_PAR_WS

    # next()
    NextFunctionCall <- "next" OPEN_PAR_WS CLOSE_PAR_WS

    # error() / error("message")
    # NOTE: multiline string must be merged in interpreter
    ErrorFunctionCall <- "error" OPEN_PAR_WS (List(:space+, :space) / FunctionArgument(FunctionArgumentStopRule) / Statement)? CLOSE_PAR_WS

    # requires(condition)
    RequiresFunctionCall <- "requires" OPEN_PAR_WS BooleanExpression CLOSE_PAR_WS
`;

private enum QMakeProjectEnquotedString =
`
    # Enquoted string: can contain any character except of quote
    EnquotedString            <- DoubleEnquotedString / SingleEnquotedString
    DoubleEnquotedString      <- doublequote ~(NonDoubleQuoteCharacter*) doublequote
    NonDoubleQuoteCharacter   <- !(doublequote / BACKSLASH / LineTerminator) SourceCharacter / BACKSLASH EscapeSequence
    SingleEnquotedString      <- quote ~(NonSingleQuoteCharacter*) quote
    NonSingleQuoteCharacter   <- !(quote / BACKSLASH / LineTerminator) SourceCharacter / BACKSLASH EscapeSequence
`;

private enum QMakeProjectIdentifier =
`
    # NOTE: "a-b" and "c++11" are valid qmake identifiers too
    #Identifier <- identifier
    Identifier      <~ [a-zA-Z_] [a-zA-Z_0-9\-\+\*]*
    QMakeIdentifier <~ [_a-zA-Z0-9\-+*]+

    # lvalue
    # FIXME: need further investigion! e.g. what another number-returning functions exist
    NumberFunctionCall  <- EXPAND_MARKER "size" OPEN_PAR_WS QualifiedIdentifier CLOSE_PAR_WS

    LValueImpl          <- NumberFunctionCall / ExpandStatement / QMakeIdentifier
    LValue              <- LValueImpl LValueImpl*
    QualifiedIdentifier <~ "."? LValue ("." LValue)*
`;

private enum QMakeProjectEscapeSequence =
`
    EscapeSequence <-
        / quote
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
        / "|"
        / "("
        / ")"
        / "["
        / "]"
        / "{"
        / "}"
`;

private enum QMakeProjectTerminals =
`
    OctDigit <- [0-7]
    DecDigit <- [0-9]
    HexDigit <- [0-9a-fA-F]
    DecNumber <- DecDigit+

    # FIXME: implement second rule as escape sequence
    EXPAND_MARKER <- "$$" / "\\$\\$"

    COMMA        <- ","
    COMMA_WS     <- :space* "," :space*
    OPEN_PAR_WS  <- :space* "(" :space*
    CLOSE_PAR_WS <- :space* ")"

    BACKSLASH <- "\\"

    SourceCharacter <- [\u0000-\uFFFC]
    LineTerminator  <- / "\u000A" # LF
                       / ("\u000D" "\u000A") # CR LF
                       / "\u2028" / "\u2029" # LineSeparator / ParagraphSeparator
`;

// --------------------------------------------------------------------------------------------------------------------

/**
  * Qt qmake project files grammar description, using extended PEG syntax;
  / `qmakeparser.d` module will be generated from it during `make.d` compilation
  */
public enum QMakeGrammar =
`
    QMakeProject:
        Project <- Statement* eoi
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
`
    ~ QMakeProjectEmptyStatement
    ~ QMakeProjectComment
    ~ QMakeProjectCodeBlock
    ~ QMakeProjectAssignment
    ~ QMakeProjectRvalue
    ~ QMakeProjectFunctionCall
    ~ QMakeProjectFunctionArguments
    ~ QMakeProjectFunctionDeclaration
    ~ QMakeProjectScope
    ~ QMakeProjectBooleanExpression
    ~ QMakeProjectForStatement
    ~ QMakeProjectExpandStatement
    ~ QMakeProjectBuiltinFunctions
    ~ QMakeProjectEnquotedString
    ~ QMakeProjectIdentifier
    ~ QMakeProjectEscapeSequence
    ~ QMakeProjectTerminals;
