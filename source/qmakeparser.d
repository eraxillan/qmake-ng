/++
This module was automatically generated from the following grammar:


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

    # No input
    EmptyStatement <- eps :LineTerminator*

    # Comment, single-line and multi-line (extension)
    Comment                         <~ MultiLineComment / SingleLineComment
    MultiLineComment                <~ :"#*" (!"*#" SourceCharacter)* :"*#"
    SingleLineComment               <- :"#" SingleLineCommentChars? :LineTerminator
    SingleLineCommentChars          <- SingleLineCommentChar+
    SingleLineCommentChar           <- !LineTerminator SourceCharacter

    # Code block
    Block           <- SingleLineBlock / MultiLineBlock
    SingleLineBlock <- :space* "@" :space* Statement
    MultiLineBlock  <- :space* "{" :space* :LineTerminator* :space* Statement+ :space* :LineTerminator* "}" :space* :LineTerminator*

    # Variable or variable property assignment
    Assignment <- StandardAssignment / ReplaceAssignment

    StandardAssignment <- QualifiedIdentifier :space* StandardAssignmentOperator :space* RvalueExpression? :LineTerminator*
    ReplaceAssignment  <- QualifiedIdentifier :space* ReplaceAssignmentOperator :space* RegularExpression? :LineTerminator*

    StandardAssignmentOperator <- "+=" / "*=" / "-=" / "="
    ReplaceAssignmentOperator  <- "~="

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

    # Test function call
    # E.g.:
    # message("Starting project build...")
    TestFunctionCall <- / EvalTestFunctionCall
                        / CacheTestFunctionCall
                        / ContainsTestFunctionCall
                        / ReturnFunctionCall
                        / RequiresFunctionCall
                        / FunctionCall

    # Replace function call
    # E.g.:
    # $$escape_expand("One\nTwo\nThree")
    ReplaceFunctionCall <- EXPAND_MARKER TestFunctionCall

    # NOTE: "$${call}($$opt, $$val, $$nextok)" is also a valid function call statement;
    #       also is \$\$"$$call"()
    FunctionCall <- FunctionId OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS
    FunctionId   <- (!("defineReplace" / "defineTest" / "eval" / "cache" / "contains" / "return" / "requires")
                    ("{" :space* QualifiedIdentifier :space* "}" / QualifiedIdentifier / EnquotedString))

    FunctionArgumentList       <- List(COMMA_WS, COMMA) / List(:space+, :space) / FunctionArgument(space / COMMA)
    List(delimRule, delimChar) <- FunctionArgument(space / COMMA) (delimRule (FunctionArgument(delimChar))?)+

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
    FunctionArgumentStringStopChar(delim) <- delim / LineTerminator / ExpandStatement / quote / doublequote / BACKSLASH / EndOfFunction
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

    FunctionDeclaration        <- ReplaceFunctionDeclaration / TestFunctionDeclaration
    ReplaceFunctionDeclaration <- "defineReplace" OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS Block
    TestFunctionDeclaration    <- "defineTest"    OPEN_PAR_WS FunctionArgumentList? CLOSE_PAR_WS Block

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

    # Loop statement (foreach and while(true) idioms)
    ForStatement <- ForEachInListStatement / ForEverStatement
    ForEachInListStatement <- "for" OPEN_PAR_WS ForIteratorVariableName COMMA_WS ForIterableList CLOSE_PAR_WS Block
    ForIteratorVariableName <- QualifiedIdentifier
    ForIterableList <- List(:space+, :space) / FunctionArgument(space / COMMA)
    ForEverStatement <- "for" OPEN_PAR_WS "ever" CLOSE_PAR_WS Block

    ExpandStatement                    <- / FunctionArgumentExpandStatement
                                          / ProjectVariableExpandStatement
                                          / MakefileVariableExpandStatement
                                          / EnvironmentVariableExpandStatement
                                          / PropertyVariableExpandStatement
    FunctionArgumentExpandStatement    <- / "$$" DecNumber
                                          / "$${" DecNumber "}"
    MakefileVariableExpandStatement    <- / "$" QualifiedIdentifier
                                          / "${" QualifiedIdentifier "}"
    ProjectVariableExpandStatement     <- / "$$" QualifiedIdentifier
                                          / "$${" QualifiedIdentifier "}"
                                          # E.g. result = \$\$"$$call"
                                          / "$$" doublequote ExpandStatement doublequote
    EnvironmentVariableExpandStatement <- ("$$" / "$") OPEN_PAR_WS QualifiedIdentifier CLOSE_PAR_WS
    PropertyVariableExpandStatement    <- "$$[" QualifiedIdentifier ("/get" / "/src")? "]"

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
    CacheTestFunctionCallParams <- QualifiedIdentifier? (COMMA_WS CacheTestFunctionParam2)? (COMMA_WS FunctionArgument(space / COMMA))?
    CacheTestFunctionParam2     <- ("set" / "add" / "sub")? :space* ("transient")? :space* ("super" / "stash")?
    
    # contains(variablename, value)
    ContainsTestFunctionCall <- "contains" OPEN_PAR_WS QualifiedIdentifier (COMMA_WS EnquotedString) CLOSE_PAR_WS

    # return(expression)
    ReturnFunctionCall <- "return" OPEN_PAR_WS (List(:space+, :space) / FunctionArgument(space / COMMA) / Statement)? CLOSE_PAR_WS

    # requires(condition)
    RequiresFunctionCall <- "requires" OPEN_PAR_WS BooleanExpression CLOSE_PAR_WS

    # Enquoted string: can contain any character except of quote
    EnquotedString            <- DoubleEnquotedString / SingleEnquotedString
    DoubleEnquotedString      <- doublequote ~(NonDoubleQuoteCharacter*) doublequote
    NonDoubleQuoteCharacter   <- !(doublequote / BACKSLASH / LineTerminator) SourceCharacter / BACKSLASH EscapeSequence
    SingleEnquotedString      <- quote ~(NonSingleQuoteCharacter*) quote
    NonSingleQuoteCharacter   <- !(quote / BACKSLASH / LineTerminator) SourceCharacter / BACKSLASH EscapeSequence

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


+/
module qmakeparser;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericQMakeProject(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct QMakeProject
    {
    enum name = "QMakeProject";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this()
    {
        rules["Project"] = toDelegate(&Project);
        rules["Statement"] = toDelegate(&Statement);
        rules["EmptyStatement"] = toDelegate(&EmptyStatement);
        rules["Comment"] = toDelegate(&Comment);
        rules["MultiLineComment"] = toDelegate(&MultiLineComment);
        rules["SingleLineComment"] = toDelegate(&SingleLineComment);
        rules["SingleLineCommentChars"] = toDelegate(&SingleLineCommentChars);
        rules["SingleLineCommentChar"] = toDelegate(&SingleLineCommentChar);
        rules["Block"] = toDelegate(&Block);
        rules["SingleLineBlock"] = toDelegate(&SingleLineBlock);
        rules["MultiLineBlock"] = toDelegate(&MultiLineBlock);
        rules["Assignment"] = toDelegate(&Assignment);
        rules["StandardAssignment"] = toDelegate(&StandardAssignment);
        rules["ReplaceAssignment"] = toDelegate(&ReplaceAssignment);
        rules["StandardAssignmentOperator"] = toDelegate(&StandardAssignmentOperator);
        rules["ReplaceAssignmentOperator"] = toDelegate(&ReplaceAssignmentOperator);
        rules["RvalueAtom"] = toDelegate(&RvalueAtom);
        rules["RvalueStopRule"] = toDelegate(&RvalueStopRule);
        rules["RvalueExpression"] = toDelegate(&RvalueExpression);
        rules["RvalueList"] = toDelegate(&RvalueList);
        rules["EnquotedRvalue"] = toDelegate(&EnquotedRvalue);
        rules["DoubleEnquotedRvalue"] = toDelegate(&DoubleEnquotedRvalue);
        rules["SingleEnquotedRvalue"] = toDelegate(&SingleEnquotedRvalue);
        rules["RegularExpression"] = toDelegate(&RegularExpression);
        rules["RegularExpressionStopChar"] = toDelegate(&RegularExpressionStopChar);
        rules["RegularExpressionChar"] = toDelegate(&RegularExpressionChar);
        rules["TestFunctionCall"] = toDelegate(&TestFunctionCall);
        rules["ReplaceFunctionCall"] = toDelegate(&ReplaceFunctionCall);
        rules["FunctionCall"] = toDelegate(&FunctionCall);
        rules["FunctionId"] = toDelegate(&FunctionId);
        rules["FunctionArgumentList"] = toDelegate(&FunctionArgumentList);
        rules["EndOfFunction"] = toDelegate(&EndOfFunction);
        rules["FunctionDeclaration"] = toDelegate(&FunctionDeclaration);
        rules["ReplaceFunctionDeclaration"] = toDelegate(&ReplaceFunctionDeclaration);
        rules["TestFunctionDeclaration"] = toDelegate(&TestFunctionDeclaration);
        rules["Scope"] = toDelegate(&Scope);
        rules["ScopeMainBranch"] = toDelegate(&ScopeMainBranch);
        rules["ScopeElseIfBranch"] = toDelegate(&ScopeElseIfBranch);
        rules["ScopeElseBranch"] = toDelegate(&ScopeElseBranch);
        rules["BooleanExpression"] = toDelegate(&BooleanExpression);
        rules["LogicalORExpression"] = toDelegate(&LogicalORExpression);
        rules["LogicalANDExpression"] = toDelegate(&LogicalANDExpression);
        rules["LogicalNOTExpression"] = toDelegate(&LogicalNOTExpression);
        rules["PrimaryBooleanExpression"] = toDelegate(&PrimaryBooleanExpression);
        rules["ParenthesedBooleanExpression"] = toDelegate(&ParenthesedBooleanExpression);
        rules["IfTestFunctionCall"] = toDelegate(&IfTestFunctionCall);
        rules["BooleanAtom"] = toDelegate(&BooleanAtom);
        rules["BooleanConst"] = toDelegate(&BooleanConst);
        rules["ForStatement"] = toDelegate(&ForStatement);
        rules["ForEachInListStatement"] = toDelegate(&ForEachInListStatement);
        rules["ForIteratorVariableName"] = toDelegate(&ForIteratorVariableName);
        rules["ForIterableList"] = toDelegate(&ForIterableList);
        rules["ForEverStatement"] = toDelegate(&ForEverStatement);
        rules["ExpandStatement"] = toDelegate(&ExpandStatement);
        rules["FunctionArgumentExpandStatement"] = toDelegate(&FunctionArgumentExpandStatement);
        rules["MakefileVariableExpandStatement"] = toDelegate(&MakefileVariableExpandStatement);
        rules["ProjectVariableExpandStatement"] = toDelegate(&ProjectVariableExpandStatement);
        rules["EnvironmentVariableExpandStatement"] = toDelegate(&EnvironmentVariableExpandStatement);
        rules["PropertyVariableExpandStatement"] = toDelegate(&PropertyVariableExpandStatement);
        rules["EvalTestFunctionCall"] = toDelegate(&EvalTestFunctionCall);
        rules["EvalArg"] = toDelegate(&EvalArg);
        rules["CacheTestFunctionCall"] = toDelegate(&CacheTestFunctionCall);
        rules["CacheTestFunctionCallParams"] = toDelegate(&CacheTestFunctionCallParams);
        rules["CacheTestFunctionParam2"] = toDelegate(&CacheTestFunctionParam2);
        rules["ContainsTestFunctionCall"] = toDelegate(&ContainsTestFunctionCall);
        rules["ReturnFunctionCall"] = toDelegate(&ReturnFunctionCall);
        rules["RequiresFunctionCall"] = toDelegate(&RequiresFunctionCall);
        rules["EnquotedString"] = toDelegate(&EnquotedString);
        rules["DoubleEnquotedString"] = toDelegate(&DoubleEnquotedString);
        rules["NonDoubleQuoteCharacter"] = toDelegate(&NonDoubleQuoteCharacter);
        rules["SingleEnquotedString"] = toDelegate(&SingleEnquotedString);
        rules["NonSingleQuoteCharacter"] = toDelegate(&NonSingleQuoteCharacter);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["QMakeIdentifier"] = toDelegate(&QMakeIdentifier);
        rules["NumberFunctionCall"] = toDelegate(&NumberFunctionCall);
        rules["LValueImpl"] = toDelegate(&LValueImpl);
        rules["LValue"] = toDelegate(&LValue);
        rules["QualifiedIdentifier"] = toDelegate(&QualifiedIdentifier);
        rules["EscapeSequence"] = toDelegate(&EscapeSequence);
        rules["OctDigit"] = toDelegate(&OctDigit);
        rules["DecDigit"] = toDelegate(&DecDigit);
        rules["HexDigit"] = toDelegate(&HexDigit);
        rules["DecNumber"] = toDelegate(&DecNumber);
        rules["EXPAND_MARKER"] = toDelegate(&EXPAND_MARKER);
        rules["COMMA"] = toDelegate(&COMMA);
        rules["COMMA_WS"] = toDelegate(&COMMA_WS);
        rules["OPEN_PAR_WS"] = toDelegate(&OPEN_PAR_WS);
        rules["CLOSE_PAR_WS"] = toDelegate(&CLOSE_PAR_WS);
        rules["BACKSLASH"] = toDelegate(&BACKSLASH);
        rules["SourceCharacter"] = toDelegate(&SourceCharacter);
        rules["LineTerminator"] = toDelegate(&LineTerminator);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
		import std.algorithm : startsWith;
        return s.startsWith("QMakeProject.");
    }
    mixin decimateTree;

    alias spacing Spacing;

    static TParseTree Project(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(Statement), eoi), "QMakeProject.Project")(p);
        }
        else
        {
            if (auto m = tuple(`Project`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(Statement), eoi), "QMakeProject.Project"), "Project")(p);
                memo[tuple(`Project`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Project(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(Statement), eoi), "QMakeProject.Project")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.zeroOrMore!(Statement), eoi), "QMakeProject.Project"), "Project")(TParseTree("", false,[], s));
        }
    }
    static string Project(GetName g)
    {
        return "QMakeProject.Project";
    }

    static TParseTree Statement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.longest_match!(FunctionDeclaration, Assignment, ForStatement, Scope, Block, ReplaceFunctionCall, TestFunctionCall, BooleanExpression, Comment, EmptyStatement), "QMakeProject.Statement")(p);
        }
        else
        {
            if (auto m = tuple(`Statement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.longest_match!(FunctionDeclaration, Assignment, ForStatement, Scope, Block, ReplaceFunctionCall, TestFunctionCall, BooleanExpression, Comment, EmptyStatement), "QMakeProject.Statement"), "Statement")(p);
                memo[tuple(`Statement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Statement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.longest_match!(FunctionDeclaration, Assignment, ForStatement, Scope, Block, ReplaceFunctionCall, TestFunctionCall, BooleanExpression, Comment, EmptyStatement), "QMakeProject.Statement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.longest_match!(FunctionDeclaration, Assignment, ForStatement, Scope, Block, ReplaceFunctionCall, TestFunctionCall, BooleanExpression, Comment, EmptyStatement), "QMakeProject.Statement"), "Statement")(TParseTree("", false,[], s));
        }
    }
    static string Statement(GetName g)
    {
        return "QMakeProject.Statement";
    }

    static TParseTree EmptyStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(eps, pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.EmptyStatement")(p);
        }
        else
        {
            if (auto m = tuple(`EmptyStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(eps, pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.EmptyStatement"), "EmptyStatement")(p);
                memo[tuple(`EmptyStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EmptyStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(eps, pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.EmptyStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(eps, pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.EmptyStatement"), "EmptyStatement")(TParseTree("", false,[], s));
        }
    }
    static string EmptyStatement(GetName g)
    {
        return "QMakeProject.EmptyStatement";
    }

    static TParseTree Comment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(MultiLineComment, SingleLineComment)), "QMakeProject.Comment")(p);
        }
        else
        {
            if (auto m = tuple(`Comment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(MultiLineComment, SingleLineComment)), "QMakeProject.Comment"), "Comment")(p);
                memo[tuple(`Comment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Comment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(MultiLineComment, SingleLineComment)), "QMakeProject.Comment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(MultiLineComment, SingleLineComment)), "QMakeProject.Comment"), "Comment")(TParseTree("", false,[], s));
        }
    }
    static string Comment(GetName g)
    {
        return "QMakeProject.Comment";
    }

    static TParseTree MultiLineComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#*")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*#")), SourceCharacter)), pegged.peg.discard!(pegged.peg.literal!("*#")))), "QMakeProject.MultiLineComment")(p);
        }
        else
        {
            if (auto m = tuple(`MultiLineComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#*")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*#")), SourceCharacter)), pegged.peg.discard!(pegged.peg.literal!("*#")))), "QMakeProject.MultiLineComment"), "MultiLineComment")(p);
                memo[tuple(`MultiLineComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MultiLineComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#*")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*#")), SourceCharacter)), pegged.peg.discard!(pegged.peg.literal!("*#")))), "QMakeProject.MultiLineComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#*")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*#")), SourceCharacter)), pegged.peg.discard!(pegged.peg.literal!("*#")))), "QMakeProject.MultiLineComment"), "MultiLineComment")(TParseTree("", false,[], s));
        }
    }
    static string MultiLineComment(GetName g)
    {
        return "QMakeProject.MultiLineComment";
    }

    static TParseTree SingleLineComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.option!(SingleLineCommentChars), pegged.peg.discard!(LineTerminator)), "QMakeProject.SingleLineComment")(p);
        }
        else
        {
            if (auto m = tuple(`SingleLineComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.option!(SingleLineCommentChars), pegged.peg.discard!(LineTerminator)), "QMakeProject.SingleLineComment"), "SingleLineComment")(p);
                memo[tuple(`SingleLineComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleLineComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.option!(SingleLineCommentChars), pegged.peg.discard!(LineTerminator)), "QMakeProject.SingleLineComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.option!(SingleLineCommentChars), pegged.peg.discard!(LineTerminator)), "QMakeProject.SingleLineComment"), "SingleLineComment")(TParseTree("", false,[], s));
        }
    }
    static string SingleLineComment(GetName g)
    {
        return "QMakeProject.SingleLineComment";
    }

    static TParseTree SingleLineCommentChars(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(SingleLineCommentChar), "QMakeProject.SingleLineCommentChars")(p);
        }
        else
        {
            if (auto m = tuple(`SingleLineCommentChars`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(SingleLineCommentChar), "QMakeProject.SingleLineCommentChars"), "SingleLineCommentChars")(p);
                memo[tuple(`SingleLineCommentChars`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleLineCommentChars(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(SingleLineCommentChar), "QMakeProject.SingleLineCommentChars")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(SingleLineCommentChar), "QMakeProject.SingleLineCommentChars"), "SingleLineCommentChars")(TParseTree("", false,[], s));
        }
    }
    static string SingleLineCommentChars(GetName g)
    {
        return "QMakeProject.SingleLineCommentChars";
    }

    static TParseTree SingleLineCommentChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(LineTerminator), SourceCharacter), "QMakeProject.SingleLineCommentChar")(p);
        }
        else
        {
            if (auto m = tuple(`SingleLineCommentChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(LineTerminator), SourceCharacter), "QMakeProject.SingleLineCommentChar"), "SingleLineCommentChar")(p);
                memo[tuple(`SingleLineCommentChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleLineCommentChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(LineTerminator), SourceCharacter), "QMakeProject.SingleLineCommentChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(LineTerminator), SourceCharacter), "QMakeProject.SingleLineCommentChar"), "SingleLineCommentChar")(TParseTree("", false,[], s));
        }
    }
    static string SingleLineCommentChar(GetName g)
    {
        return "QMakeProject.SingleLineCommentChar";
    }

    static TParseTree Block(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(SingleLineBlock, MultiLineBlock), "QMakeProject.Block")(p);
        }
        else
        {
            if (auto m = tuple(`Block`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(SingleLineBlock, MultiLineBlock), "QMakeProject.Block"), "Block")(p);
                memo[tuple(`Block`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Block(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(SingleLineBlock, MultiLineBlock), "QMakeProject.Block")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(SingleLineBlock, MultiLineBlock), "QMakeProject.Block"), "Block")(TParseTree("", false,[], s));
        }
    }
    static string Block(GetName g)
    {
        return "QMakeProject.Block";
    }

    static TParseTree SingleLineBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), "QMakeProject.SingleLineBlock")(p);
        }
        else
        {
            if (auto m = tuple(`SingleLineBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), "QMakeProject.SingleLineBlock"), "SingleLineBlock")(p);
                memo[tuple(`SingleLineBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleLineBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), "QMakeProject.SingleLineBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), "QMakeProject.SingleLineBlock"), "SingleLineBlock")(TParseTree("", false,[], s));
        }
    }
    static string SingleLineBlock(GetName g)
    {
        return "QMakeProject.SingleLineBlock";
    }

    static TParseTree MultiLineBlock(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator)), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.oneOrMore!(Statement), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator)), pegged.peg.literal!("}"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.MultiLineBlock")(p);
        }
        else
        {
            if (auto m = tuple(`MultiLineBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator)), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.oneOrMore!(Statement), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator)), pegged.peg.literal!("}"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.MultiLineBlock"), "MultiLineBlock")(p);
                memo[tuple(`MultiLineBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MultiLineBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator)), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.oneOrMore!(Statement), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator)), pegged.peg.literal!("}"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.MultiLineBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator)), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.oneOrMore!(Statement), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator)), pegged.peg.literal!("}"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.MultiLineBlock"), "MultiLineBlock")(TParseTree("", false,[], s));
        }
    }
    static string MultiLineBlock(GetName g)
    {
        return "QMakeProject.MultiLineBlock";
    }

    static TParseTree Assignment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(StandardAssignment, ReplaceAssignment), "QMakeProject.Assignment")(p);
        }
        else
        {
            if (auto m = tuple(`Assignment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(StandardAssignment, ReplaceAssignment), "QMakeProject.Assignment"), "Assignment")(p);
                memo[tuple(`Assignment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Assignment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(StandardAssignment, ReplaceAssignment), "QMakeProject.Assignment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(StandardAssignment, ReplaceAssignment), "QMakeProject.Assignment"), "Assignment")(TParseTree("", false,[], s));
        }
    }
    static string Assignment(GetName g)
    {
        return "QMakeProject.Assignment";
    }

    static TParseTree StandardAssignment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), StandardAssignmentOperator, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(RvalueExpression), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.StandardAssignment")(p);
        }
        else
        {
            if (auto m = tuple(`StandardAssignment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), StandardAssignmentOperator, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(RvalueExpression), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.StandardAssignment"), "StandardAssignment")(p);
                memo[tuple(`StandardAssignment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StandardAssignment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), StandardAssignmentOperator, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(RvalueExpression), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.StandardAssignment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), StandardAssignmentOperator, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(RvalueExpression), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.StandardAssignment"), "StandardAssignment")(TParseTree("", false,[], s));
        }
    }
    static string StandardAssignment(GetName g)
    {
        return "QMakeProject.StandardAssignment";
    }

    static TParseTree ReplaceAssignment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), ReplaceAssignmentOperator, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(RegularExpression), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.ReplaceAssignment")(p);
        }
        else
        {
            if (auto m = tuple(`ReplaceAssignment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), ReplaceAssignmentOperator, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(RegularExpression), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.ReplaceAssignment"), "ReplaceAssignment")(p);
                memo[tuple(`ReplaceAssignment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReplaceAssignment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), ReplaceAssignmentOperator, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(RegularExpression), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.ReplaceAssignment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), ReplaceAssignmentOperator, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(RegularExpression), pegged.peg.discard!(pegged.peg.zeroOrMore!(LineTerminator))), "QMakeProject.ReplaceAssignment"), "ReplaceAssignment")(TParseTree("", false,[], s));
        }
    }
    static string ReplaceAssignment(GetName g)
    {
        return "QMakeProject.ReplaceAssignment";
    }

    static TParseTree StandardAssignmentOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("+=", "*=", "-=", "="), "QMakeProject.StandardAssignmentOperator")(p);
        }
        else
        {
            if (auto m = tuple(`StandardAssignmentOperator`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("+=", "*=", "-=", "="), "QMakeProject.StandardAssignmentOperator"), "StandardAssignmentOperator")(p);
                memo[tuple(`StandardAssignmentOperator`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StandardAssignmentOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("+=", "*=", "-=", "="), "QMakeProject.StandardAssignmentOperator")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("+=", "*=", "-=", "="), "QMakeProject.StandardAssignmentOperator"), "StandardAssignmentOperator")(TParseTree("", false,[], s));
        }
    }
    static string StandardAssignmentOperator(GetName g)
    {
        return "QMakeProject.StandardAssignmentOperator";
    }

    static TParseTree ReplaceAssignmentOperator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("~="), "QMakeProject.ReplaceAssignmentOperator")(p);
        }
        else
        {
            if (auto m = tuple(`ReplaceAssignmentOperator`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("~="), "QMakeProject.ReplaceAssignmentOperator"), "ReplaceAssignmentOperator")(p);
                memo[tuple(`ReplaceAssignmentOperator`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReplaceAssignmentOperator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("~="), "QMakeProject.ReplaceAssignmentOperator")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("~="), "QMakeProject.ReplaceAssignmentOperator"), "ReplaceAssignmentOperator")(TParseTree("", false,[], s));
        }
    }
    static string ReplaceAssignmentOperator(GetName g)
    {
        return "QMakeProject.ReplaceAssignmentOperator";
    }

    static TParseTree RvalueAtom(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall), "QMakeProject.RvalueAtom")(p);
        }
        else
        {
            if (auto m = tuple(`RvalueAtom`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall), "QMakeProject.RvalueAtom"), "RvalueAtom")(p);
                memo[tuple(`RvalueAtom`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RvalueAtom(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall), "QMakeProject.RvalueAtom")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall), "QMakeProject.RvalueAtom"), "RvalueAtom")(TParseTree("", false,[], s));
        }
    }
    static string RvalueAtom(GetName g)
    {
        return "QMakeProject.RvalueAtom";
    }

    static TParseTree RvalueStopRule(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(space, quote, doublequote), "QMakeProject.RvalueStopRule")(p);
        }
        else
        {
            if (auto m = tuple(`RvalueStopRule`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(space, quote, doublequote), "QMakeProject.RvalueStopRule"), "RvalueStopRule")(p);
                memo[tuple(`RvalueStopRule`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RvalueStopRule(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(space, quote, doublequote), "QMakeProject.RvalueStopRule")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(space, quote, doublequote), "QMakeProject.RvalueStopRule"), "RvalueStopRule")(TParseTree("", false,[], s));
        }
    }
    static string RvalueStopRule(GetName g)
    {
        return "QMakeProject.RvalueStopRule";
    }

    static TParseTree RvalueExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RvalueList, RvalueChain!(RvalueStopRule)), "QMakeProject.RvalueExpression")(p);
        }
        else
        {
            if (auto m = tuple(`RvalueExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(RvalueList, RvalueChain!(RvalueStopRule)), "QMakeProject.RvalueExpression"), "RvalueExpression")(p);
                memo[tuple(`RvalueExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RvalueExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RvalueList, RvalueChain!(RvalueStopRule)), "QMakeProject.RvalueExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(RvalueList, RvalueChain!(RvalueStopRule)), "QMakeProject.RvalueExpression"), "RvalueExpression")(TParseTree("", false,[], s));
        }
    }
    static string RvalueExpression(GetName g)
    {
        return "QMakeProject.RvalueExpression";
    }

    static TParseTree RvalueList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(RvalueChain!(RvalueStopRule), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), RvalueChain!(RvalueStopRule)))), "QMakeProject.RvalueList")(p);
        }
        else
        {
            if (auto m = tuple(`RvalueList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(RvalueChain!(RvalueStopRule), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), RvalueChain!(RvalueStopRule)))), "QMakeProject.RvalueList"), "RvalueList")(p);
                memo[tuple(`RvalueList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RvalueList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(RvalueChain!(RvalueStopRule), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), RvalueChain!(RvalueStopRule)))), "QMakeProject.RvalueList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(RvalueChain!(RvalueStopRule), pegged.peg.oneOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), RvalueChain!(RvalueStopRule)))), "QMakeProject.RvalueList"), "RvalueList")(TParseTree("", false,[], s));
        }
    }
    static string RvalueList(GetName g)
    {
        return "QMakeProject.RvalueList";
    }

    template RvalueChain(alias T)
    {
    static TParseTree RvalueChain(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Rvalue!(T), pegged.peg.zeroOrMore!(Rvalue!(T))), "QMakeProject.RvalueChain!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("RvalueChain!(" ~ pegged.peg.getName!(T) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(Rvalue!(T), pegged.peg.zeroOrMore!(Rvalue!(T))), "QMakeProject.RvalueChain!(" ~ pegged.peg.getName!(T) ~ ")"), "RvalueChain_1")(p);
                memo[tuple("RvalueChain!(" ~ pegged.peg.getName!(T) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RvalueChain(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(Rvalue!(T), pegged.peg.zeroOrMore!(Rvalue!(T))), "QMakeProject.RvalueChain!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(Rvalue!(T), pegged.peg.zeroOrMore!(Rvalue!(T))), "QMakeProject.RvalueChain!(" ~ pegged.peg.getName!(T) ~ ")"), "RvalueChain_1")(TParseTree("", false,[], s));
        }
    }
    static string RvalueChain(GetName g)
    {
        return "QMakeProject.RvalueChain!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    template Rvalue(alias T)
    {
    static TParseTree Rvalue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedRvalue, Leftover!(T)), "QMakeProject.Rvalue!(" ~ pegged.peg.getName!(T) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("Rvalue!(" ~ pegged.peg.getName!(T) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedRvalue, Leftover!(T)), "QMakeProject.Rvalue!(" ~ pegged.peg.getName!(T) ~ ")"), "Rvalue_1")(p);
                memo[tuple("Rvalue!(" ~ pegged.peg.getName!(T) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Rvalue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedRvalue, Leftover!(T)), "QMakeProject.Rvalue!(" ~ pegged.peg.getName!(T) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedRvalue, Leftover!(T)), "QMakeProject.Rvalue!(" ~ pegged.peg.getName!(T) ~ ")"), "Rvalue_1")(TParseTree("", false,[], s));
        }
    }
    static string Rvalue(GetName g)
    {
        return "QMakeProject.Rvalue!(" ~ pegged.peg.getName!(T) ~ ")";
    }

    }
    static TParseTree EnquotedRvalue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedRvalue, SingleEnquotedRvalue), "QMakeProject.EnquotedRvalue")(p);
        }
        else
        {
            if (auto m = tuple(`EnquotedRvalue`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedRvalue, SingleEnquotedRvalue), "QMakeProject.EnquotedRvalue"), "EnquotedRvalue")(p);
                memo[tuple(`EnquotedRvalue`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnquotedRvalue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedRvalue, SingleEnquotedRvalue), "QMakeProject.EnquotedRvalue")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedRvalue, SingleEnquotedRvalue), "QMakeProject.EnquotedRvalue"), "EnquotedRvalue")(TParseTree("", false,[], s));
        }
    }
    static string EnquotedRvalue(GetName g)
    {
        return "QMakeProject.EnquotedRvalue";
    }

    static TParseTree DoubleEnquotedRvalue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.option!(RvalueChain!(doublequote)), doublequote), "QMakeProject.DoubleEnquotedRvalue")(p);
        }
        else
        {
            if (auto m = tuple(`DoubleEnquotedRvalue`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.option!(RvalueChain!(doublequote)), doublequote), "QMakeProject.DoubleEnquotedRvalue"), "DoubleEnquotedRvalue")(p);
                memo[tuple(`DoubleEnquotedRvalue`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DoubleEnquotedRvalue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.option!(RvalueChain!(doublequote)), doublequote), "QMakeProject.DoubleEnquotedRvalue")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.option!(RvalueChain!(doublequote)), doublequote), "QMakeProject.DoubleEnquotedRvalue"), "DoubleEnquotedRvalue")(TParseTree("", false,[], s));
        }
    }
    static string DoubleEnquotedRvalue(GetName g)
    {
        return "QMakeProject.DoubleEnquotedRvalue";
    }

    static TParseTree SingleEnquotedRvalue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.option!(RvalueChain!(quote)), quote), "QMakeProject.SingleEnquotedRvalue")(p);
        }
        else
        {
            if (auto m = tuple(`SingleEnquotedRvalue`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.option!(RvalueChain!(quote)), quote), "QMakeProject.SingleEnquotedRvalue"), "SingleEnquotedRvalue")(p);
                memo[tuple(`SingleEnquotedRvalue`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleEnquotedRvalue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.option!(RvalueChain!(quote)), quote), "QMakeProject.SingleEnquotedRvalue")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.option!(RvalueChain!(quote)), quote), "QMakeProject.SingleEnquotedRvalue"), "SingleEnquotedRvalue")(TParseTree("", false,[], s));
        }
    }
    static string SingleEnquotedRvalue(GetName g)
    {
        return "QMakeProject.SingleEnquotedRvalue";
    }

    template Leftover(alias StopPattern)
    {
    static TParseTree Leftover(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(LeftoverChar!(StopPattern))), "QMakeProject.Leftover!(" ~ pegged.peg.getName!(StopPattern) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("Leftover!(" ~ pegged.peg.getName!(StopPattern) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(LeftoverChar!(StopPattern))), "QMakeProject.Leftover!(" ~ pegged.peg.getName!(StopPattern) ~ ")"), "Leftover_1")(p);
                memo[tuple("Leftover!(" ~ pegged.peg.getName!(StopPattern) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Leftover(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(LeftoverChar!(StopPattern))), "QMakeProject.Leftover!(" ~ pegged.peg.getName!(StopPattern) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(LeftoverChar!(StopPattern))), "QMakeProject.Leftover!(" ~ pegged.peg.getName!(StopPattern) ~ ")"), "Leftover_1")(TParseTree("", false,[], s));
        }
    }
    static string Leftover(GetName g)
    {
        return "QMakeProject.Leftover!(" ~ pegged.peg.getName!(StopPattern) ~ ")";
    }

    }
    template LeftoverStopChar(alias StopPattern)
    {
    static TParseTree LeftoverStopChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(LineTerminator, ExpandStatement, BACKSLASH, StopPattern), "QMakeProject.LeftoverStopChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("LeftoverStopChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(LineTerminator, ExpandStatement, BACKSLASH, StopPattern), "QMakeProject.LeftoverStopChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")"), "LeftoverStopChar_1")(p);
                memo[tuple("LeftoverStopChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LeftoverStopChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(LineTerminator, ExpandStatement, BACKSLASH, StopPattern), "QMakeProject.LeftoverStopChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(LineTerminator, ExpandStatement, BACKSLASH, StopPattern), "QMakeProject.LeftoverStopChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")"), "LeftoverStopChar_1")(TParseTree("", false,[], s));
        }
    }
    static string LeftoverStopChar(GetName g)
    {
        return "QMakeProject.LeftoverStopChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")";
    }

    }
    template LeftoverChar(alias StopPattern)
    {
    static TParseTree LeftoverChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(LeftoverStopChar!(StopPattern)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.LeftoverChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("LeftoverChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(LeftoverStopChar!(StopPattern)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.LeftoverChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")"), "LeftoverChar_1")(p);
                memo[tuple("LeftoverChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LeftoverChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(LeftoverStopChar!(StopPattern)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.LeftoverChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(LeftoverStopChar!(StopPattern)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.LeftoverChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")"), "LeftoverChar_1")(TParseTree("", false,[], s));
        }
    }
    static string LeftoverChar(GetName g)
    {
        return "QMakeProject.LeftoverChar!(" ~ pegged.peg.getName!(StopPattern) ~ ")";
    }

    }
    static TParseTree RegularExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(RegularExpressionChar)), "QMakeProject.RegularExpression")(p);
        }
        else
        {
            if (auto m = tuple(`RegularExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(RegularExpressionChar)), "QMakeProject.RegularExpression"), "RegularExpression")(p);
                memo[tuple(`RegularExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RegularExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(RegularExpressionChar)), "QMakeProject.RegularExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(RegularExpressionChar)), "QMakeProject.RegularExpression"), "RegularExpression")(TParseTree("", false,[], s));
        }
    }
    static string RegularExpression(GetName g)
    {
        return "QMakeProject.RegularExpression";
    }

    static TParseTree RegularExpressionStopChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(LineTerminator, "QMakeProject.RegularExpressionStopChar")(p);
        }
        else
        {
            if (auto m = tuple(`RegularExpressionStopChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(LineTerminator, "QMakeProject.RegularExpressionStopChar"), "RegularExpressionStopChar")(p);
                memo[tuple(`RegularExpressionStopChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RegularExpressionStopChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(LineTerminator, "QMakeProject.RegularExpressionStopChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(LineTerminator, "QMakeProject.RegularExpressionStopChar"), "RegularExpressionStopChar")(TParseTree("", false,[], s));
        }
    }
    static string RegularExpressionStopChar(GetName g)
    {
        return "QMakeProject.RegularExpressionStopChar";
    }

    static TParseTree RegularExpressionChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(RegularExpressionStopChar), SourceCharacter), "QMakeProject.RegularExpressionChar")(p);
        }
        else
        {
            if (auto m = tuple(`RegularExpressionChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(RegularExpressionStopChar), SourceCharacter), "QMakeProject.RegularExpressionChar"), "RegularExpressionChar")(p);
                memo[tuple(`RegularExpressionChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RegularExpressionChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(RegularExpressionStopChar), SourceCharacter), "QMakeProject.RegularExpressionChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(RegularExpressionStopChar), SourceCharacter), "QMakeProject.RegularExpressionChar"), "RegularExpressionChar")(TParseTree("", false,[], s));
        }
    }
    static string RegularExpressionChar(GetName g)
    {
        return "QMakeProject.RegularExpressionChar";
    }

    static TParseTree TestFunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EvalTestFunctionCall, CacheTestFunctionCall, ContainsTestFunctionCall, ReturnFunctionCall, RequiresFunctionCall, FunctionCall), "QMakeProject.TestFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`TestFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(EvalTestFunctionCall, CacheTestFunctionCall, ContainsTestFunctionCall, ReturnFunctionCall, RequiresFunctionCall, FunctionCall), "QMakeProject.TestFunctionCall"), "TestFunctionCall")(p);
                memo[tuple(`TestFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TestFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(EvalTestFunctionCall, CacheTestFunctionCall, ContainsTestFunctionCall, ReturnFunctionCall, RequiresFunctionCall, FunctionCall), "QMakeProject.TestFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(EvalTestFunctionCall, CacheTestFunctionCall, ContainsTestFunctionCall, ReturnFunctionCall, RequiresFunctionCall, FunctionCall), "QMakeProject.TestFunctionCall"), "TestFunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string TestFunctionCall(GetName g)
    {
        return "QMakeProject.TestFunctionCall";
    }

    static TParseTree ReplaceFunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, TestFunctionCall), "QMakeProject.ReplaceFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`ReplaceFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, TestFunctionCall), "QMakeProject.ReplaceFunctionCall"), "ReplaceFunctionCall")(p);
                memo[tuple(`ReplaceFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReplaceFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, TestFunctionCall), "QMakeProject.ReplaceFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, TestFunctionCall), "QMakeProject.ReplaceFunctionCall"), "ReplaceFunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string ReplaceFunctionCall(GetName g)
    {
        return "QMakeProject.ReplaceFunctionCall";
    }

    static TParseTree FunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionId, OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS), "QMakeProject.FunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionId, OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS), "QMakeProject.FunctionCall"), "FunctionCall")(p);
                memo[tuple(`FunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionId, OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS), "QMakeProject.FunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionId, OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS), "QMakeProject.FunctionCall"), "FunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string FunctionCall(GetName g)
    {
        return "QMakeProject.FunctionCall";
    }

    static TParseTree FunctionId(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("defineReplace", "defineTest", "eval", "cache", "contains", "return", "requires")), pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("}")), QualifiedIdentifier, EnquotedString)), "QMakeProject.FunctionId")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionId`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("defineReplace", "defineTest", "eval", "cache", "contains", "return", "requires")), pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("}")), QualifiedIdentifier, EnquotedString)), "QMakeProject.FunctionId"), "FunctionId")(p);
                memo[tuple(`FunctionId`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionId(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("defineReplace", "defineTest", "eval", "cache", "contains", "return", "requires")), pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("}")), QualifiedIdentifier, EnquotedString)), "QMakeProject.FunctionId")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.keywords!("defineReplace", "defineTest", "eval", "cache", "contains", "return", "requires")), pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("}")), QualifiedIdentifier, EnquotedString)), "QMakeProject.FunctionId"), "FunctionId")(TParseTree("", false,[], s));
        }
    }
    static string FunctionId(GetName g)
    {
        return "QMakeProject.FunctionId";
    }

    static TParseTree FunctionArgumentList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(List!(COMMA_WS, COMMA), List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA))), "QMakeProject.FunctionArgumentList")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionArgumentList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(List!(COMMA_WS, COMMA), List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA))), "QMakeProject.FunctionArgumentList"), "FunctionArgumentList")(p);
                memo[tuple(`FunctionArgumentList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionArgumentList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(List!(COMMA_WS, COMMA), List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA))), "QMakeProject.FunctionArgumentList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(List!(COMMA_WS, COMMA), List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA))), "QMakeProject.FunctionArgumentList"), "FunctionArgumentList")(TParseTree("", false,[], s));
        }
    }
    static string FunctionArgumentList(GetName g)
    {
        return "QMakeProject.FunctionArgumentList";
    }

    template List(alias delimRule, alias delimChar)
    {
    static TParseTree List(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionArgument!(pegged.peg.or!(space, COMMA)), pegged.peg.oneOrMore!(pegged.peg.and!(delimRule, pegged.peg.option!(FunctionArgument!(delimChar))))), "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionArgument!(pegged.peg.or!(space, COMMA)), pegged.peg.oneOrMore!(pegged.peg.and!(delimRule, pegged.peg.option!(FunctionArgument!(delimChar))))), "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")"), "List_2")(p);
                memo[tuple("List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree List(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionArgument!(pegged.peg.or!(space, COMMA)), pegged.peg.oneOrMore!(pegged.peg.and!(delimRule, pegged.peg.option!(FunctionArgument!(delimChar))))), "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionArgument!(pegged.peg.or!(space, COMMA)), pegged.peg.oneOrMore!(pegged.peg.and!(delimRule, pegged.peg.option!(FunctionArgument!(delimChar))))), "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")"), "List_2")(TParseTree("", false,[], s));
        }
    }
    static string List(GetName g)
    {
        return "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")";
    }

    }
    template FunctionArgument(alias delim)
    {
    static TParseTree FunctionArgument(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionArgumentImpl!(delim), pegged.peg.zeroOrMore!(FunctionArgumentImpl!(delim))), "QMakeProject.FunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionArgumentImpl!(delim), pegged.peg.zeroOrMore!(FunctionArgumentImpl!(delim))), "QMakeProject.FunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgument_1")(p);
                memo[tuple("FunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionArgument(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionArgumentImpl!(delim), pegged.peg.zeroOrMore!(FunctionArgumentImpl!(delim))), "QMakeProject.FunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionArgumentImpl!(delim), pegged.peg.zeroOrMore!(FunctionArgumentImpl!(delim))), "QMakeProject.FunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgument_1")(TParseTree("", false,[], s));
        }
    }
    static string FunctionArgument(GetName g)
    {
        return "QMakeProject.FunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template FunctionArgumentImpl(alias delim)
    {
    static TParseTree FunctionArgumentImpl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedFunctionArgument!(delim), FunctionArgumentString!(delim)), "QMakeProject.FunctionArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedFunctionArgument!(delim), FunctionArgumentString!(delim)), "QMakeProject.FunctionArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgumentImpl_1")(p);
                memo[tuple("FunctionArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionArgumentImpl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedFunctionArgument!(delim), FunctionArgumentString!(delim)), "QMakeProject.FunctionArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedFunctionArgument!(delim), FunctionArgumentString!(delim)), "QMakeProject.FunctionArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgumentImpl_1")(TParseTree("", false,[], s));
        }
    }
    static string FunctionArgumentImpl(GetName g)
    {
        return "QMakeProject.FunctionArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template EnquotedFunctionArgument(alias delim)
    {
    static TParseTree EnquotedFunctionArgument(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedFunctionArgument!(delim), SingleEnquotedFunctionArgument!(delim)), "QMakeProject.EnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("EnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedFunctionArgument!(delim), SingleEnquotedFunctionArgument!(delim)), "QMakeProject.EnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "EnquotedFunctionArgument_1")(p);
                memo[tuple("EnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnquotedFunctionArgument(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedFunctionArgument!(delim), SingleEnquotedFunctionArgument!(delim)), "QMakeProject.EnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedFunctionArgument!(delim), SingleEnquotedFunctionArgument!(delim)), "QMakeProject.EnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "EnquotedFunctionArgument_1")(TParseTree("", false,[], s));
        }
    }
    static string EnquotedFunctionArgument(GetName g)
    {
        return "QMakeProject.EnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template DoubleEnquotedFunctionArgument(alias delim)
    {
    static TParseTree DoubleEnquotedFunctionArgument(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.option!(EnquotedFunctionArgumentChain!(delim, doublequote)), doublequote), "QMakeProject.DoubleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("DoubleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.option!(EnquotedFunctionArgumentChain!(delim, doublequote)), doublequote), "QMakeProject.DoubleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "DoubleEnquotedFunctionArgument_1")(p);
                memo[tuple("DoubleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DoubleEnquotedFunctionArgument(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.option!(EnquotedFunctionArgumentChain!(delim, doublequote)), doublequote), "QMakeProject.DoubleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.option!(EnquotedFunctionArgumentChain!(delim, doublequote)), doublequote), "QMakeProject.DoubleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "DoubleEnquotedFunctionArgument_1")(TParseTree("", false,[], s));
        }
    }
    static string DoubleEnquotedFunctionArgument(GetName g)
    {
        return "QMakeProject.DoubleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template SingleEnquotedFunctionArgument(alias delim)
    {
    static TParseTree SingleEnquotedFunctionArgument(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.option!(EnquotedFunctionArgumentChain!(delim, quote)), quote), "QMakeProject.SingleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("SingleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.option!(EnquotedFunctionArgumentChain!(delim, quote)), quote), "QMakeProject.SingleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "SingleEnquotedFunctionArgument_1")(p);
                memo[tuple("SingleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleEnquotedFunctionArgument(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.option!(EnquotedFunctionArgumentChain!(delim, quote)), quote), "QMakeProject.SingleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.option!(EnquotedFunctionArgumentChain!(delim, quote)), quote), "QMakeProject.SingleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "SingleEnquotedFunctionArgument_1")(TParseTree("", false,[], s));
        }
    }
    static string SingleEnquotedFunctionArgument(GetName g)
    {
        return "QMakeProject.SingleEnquotedFunctionArgument!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template EnquotedFunctionArgumentChain(alias delim, alias Q)
    {
    static TParseTree EnquotedFunctionArgumentChain(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionArgumentImpl_2!(delim, Q), pegged.peg.zeroOrMore!(FunctionArgumentImpl_2!(delim, Q))), "QMakeProject.EnquotedFunctionArgumentChain!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("EnquotedFunctionArgumentChain!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionArgumentImpl_2!(delim, Q), pegged.peg.zeroOrMore!(FunctionArgumentImpl_2!(delim, Q))), "QMakeProject.EnquotedFunctionArgumentChain!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")"), "EnquotedFunctionArgumentChain_2")(p);
                memo[tuple("EnquotedFunctionArgumentChain!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnquotedFunctionArgumentChain(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionArgumentImpl_2!(delim, Q), pegged.peg.zeroOrMore!(FunctionArgumentImpl_2!(delim, Q))), "QMakeProject.EnquotedFunctionArgumentChain!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionArgumentImpl_2!(delim, Q), pegged.peg.zeroOrMore!(FunctionArgumentImpl_2!(delim, Q))), "QMakeProject.EnquotedFunctionArgumentChain!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")"), "EnquotedFunctionArgumentChain_2")(TParseTree("", false,[], s));
        }
    }
    static string EnquotedFunctionArgumentChain(GetName g)
    {
        return "QMakeProject.EnquotedFunctionArgumentChain!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")";
    }

    }
    template FunctionArgumentImpl_2(alias delim, alias Q)
    {
    static TParseTree FunctionArgumentImpl_2(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedFunctionArgument!(delim), FunctionArgumentString!(delim), Leftover!(Q)), "QMakeProject.FunctionArgumentImpl_2!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionArgumentImpl_2!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedFunctionArgument!(delim), FunctionArgumentString!(delim), Leftover!(Q)), "QMakeProject.FunctionArgumentImpl_2!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")"), "FunctionArgumentImpl_2_2")(p);
                memo[tuple("FunctionArgumentImpl_2!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionArgumentImpl_2(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedFunctionArgument!(delim), FunctionArgumentString!(delim), Leftover!(Q)), "QMakeProject.FunctionArgumentImpl_2!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(RvalueAtom, EnquotedFunctionArgument!(delim), FunctionArgumentString!(delim), Leftover!(Q)), "QMakeProject.FunctionArgumentImpl_2!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")"), "FunctionArgumentImpl_2_2")(TParseTree("", false,[], s));
        }
    }
    static string FunctionArgumentImpl_2(GetName g)
    {
        return "QMakeProject.FunctionArgumentImpl_2!(" ~ pegged.peg.getName!(delim)() ~ ", " ~ pegged.peg.getName!(Q) ~ ")";
    }

    }
    template FunctionArgumentString(alias delim)
    {
    static TParseTree FunctionArgumentString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionArgumentStringChar!(delim))), "QMakeProject.FunctionArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionArgumentStringChar!(delim))), "QMakeProject.FunctionArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgumentString_1")(p);
                memo[tuple("FunctionArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionArgumentString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionArgumentStringChar!(delim))), "QMakeProject.FunctionArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionArgumentStringChar!(delim))), "QMakeProject.FunctionArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgumentString_1")(TParseTree("", false,[], s));
        }
    }
    static string FunctionArgumentString(GetName g)
    {
        return "QMakeProject.FunctionArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template FunctionArgumentStringStopChar(alias delim)
    {
    static TParseTree FunctionArgumentStringStopChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(delim, LineTerminator, ExpandStatement, quote, doublequote, BACKSLASH, EndOfFunction), "QMakeProject.FunctionArgumentStringStopChar!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionArgumentStringStopChar!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(delim, LineTerminator, ExpandStatement, quote, doublequote, BACKSLASH, EndOfFunction), "QMakeProject.FunctionArgumentStringStopChar!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgumentStringStopChar_1")(p);
                memo[tuple("FunctionArgumentStringStopChar!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionArgumentStringStopChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(delim, LineTerminator, ExpandStatement, quote, doublequote, BACKSLASH, EndOfFunction), "QMakeProject.FunctionArgumentStringStopChar!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(delim, LineTerminator, ExpandStatement, quote, doublequote, BACKSLASH, EndOfFunction), "QMakeProject.FunctionArgumentStringStopChar!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgumentStringStopChar_1")(TParseTree("", false,[], s));
        }
    }
    static string FunctionArgumentStringStopChar(GetName g)
    {
        return "QMakeProject.FunctionArgumentStringStopChar!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template FunctionArgumentStringChar(alias delim)
    {
    static TParseTree FunctionArgumentStringChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(FunctionArgumentStringStopChar!(delim)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(FunctionArgumentStringStopChar!(delim)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgumentStringChar_1")(p);
                memo[tuple("FunctionArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionArgumentStringChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(FunctionArgumentStringStopChar!(delim)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(FunctionArgumentStringStopChar!(delim)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionArgumentStringChar_1")(TParseTree("", false,[], s));
        }
    }
    static string FunctionArgumentStringChar(GetName g)
    {
        return "QMakeProject.FunctionArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    static TParseTree EndOfFunction(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"), pegged.peg.literal!("/")), eoi, LineTerminator, pegged.peg.literal!("="), pegged.peg.literal!("+="), pegged.peg.literal!("*="), pegged.peg.literal!("-="), pegged.peg.literal!("~="), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("_"), pegged.peg.literal!("("), pegged.peg.literal!(")"), EXPAND_MARKER, pegged.peg.literal!("@"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!(":"), pegged.peg.literal!("|"), pegged.peg.literal!("\""))), "QMakeProject.EndOfFunction")(p);
        }
        else
        {
            if (auto m = tuple(`EndOfFunction`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"), pegged.peg.literal!("/")), eoi, LineTerminator, pegged.peg.literal!("="), pegged.peg.literal!("+="), pegged.peg.literal!("*="), pegged.peg.literal!("-="), pegged.peg.literal!("~="), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("_"), pegged.peg.literal!("("), pegged.peg.literal!(")"), EXPAND_MARKER, pegged.peg.literal!("@"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!(":"), pegged.peg.literal!("|"), pegged.peg.literal!("\""))), "QMakeProject.EndOfFunction"), "EndOfFunction")(p);
                memo[tuple(`EndOfFunction`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EndOfFunction(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"), pegged.peg.literal!("/")), eoi, LineTerminator, pegged.peg.literal!("="), pegged.peg.literal!("+="), pegged.peg.literal!("*="), pegged.peg.literal!("-="), pegged.peg.literal!("~="), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("_"), pegged.peg.literal!("("), pegged.peg.literal!(")"), EXPAND_MARKER, pegged.peg.literal!("@"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!(":"), pegged.peg.literal!("|"), pegged.peg.literal!("\""))), "QMakeProject.EndOfFunction")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"), pegged.peg.literal!("/")), eoi, LineTerminator, pegged.peg.literal!("="), pegged.peg.literal!("+="), pegged.peg.literal!("*="), pegged.peg.literal!("-="), pegged.peg.literal!("~="), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("_"), pegged.peg.literal!("("), pegged.peg.literal!(")"), EXPAND_MARKER, pegged.peg.literal!("@"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!(":"), pegged.peg.literal!("|"), pegged.peg.literal!("\""))), "QMakeProject.EndOfFunction"), "EndOfFunction")(TParseTree("", false,[], s));
        }
    }
    static string EndOfFunction(GetName g)
    {
        return "QMakeProject.EndOfFunction";
    }

    static TParseTree FunctionDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionDeclaration, TestFunctionDeclaration), "QMakeProject.FunctionDeclaration")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionDeclaration`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionDeclaration, TestFunctionDeclaration), "QMakeProject.FunctionDeclaration"), "FunctionDeclaration")(p);
                memo[tuple(`FunctionDeclaration`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionDeclaration, TestFunctionDeclaration), "QMakeProject.FunctionDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionDeclaration, TestFunctionDeclaration), "QMakeProject.FunctionDeclaration"), "FunctionDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string FunctionDeclaration(GetName g)
    {
        return "QMakeProject.FunctionDeclaration";
    }

    static TParseTree ReplaceFunctionDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineReplace"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, Block), "QMakeProject.ReplaceFunctionDeclaration")(p);
        }
        else
        {
            if (auto m = tuple(`ReplaceFunctionDeclaration`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineReplace"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, Block), "QMakeProject.ReplaceFunctionDeclaration"), "ReplaceFunctionDeclaration")(p);
                memo[tuple(`ReplaceFunctionDeclaration`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReplaceFunctionDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineReplace"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, Block), "QMakeProject.ReplaceFunctionDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineReplace"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, Block), "QMakeProject.ReplaceFunctionDeclaration"), "ReplaceFunctionDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string ReplaceFunctionDeclaration(GetName g)
    {
        return "QMakeProject.ReplaceFunctionDeclaration";
    }

    static TParseTree TestFunctionDeclaration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineTest"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, Block), "QMakeProject.TestFunctionDeclaration")(p);
        }
        else
        {
            if (auto m = tuple(`TestFunctionDeclaration`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineTest"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, Block), "QMakeProject.TestFunctionDeclaration"), "TestFunctionDeclaration")(p);
                memo[tuple(`TestFunctionDeclaration`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TestFunctionDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineTest"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, Block), "QMakeProject.TestFunctionDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineTest"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, Block), "QMakeProject.TestFunctionDeclaration"), "TestFunctionDeclaration")(TParseTree("", false,[], s));
        }
    }
    static string TestFunctionDeclaration(GetName g)
    {
        return "QMakeProject.TestFunctionDeclaration";
    }

    static TParseTree Scope(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(BooleanExpression, ScopeMainBranch, pegged.peg.zeroOrMore!(ScopeElseIfBranch), pegged.peg.option!(ScopeElseBranch)), "QMakeProject.Scope")(p);
        }
        else
        {
            if (auto m = tuple(`Scope`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(BooleanExpression, ScopeMainBranch, pegged.peg.zeroOrMore!(ScopeElseIfBranch), pegged.peg.option!(ScopeElseBranch)), "QMakeProject.Scope"), "Scope")(p);
                memo[tuple(`Scope`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Scope(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(BooleanExpression, ScopeMainBranch, pegged.peg.zeroOrMore!(ScopeElseIfBranch), pegged.peg.option!(ScopeElseBranch)), "QMakeProject.Scope")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(BooleanExpression, ScopeMainBranch, pegged.peg.zeroOrMore!(ScopeElseIfBranch), pegged.peg.option!(ScopeElseBranch)), "QMakeProject.Scope"), "Scope")(TParseTree("", false,[], s));
        }
    }
    static string Scope(GetName g)
    {
        return "QMakeProject.Scope";
    }

    static TParseTree ScopeMainBranch(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(Block, "QMakeProject.ScopeMainBranch")(p);
        }
        else
        {
            if (auto m = tuple(`ScopeMainBranch`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(Block, "QMakeProject.ScopeMainBranch"), "ScopeMainBranch")(p);
                memo[tuple(`ScopeMainBranch`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ScopeMainBranch(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(Block, "QMakeProject.ScopeMainBranch")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(Block, "QMakeProject.ScopeMainBranch"), "ScopeMainBranch")(TParseTree("", false,[], s));
        }
    }
    static string ScopeMainBranch(GetName g)
    {
        return "QMakeProject.ScopeMainBranch";
    }

    static TParseTree ScopeElseIfBranch(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("else@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), BooleanExpression, Block), "QMakeProject.ScopeElseIfBranch")(p);
        }
        else
        {
            if (auto m = tuple(`ScopeElseIfBranch`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("else@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), BooleanExpression, Block), "QMakeProject.ScopeElseIfBranch"), "ScopeElseIfBranch")(p);
                memo[tuple(`ScopeElseIfBranch`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ScopeElseIfBranch(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("else@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), BooleanExpression, Block), "QMakeProject.ScopeElseIfBranch")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("else@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), BooleanExpression, Block), "QMakeProject.ScopeElseIfBranch"), "ScopeElseIfBranch")(TParseTree("", false,[], s));
        }
    }
    static string ScopeElseIfBranch(GetName g)
    {
        return "QMakeProject.ScopeElseIfBranch";
    }

    static TParseTree ScopeElseBranch(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("else@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), pegged.peg.and!(pegged.peg.literal!("else"), MultiLineBlock)), "QMakeProject.ScopeElseBranch")(p);
        }
        else
        {
            if (auto m = tuple(`ScopeElseBranch`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("else@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), pegged.peg.and!(pegged.peg.literal!("else"), MultiLineBlock)), "QMakeProject.ScopeElseBranch"), "ScopeElseBranch")(p);
                memo[tuple(`ScopeElseBranch`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ScopeElseBranch(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("else@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), pegged.peg.and!(pegged.peg.literal!("else"), MultiLineBlock)), "QMakeProject.ScopeElseBranch")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("else@"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), pegged.peg.and!(pegged.peg.literal!("else"), MultiLineBlock)), "QMakeProject.ScopeElseBranch"), "ScopeElseBranch")(TParseTree("", false,[], s));
        }
    }
    static string ScopeElseBranch(GetName g)
    {
        return "QMakeProject.ScopeElseBranch";
    }

    static TParseTree BooleanExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(LogicalORExpression, "QMakeProject.BooleanExpression")(p);
        }
        else
        {
            if (auto m = tuple(`BooleanExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(LogicalORExpression, "QMakeProject.BooleanExpression"), "BooleanExpression")(p);
                memo[tuple(`BooleanExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BooleanExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(LogicalORExpression, "QMakeProject.BooleanExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(LogicalORExpression, "QMakeProject.BooleanExpression"), "BooleanExpression")(TParseTree("", false,[], s));
        }
    }
    static string BooleanExpression(GetName g)
    {
        return "QMakeProject.BooleanExpression";
    }

    static TParseTree LogicalORExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(LogicalANDExpression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("|"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), LogicalANDExpression))), "QMakeProject.LogicalORExpression")(p);
        }
        else
        {
            if (auto m = tuple(`LogicalORExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(LogicalANDExpression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("|"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), LogicalANDExpression))), "QMakeProject.LogicalORExpression"), "LogicalORExpression")(p);
                memo[tuple(`LogicalORExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LogicalORExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(LogicalANDExpression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("|"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), LogicalANDExpression))), "QMakeProject.LogicalORExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(LogicalANDExpression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("|"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), LogicalANDExpression))), "QMakeProject.LogicalORExpression"), "LogicalORExpression")(TParseTree("", false,[], s));
        }
    }
    static string LogicalORExpression(GetName g)
    {
        return "QMakeProject.LogicalORExpression";
    }

    static TParseTree LogicalANDExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(LogicalNOTExpression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(":"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), LogicalNOTExpression))), "QMakeProject.LogicalANDExpression")(p);
        }
        else
        {
            if (auto m = tuple(`LogicalANDExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(LogicalNOTExpression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(":"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), LogicalNOTExpression))), "QMakeProject.LogicalANDExpression"), "LogicalANDExpression")(p);
                memo[tuple(`LogicalANDExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LogicalANDExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(LogicalNOTExpression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(":"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), LogicalNOTExpression))), "QMakeProject.LogicalANDExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(LogicalNOTExpression, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(":"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), LogicalNOTExpression))), "QMakeProject.LogicalANDExpression"), "LogicalANDExpression")(TParseTree("", false,[], s));
        }
    }
    static string LogicalANDExpression(GetName g)
    {
        return "QMakeProject.LogicalANDExpression";
    }

    static TParseTree LogicalNOTExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("!"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)))), PrimaryBooleanExpression), "QMakeProject.LogicalNOTExpression")(p);
        }
        else
        {
            if (auto m = tuple(`LogicalNOTExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("!"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)))), PrimaryBooleanExpression), "QMakeProject.LogicalNOTExpression"), "LogicalNOTExpression")(p);
                memo[tuple(`LogicalNOTExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LogicalNOTExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("!"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)))), PrimaryBooleanExpression), "QMakeProject.LogicalNOTExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("!"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)))), PrimaryBooleanExpression), "QMakeProject.LogicalNOTExpression"), "LogicalNOTExpression")(TParseTree("", false,[], s));
        }
    }
    static string LogicalNOTExpression(GetName g)
    {
        return "QMakeProject.LogicalNOTExpression";
    }

    static TParseTree PrimaryBooleanExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ParenthesedBooleanExpression, IfTestFunctionCall, BooleanAtom), "QMakeProject.PrimaryBooleanExpression")(p);
        }
        else
        {
            if (auto m = tuple(`PrimaryBooleanExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ParenthesedBooleanExpression, IfTestFunctionCall, BooleanAtom), "QMakeProject.PrimaryBooleanExpression"), "PrimaryBooleanExpression")(p);
                memo[tuple(`PrimaryBooleanExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PrimaryBooleanExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ParenthesedBooleanExpression, IfTestFunctionCall, BooleanAtom), "QMakeProject.PrimaryBooleanExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ParenthesedBooleanExpression, IfTestFunctionCall, BooleanAtom), "QMakeProject.PrimaryBooleanExpression"), "PrimaryBooleanExpression")(TParseTree("", false,[], s));
        }
    }
    static string PrimaryBooleanExpression(GetName g)
    {
        return "QMakeProject.PrimaryBooleanExpression";
    }

    static TParseTree ParenthesedBooleanExpression(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), BooleanExpression, pegged.peg.literal!(")")), "QMakeProject.ParenthesedBooleanExpression")(p);
        }
        else
        {
            if (auto m = tuple(`ParenthesedBooleanExpression`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), BooleanExpression, pegged.peg.literal!(")")), "QMakeProject.ParenthesedBooleanExpression"), "ParenthesedBooleanExpression")(p);
                memo[tuple(`ParenthesedBooleanExpression`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ParenthesedBooleanExpression(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), BooleanExpression, pegged.peg.literal!(")")), "QMakeProject.ParenthesedBooleanExpression")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("("), BooleanExpression, pegged.peg.literal!(")")), "QMakeProject.ParenthesedBooleanExpression"), "ParenthesedBooleanExpression")(TParseTree("", false,[], s));
        }
    }
    static string ParenthesedBooleanExpression(GetName g)
    {
        return "QMakeProject.ParenthesedBooleanExpression";
    }

    static TParseTree IfTestFunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("if"), OPEN_PAR_WS, BooleanExpression, CLOSE_PAR_WS), "QMakeProject.IfTestFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`IfTestFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("if"), OPEN_PAR_WS, BooleanExpression, CLOSE_PAR_WS), "QMakeProject.IfTestFunctionCall"), "IfTestFunctionCall")(p);
                memo[tuple(`IfTestFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree IfTestFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("if"), OPEN_PAR_WS, BooleanExpression, CLOSE_PAR_WS), "QMakeProject.IfTestFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("if"), OPEN_PAR_WS, BooleanExpression, CLOSE_PAR_WS), "QMakeProject.IfTestFunctionCall"), "IfTestFunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string IfTestFunctionCall(GetName g)
    {
        return "QMakeProject.IfTestFunctionCall";
    }

    static TParseTree BooleanAtom(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, TestFunctionCall, QualifiedIdentifier, BooleanConst), "QMakeProject.BooleanAtom")(p);
        }
        else
        {
            if (auto m = tuple(`BooleanAtom`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, TestFunctionCall, QualifiedIdentifier, BooleanConst), "QMakeProject.BooleanAtom"), "BooleanAtom")(p);
                memo[tuple(`BooleanAtom`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BooleanAtom(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, TestFunctionCall, QualifiedIdentifier, BooleanConst), "QMakeProject.BooleanAtom")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, TestFunctionCall, QualifiedIdentifier, BooleanConst), "QMakeProject.BooleanAtom"), "BooleanAtom")(TParseTree("", false,[], s));
        }
    }
    static string BooleanAtom(GetName g)
    {
        return "QMakeProject.BooleanAtom";
    }

    static TParseTree BooleanConst(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("true", "false"), "QMakeProject.BooleanConst")(p);
        }
        else
        {
            if (auto m = tuple(`BooleanConst`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("true", "false"), "QMakeProject.BooleanConst"), "BooleanConst")(p);
                memo[tuple(`BooleanConst`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BooleanConst(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("true", "false"), "QMakeProject.BooleanConst")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("true", "false"), "QMakeProject.BooleanConst"), "BooleanConst")(TParseTree("", false,[], s));
        }
    }
    static string BooleanConst(GetName g)
    {
        return "QMakeProject.BooleanConst";
    }

    static TParseTree ForStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ForEachInListStatement, ForEverStatement), "QMakeProject.ForStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ForStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ForEachInListStatement, ForEverStatement), "QMakeProject.ForStatement"), "ForStatement")(p);
                memo[tuple(`ForStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ForEachInListStatement, ForEverStatement), "QMakeProject.ForStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ForEachInListStatement, ForEverStatement), "QMakeProject.ForStatement"), "ForStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForStatement(GetName g)
    {
        return "QMakeProject.ForStatement";
    }

    static TParseTree ForEachInListStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("for"), OPEN_PAR_WS, ForIteratorVariableName, COMMA_WS, ForIterableList, CLOSE_PAR_WS, Block), "QMakeProject.ForEachInListStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ForEachInListStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("for"), OPEN_PAR_WS, ForIteratorVariableName, COMMA_WS, ForIterableList, CLOSE_PAR_WS, Block), "QMakeProject.ForEachInListStatement"), "ForEachInListStatement")(p);
                memo[tuple(`ForEachInListStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForEachInListStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("for"), OPEN_PAR_WS, ForIteratorVariableName, COMMA_WS, ForIterableList, CLOSE_PAR_WS, Block), "QMakeProject.ForEachInListStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("for"), OPEN_PAR_WS, ForIteratorVariableName, COMMA_WS, ForIterableList, CLOSE_PAR_WS, Block), "QMakeProject.ForEachInListStatement"), "ForEachInListStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForEachInListStatement(GetName g)
    {
        return "QMakeProject.ForEachInListStatement";
    }

    static TParseTree ForIteratorVariableName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(QualifiedIdentifier, "QMakeProject.ForIteratorVariableName")(p);
        }
        else
        {
            if (auto m = tuple(`ForIteratorVariableName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(QualifiedIdentifier, "QMakeProject.ForIteratorVariableName"), "ForIteratorVariableName")(p);
                memo[tuple(`ForIteratorVariableName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForIteratorVariableName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(QualifiedIdentifier, "QMakeProject.ForIteratorVariableName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(QualifiedIdentifier, "QMakeProject.ForIteratorVariableName"), "ForIteratorVariableName")(TParseTree("", false,[], s));
        }
    }
    static string ForIteratorVariableName(GetName g)
    {
        return "QMakeProject.ForIteratorVariableName";
    }

    static TParseTree ForIterableList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA))), "QMakeProject.ForIterableList")(p);
        }
        else
        {
            if (auto m = tuple(`ForIterableList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA))), "QMakeProject.ForIterableList"), "ForIterableList")(p);
                memo[tuple(`ForIterableList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForIterableList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA))), "QMakeProject.ForIterableList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA))), "QMakeProject.ForIterableList"), "ForIterableList")(TParseTree("", false,[], s));
        }
    }
    static string ForIterableList(GetName g)
    {
        return "QMakeProject.ForIterableList";
    }

    static TParseTree ForEverStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("for"), OPEN_PAR_WS, pegged.peg.literal!("ever"), CLOSE_PAR_WS, Block), "QMakeProject.ForEverStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ForEverStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("for"), OPEN_PAR_WS, pegged.peg.literal!("ever"), CLOSE_PAR_WS, Block), "QMakeProject.ForEverStatement"), "ForEverStatement")(p);
                memo[tuple(`ForEverStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ForEverStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("for"), OPEN_PAR_WS, pegged.peg.literal!("ever"), CLOSE_PAR_WS, Block), "QMakeProject.ForEverStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("for"), OPEN_PAR_WS, pegged.peg.literal!("ever"), CLOSE_PAR_WS, Block), "QMakeProject.ForEverStatement"), "ForEverStatement")(TParseTree("", false,[], s));
        }
    }
    static string ForEverStatement(GetName g)
    {
        return "QMakeProject.ForEverStatement";
    }

    static TParseTree ExpandStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(FunctionArgumentExpandStatement, ProjectVariableExpandStatement, MakefileVariableExpandStatement, EnvironmentVariableExpandStatement, PropertyVariableExpandStatement), "QMakeProject.ExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(FunctionArgumentExpandStatement, ProjectVariableExpandStatement, MakefileVariableExpandStatement, EnvironmentVariableExpandStatement, PropertyVariableExpandStatement), "QMakeProject.ExpandStatement"), "ExpandStatement")(p);
                memo[tuple(`ExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(FunctionArgumentExpandStatement, ProjectVariableExpandStatement, MakefileVariableExpandStatement, EnvironmentVariableExpandStatement, PropertyVariableExpandStatement), "QMakeProject.ExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(FunctionArgumentExpandStatement, ProjectVariableExpandStatement, MakefileVariableExpandStatement, EnvironmentVariableExpandStatement, PropertyVariableExpandStatement), "QMakeProject.ExpandStatement"), "ExpandStatement")(TParseTree("", false,[], s));
        }
    }
    static string ExpandStatement(GetName g)
    {
        return "QMakeProject.ExpandStatement";
    }

    static TParseTree FunctionArgumentExpandStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$$"), DecNumber), pegged.peg.and!(pegged.peg.literal!("$${"), DecNumber, pegged.peg.literal!("}"))), "QMakeProject.FunctionArgumentExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionArgumentExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$$"), DecNumber), pegged.peg.and!(pegged.peg.literal!("$${"), DecNumber, pegged.peg.literal!("}"))), "QMakeProject.FunctionArgumentExpandStatement"), "FunctionArgumentExpandStatement")(p);
                memo[tuple(`FunctionArgumentExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionArgumentExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$$"), DecNumber), pegged.peg.and!(pegged.peg.literal!("$${"), DecNumber, pegged.peg.literal!("}"))), "QMakeProject.FunctionArgumentExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$$"), DecNumber), pegged.peg.and!(pegged.peg.literal!("$${"), DecNumber, pegged.peg.literal!("}"))), "QMakeProject.FunctionArgumentExpandStatement"), "FunctionArgumentExpandStatement")(TParseTree("", false,[], s));
        }
    }
    static string FunctionArgumentExpandStatement(GetName g)
    {
        return "QMakeProject.FunctionArgumentExpandStatement";
    }

    static TParseTree MakefileVariableExpandStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$"), QualifiedIdentifier), pegged.peg.and!(pegged.peg.literal!("${"), QualifiedIdentifier, pegged.peg.literal!("}"))), "QMakeProject.MakefileVariableExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`MakefileVariableExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$"), QualifiedIdentifier), pegged.peg.and!(pegged.peg.literal!("${"), QualifiedIdentifier, pegged.peg.literal!("}"))), "QMakeProject.MakefileVariableExpandStatement"), "MakefileVariableExpandStatement")(p);
                memo[tuple(`MakefileVariableExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MakefileVariableExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$"), QualifiedIdentifier), pegged.peg.and!(pegged.peg.literal!("${"), QualifiedIdentifier, pegged.peg.literal!("}"))), "QMakeProject.MakefileVariableExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$"), QualifiedIdentifier), pegged.peg.and!(pegged.peg.literal!("${"), QualifiedIdentifier, pegged.peg.literal!("}"))), "QMakeProject.MakefileVariableExpandStatement"), "MakefileVariableExpandStatement")(TParseTree("", false,[], s));
        }
    }
    static string MakefileVariableExpandStatement(GetName g)
    {
        return "QMakeProject.MakefileVariableExpandStatement";
    }

    static TParseTree ProjectVariableExpandStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$$"), QualifiedIdentifier), pegged.peg.and!(pegged.peg.literal!("$${"), QualifiedIdentifier, pegged.peg.literal!("}")), pegged.peg.and!(pegged.peg.literal!("$$"), doublequote, ExpandStatement, doublequote)), "QMakeProject.ProjectVariableExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ProjectVariableExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$$"), QualifiedIdentifier), pegged.peg.and!(pegged.peg.literal!("$${"), QualifiedIdentifier, pegged.peg.literal!("}")), pegged.peg.and!(pegged.peg.literal!("$$"), doublequote, ExpandStatement, doublequote)), "QMakeProject.ProjectVariableExpandStatement"), "ProjectVariableExpandStatement")(p);
                memo[tuple(`ProjectVariableExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ProjectVariableExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$$"), QualifiedIdentifier), pegged.peg.and!(pegged.peg.literal!("$${"), QualifiedIdentifier, pegged.peg.literal!("}")), pegged.peg.and!(pegged.peg.literal!("$$"), doublequote, ExpandStatement, doublequote)), "QMakeProject.ProjectVariableExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.literal!("$$"), QualifiedIdentifier), pegged.peg.and!(pegged.peg.literal!("$${"), QualifiedIdentifier, pegged.peg.literal!("}")), pegged.peg.and!(pegged.peg.literal!("$$"), doublequote, ExpandStatement, doublequote)), "QMakeProject.ProjectVariableExpandStatement"), "ProjectVariableExpandStatement")(TParseTree("", false,[], s));
        }
    }
    static string ProjectVariableExpandStatement(GetName g)
    {
        return "QMakeProject.ProjectVariableExpandStatement";
    }

    static TParseTree EnvironmentVariableExpandStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("$$", "$"), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.EnvironmentVariableExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`EnvironmentVariableExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("$$", "$"), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.EnvironmentVariableExpandStatement"), "EnvironmentVariableExpandStatement")(p);
                memo[tuple(`EnvironmentVariableExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnvironmentVariableExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("$$", "$"), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.EnvironmentVariableExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.keywords!("$$", "$"), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.EnvironmentVariableExpandStatement"), "EnvironmentVariableExpandStatement")(TParseTree("", false,[], s));
        }
    }
    static string EnvironmentVariableExpandStatement(GetName g)
    {
        return "QMakeProject.EnvironmentVariableExpandStatement";
    }

    static TParseTree PropertyVariableExpandStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("$$["), QualifiedIdentifier, pegged.peg.option!(pegged.peg.keywords!("/get", "/src")), pegged.peg.literal!("]")), "QMakeProject.PropertyVariableExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`PropertyVariableExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("$$["), QualifiedIdentifier, pegged.peg.option!(pegged.peg.keywords!("/get", "/src")), pegged.peg.literal!("]")), "QMakeProject.PropertyVariableExpandStatement"), "PropertyVariableExpandStatement")(p);
                memo[tuple(`PropertyVariableExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PropertyVariableExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("$$["), QualifiedIdentifier, pegged.peg.option!(pegged.peg.keywords!("/get", "/src")), pegged.peg.literal!("]")), "QMakeProject.PropertyVariableExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("$$["), QualifiedIdentifier, pegged.peg.option!(pegged.peg.keywords!("/get", "/src")), pegged.peg.literal!("]")), "QMakeProject.PropertyVariableExpandStatement"), "PropertyVariableExpandStatement")(TParseTree("", false,[], s));
        }
    }
    static string PropertyVariableExpandStatement(GetName g)
    {
        return "QMakeProject.PropertyVariableExpandStatement";
    }

    static TParseTree EvalTestFunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("eval"), OPEN_PAR_WS, EvalArg, CLOSE_PAR_WS), "QMakeProject.EvalTestFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`EvalTestFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("eval"), OPEN_PAR_WS, EvalArg, CLOSE_PAR_WS), "QMakeProject.EvalTestFunctionCall"), "EvalTestFunctionCall")(p);
                memo[tuple(`EvalTestFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EvalTestFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("eval"), OPEN_PAR_WS, EvalArg, CLOSE_PAR_WS), "QMakeProject.EvalTestFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("eval"), OPEN_PAR_WS, EvalArg, CLOSE_PAR_WS), "QMakeProject.EvalTestFunctionCall"), "EvalTestFunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string EvalTestFunctionCall(GetName g)
    {
        return "QMakeProject.EvalTestFunctionCall";
    }

    static TParseTree EvalArg(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), Statement), "QMakeProject.EvalArg")(p);
        }
        else
        {
            if (auto m = tuple(`EvalArg`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), Statement), "QMakeProject.EvalArg"), "EvalArg")(p);
                memo[tuple(`EvalArg`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EvalArg(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), Statement), "QMakeProject.EvalArg")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), Statement), Statement), "QMakeProject.EvalArg"), "EvalArg")(TParseTree("", false,[], s));
        }
    }
    static string EvalArg(GetName g)
    {
        return "QMakeProject.EvalArg";
    }

    static TParseTree CacheTestFunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("cache"), OPEN_PAR_WS, pegged.peg.option!(CacheTestFunctionCallParams), CLOSE_PAR_WS), "QMakeProject.CacheTestFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`CacheTestFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("cache"), OPEN_PAR_WS, pegged.peg.option!(CacheTestFunctionCallParams), CLOSE_PAR_WS), "QMakeProject.CacheTestFunctionCall"), "CacheTestFunctionCall")(p);
                memo[tuple(`CacheTestFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CacheTestFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("cache"), OPEN_PAR_WS, pegged.peg.option!(CacheTestFunctionCallParams), CLOSE_PAR_WS), "QMakeProject.CacheTestFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("cache"), OPEN_PAR_WS, pegged.peg.option!(CacheTestFunctionCallParams), CLOSE_PAR_WS), "QMakeProject.CacheTestFunctionCall"), "CacheTestFunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string CacheTestFunctionCall(GetName g)
    {
        return "QMakeProject.CacheTestFunctionCall";
    }

    static TParseTree CacheTestFunctionCallParams(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(QualifiedIdentifier), pegged.peg.option!(pegged.peg.and!(COMMA_WS, CacheTestFunctionParam2)), pegged.peg.option!(pegged.peg.and!(COMMA_WS, FunctionArgument!(pegged.peg.or!(space, COMMA))))), "QMakeProject.CacheTestFunctionCallParams")(p);
        }
        else
        {
            if (auto m = tuple(`CacheTestFunctionCallParams`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(QualifiedIdentifier), pegged.peg.option!(pegged.peg.and!(COMMA_WS, CacheTestFunctionParam2)), pegged.peg.option!(pegged.peg.and!(COMMA_WS, FunctionArgument!(pegged.peg.or!(space, COMMA))))), "QMakeProject.CacheTestFunctionCallParams"), "CacheTestFunctionCallParams")(p);
                memo[tuple(`CacheTestFunctionCallParams`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CacheTestFunctionCallParams(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(QualifiedIdentifier), pegged.peg.option!(pegged.peg.and!(COMMA_WS, CacheTestFunctionParam2)), pegged.peg.option!(pegged.peg.and!(COMMA_WS, FunctionArgument!(pegged.peg.or!(space, COMMA))))), "QMakeProject.CacheTestFunctionCallParams")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(QualifiedIdentifier), pegged.peg.option!(pegged.peg.and!(COMMA_WS, CacheTestFunctionParam2)), pegged.peg.option!(pegged.peg.and!(COMMA_WS, FunctionArgument!(pegged.peg.or!(space, COMMA))))), "QMakeProject.CacheTestFunctionCallParams"), "CacheTestFunctionCallParams")(TParseTree("", false,[], s));
        }
    }
    static string CacheTestFunctionCallParams(GetName g)
    {
        return "QMakeProject.CacheTestFunctionCallParams";
    }

    static TParseTree CacheTestFunctionParam2(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.keywords!("set", "add", "sub")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(pegged.peg.literal!("transient")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(pegged.peg.keywords!("super", "stash"))), "QMakeProject.CacheTestFunctionParam2")(p);
        }
        else
        {
            if (auto m = tuple(`CacheTestFunctionParam2`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.keywords!("set", "add", "sub")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(pegged.peg.literal!("transient")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(pegged.peg.keywords!("super", "stash"))), "QMakeProject.CacheTestFunctionParam2"), "CacheTestFunctionParam2")(p);
                memo[tuple(`CacheTestFunctionParam2`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CacheTestFunctionParam2(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.keywords!("set", "add", "sub")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(pegged.peg.literal!("transient")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(pegged.peg.keywords!("super", "stash"))), "QMakeProject.CacheTestFunctionParam2")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(pegged.peg.keywords!("set", "add", "sub")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(pegged.peg.literal!("transient")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(pegged.peg.keywords!("super", "stash"))), "QMakeProject.CacheTestFunctionParam2"), "CacheTestFunctionParam2")(TParseTree("", false,[], s));
        }
    }
    static string CacheTestFunctionParam2(GetName g)
    {
        return "QMakeProject.CacheTestFunctionParam2";
    }

    static TParseTree ContainsTestFunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("contains"), OPEN_PAR_WS, QualifiedIdentifier, pegged.peg.and!(COMMA_WS, EnquotedString), CLOSE_PAR_WS), "QMakeProject.ContainsTestFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`ContainsTestFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("contains"), OPEN_PAR_WS, QualifiedIdentifier, pegged.peg.and!(COMMA_WS, EnquotedString), CLOSE_PAR_WS), "QMakeProject.ContainsTestFunctionCall"), "ContainsTestFunctionCall")(p);
                memo[tuple(`ContainsTestFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ContainsTestFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("contains"), OPEN_PAR_WS, QualifiedIdentifier, pegged.peg.and!(COMMA_WS, EnquotedString), CLOSE_PAR_WS), "QMakeProject.ContainsTestFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("contains"), OPEN_PAR_WS, QualifiedIdentifier, pegged.peg.and!(COMMA_WS, EnquotedString), CLOSE_PAR_WS), "QMakeProject.ContainsTestFunctionCall"), "ContainsTestFunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string ContainsTestFunctionCall(GetName g)
    {
        return "QMakeProject.ContainsTestFunctionCall";
    }

    static TParseTree ReturnFunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("return"), OPEN_PAR_WS, pegged.peg.option!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA)), Statement)), CLOSE_PAR_WS), "QMakeProject.ReturnFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`ReturnFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("return"), OPEN_PAR_WS, pegged.peg.option!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA)), Statement)), CLOSE_PAR_WS), "QMakeProject.ReturnFunctionCall"), "ReturnFunctionCall")(p);
                memo[tuple(`ReturnFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReturnFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("return"), OPEN_PAR_WS, pegged.peg.option!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA)), Statement)), CLOSE_PAR_WS), "QMakeProject.ReturnFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("return"), OPEN_PAR_WS, pegged.peg.option!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionArgument!(pegged.peg.or!(space, COMMA)), Statement)), CLOSE_PAR_WS), "QMakeProject.ReturnFunctionCall"), "ReturnFunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string ReturnFunctionCall(GetName g)
    {
        return "QMakeProject.ReturnFunctionCall";
    }

    static TParseTree RequiresFunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("requires"), OPEN_PAR_WS, BooleanExpression, CLOSE_PAR_WS), "QMakeProject.RequiresFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`RequiresFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("requires"), OPEN_PAR_WS, BooleanExpression, CLOSE_PAR_WS), "QMakeProject.RequiresFunctionCall"), "RequiresFunctionCall")(p);
                memo[tuple(`RequiresFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RequiresFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("requires"), OPEN_PAR_WS, BooleanExpression, CLOSE_PAR_WS), "QMakeProject.RequiresFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("requires"), OPEN_PAR_WS, BooleanExpression, CLOSE_PAR_WS), "QMakeProject.RequiresFunctionCall"), "RequiresFunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string RequiresFunctionCall(GetName g)
    {
        return "QMakeProject.RequiresFunctionCall";
    }

    static TParseTree EnquotedString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedString, SingleEnquotedString), "QMakeProject.EnquotedString")(p);
        }
        else
        {
            if (auto m = tuple(`EnquotedString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedString, SingleEnquotedString), "QMakeProject.EnquotedString"), "EnquotedString")(p);
                memo[tuple(`EnquotedString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnquotedString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedString, SingleEnquotedString), "QMakeProject.EnquotedString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DoubleEnquotedString, SingleEnquotedString), "QMakeProject.EnquotedString"), "EnquotedString")(TParseTree("", false,[], s));
        }
    }
    static string EnquotedString(GetName g)
    {
        return "QMakeProject.EnquotedString";
    }

    static TParseTree DoubleEnquotedString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(NonDoubleQuoteCharacter)), doublequote), "QMakeProject.DoubleEnquotedString")(p);
        }
        else
        {
            if (auto m = tuple(`DoubleEnquotedString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(NonDoubleQuoteCharacter)), doublequote), "QMakeProject.DoubleEnquotedString"), "DoubleEnquotedString")(p);
                memo[tuple(`DoubleEnquotedString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DoubleEnquotedString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(NonDoubleQuoteCharacter)), doublequote), "QMakeProject.DoubleEnquotedString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(doublequote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(NonDoubleQuoteCharacter)), doublequote), "QMakeProject.DoubleEnquotedString"), "DoubleEnquotedString")(TParseTree("", false,[], s));
        }
    }
    static string DoubleEnquotedString(GetName g)
    {
        return "QMakeProject.DoubleEnquotedString";
    }

    static TParseTree NonDoubleQuoteCharacter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(doublequote, BACKSLASH, LineTerminator)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonDoubleQuoteCharacter")(p);
        }
        else
        {
            if (auto m = tuple(`NonDoubleQuoteCharacter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(doublequote, BACKSLASH, LineTerminator)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonDoubleQuoteCharacter"), "NonDoubleQuoteCharacter")(p);
                memo[tuple(`NonDoubleQuoteCharacter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonDoubleQuoteCharacter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(doublequote, BACKSLASH, LineTerminator)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonDoubleQuoteCharacter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(doublequote, BACKSLASH, LineTerminator)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonDoubleQuoteCharacter"), "NonDoubleQuoteCharacter")(TParseTree("", false,[], s));
        }
    }
    static string NonDoubleQuoteCharacter(GetName g)
    {
        return "QMakeProject.NonDoubleQuoteCharacter";
    }

    static TParseTree SingleEnquotedString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(NonSingleQuoteCharacter)), quote), "QMakeProject.SingleEnquotedString")(p);
        }
        else
        {
            if (auto m = tuple(`SingleEnquotedString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(NonSingleQuoteCharacter)), quote), "QMakeProject.SingleEnquotedString"), "SingleEnquotedString")(p);
                memo[tuple(`SingleEnquotedString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleEnquotedString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(NonSingleQuoteCharacter)), quote), "QMakeProject.SingleEnquotedString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(quote, pegged.peg.fuse!(pegged.peg.zeroOrMore!(NonSingleQuoteCharacter)), quote), "QMakeProject.SingleEnquotedString"), "SingleEnquotedString")(TParseTree("", false,[], s));
        }
    }
    static string SingleEnquotedString(GetName g)
    {
        return "QMakeProject.SingleEnquotedString";
    }

    static TParseTree NonSingleQuoteCharacter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(quote, BACKSLASH, LineTerminator)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonSingleQuoteCharacter")(p);
        }
        else
        {
            if (auto m = tuple(`NonSingleQuoteCharacter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(quote, BACKSLASH, LineTerminator)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonSingleQuoteCharacter"), "NonSingleQuoteCharacter")(p);
                memo[tuple(`NonSingleQuoteCharacter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonSingleQuoteCharacter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(quote, BACKSLASH, LineTerminator)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonSingleQuoteCharacter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(quote, BACKSLASH, LineTerminator)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonSingleQuoteCharacter"), "NonSingleQuoteCharacter")(TParseTree("", false,[], s));
        }
    }
    static string NonSingleQuoteCharacter(GetName g)
    {
        return "QMakeProject.NonSingleQuoteCharacter";
    }

    static TParseTree Identifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"))))), "QMakeProject.Identifier")(p);
        }
        else
        {
            if (auto m = tuple(`Identifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"))))), "QMakeProject.Identifier"), "Identifier")(p);
                memo[tuple(`Identifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Identifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"))))), "QMakeProject.Identifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_")), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"))))), "QMakeProject.Identifier"), "Identifier")(TParseTree("", false,[], s));
        }
    }
    static string Identifier(GetName g)
    {
        return "QMakeProject.Identifier";
    }

    static TParseTree QMakeIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("*")))), "QMakeProject.QMakeIdentifier")(p);
        }
        else
        {
            if (auto m = tuple(`QMakeIdentifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("*")))), "QMakeProject.QMakeIdentifier"), "QMakeIdentifier")(p);
                memo[tuple(`QMakeIdentifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree QMakeIdentifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("*")))), "QMakeProject.QMakeIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.literal!("_"), pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!("+"), pegged.peg.literal!("*")))), "QMakeProject.QMakeIdentifier"), "QMakeIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string QMakeIdentifier(GetName g)
    {
        return "QMakeProject.QMakeIdentifier";
    }

    static TParseTree NumberFunctionCall(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.literal!("size"), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.NumberFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`NumberFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.literal!("size"), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.NumberFunctionCall"), "NumberFunctionCall")(p);
                memo[tuple(`NumberFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NumberFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.literal!("size"), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.NumberFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.literal!("size"), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.NumberFunctionCall"), "NumberFunctionCall")(TParseTree("", false,[], s));
        }
    }
    static string NumberFunctionCall(GetName g)
    {
        return "QMakeProject.NumberFunctionCall";
    }

    static TParseTree LValueImpl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(NumberFunctionCall, ExpandStatement, QMakeIdentifier), "QMakeProject.LValueImpl")(p);
        }
        else
        {
            if (auto m = tuple(`LValueImpl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(NumberFunctionCall, ExpandStatement, QMakeIdentifier), "QMakeProject.LValueImpl"), "LValueImpl")(p);
                memo[tuple(`LValueImpl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LValueImpl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(NumberFunctionCall, ExpandStatement, QMakeIdentifier), "QMakeProject.LValueImpl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(NumberFunctionCall, ExpandStatement, QMakeIdentifier), "QMakeProject.LValueImpl"), "LValueImpl")(TParseTree("", false,[], s));
        }
    }
    static string LValueImpl(GetName g)
    {
        return "QMakeProject.LValueImpl";
    }

    static TParseTree LValue(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(LValueImpl, pegged.peg.zeroOrMore!(LValueImpl)), "QMakeProject.LValue")(p);
        }
        else
        {
            if (auto m = tuple(`LValue`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(LValueImpl, pegged.peg.zeroOrMore!(LValueImpl)), "QMakeProject.LValue"), "LValue")(p);
                memo[tuple(`LValue`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LValue(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(LValueImpl, pegged.peg.zeroOrMore!(LValueImpl)), "QMakeProject.LValue")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(LValueImpl, pegged.peg.zeroOrMore!(LValueImpl)), "QMakeProject.LValue"), "LValue")(TParseTree("", false,[], s));
        }
    }
    static string LValue(GetName g)
    {
        return "QMakeProject.LValue";
    }

    static TParseTree QualifiedIdentifier(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!(".")), LValue, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("."), LValue)))), "QMakeProject.QualifiedIdentifier")(p);
        }
        else
        {
            if (auto m = tuple(`QualifiedIdentifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!(".")), LValue, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("."), LValue)))), "QMakeProject.QualifiedIdentifier"), "QualifiedIdentifier")(p);
                memo[tuple(`QualifiedIdentifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree QualifiedIdentifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!(".")), LValue, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("."), LValue)))), "QMakeProject.QualifiedIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.option!(pegged.peg.literal!(".")), LValue, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.literal!("."), LValue)))), "QMakeProject.QualifiedIdentifier"), "QualifiedIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string QualifiedIdentifier(GetName g)
    {
        return "QMakeProject.QualifiedIdentifier";
    }

    static TParseTree EscapeSequence(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(quote, doublequote, BACKSLASH, pegged.peg.literal!("$"), pegged.peg.literal!("."), pegged.peg.literal!("?"), pegged.peg.literal!("a"), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit, HexDigit, HexDigit))), pegged.peg.fuse!(pegged.peg.and!(OctDigit, OctDigit, OctDigit)), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit))), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"), pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("|"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("{"), pegged.peg.literal!("}")), "QMakeProject.EscapeSequence")(p);
        }
        else
        {
            if (auto m = tuple(`EscapeSequence`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(quote, doublequote, BACKSLASH, pegged.peg.literal!("$"), pegged.peg.literal!("."), pegged.peg.literal!("?"), pegged.peg.literal!("a"), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit, HexDigit, HexDigit))), pegged.peg.fuse!(pegged.peg.and!(OctDigit, OctDigit, OctDigit)), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit))), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"), pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("|"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("{"), pegged.peg.literal!("}")), "QMakeProject.EscapeSequence"), "EscapeSequence")(p);
                memo[tuple(`EscapeSequence`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EscapeSequence(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(quote, doublequote, BACKSLASH, pegged.peg.literal!("$"), pegged.peg.literal!("."), pegged.peg.literal!("?"), pegged.peg.literal!("a"), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit, HexDigit, HexDigit))), pegged.peg.fuse!(pegged.peg.and!(OctDigit, OctDigit, OctDigit)), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit))), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"), pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("|"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("{"), pegged.peg.literal!("}")), "QMakeProject.EscapeSequence")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(quote, doublequote, BACKSLASH, pegged.peg.literal!("$"), pegged.peg.literal!("."), pegged.peg.literal!("?"), pegged.peg.literal!("a"), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit, HexDigit, HexDigit))), pegged.peg.fuse!(pegged.peg.and!(OctDigit, OctDigit, OctDigit)), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit))), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"), pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("|"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("{"), pegged.peg.literal!("}")), "QMakeProject.EscapeSequence"), "EscapeSequence")(TParseTree("", false,[], s));
        }
    }
    static string EscapeSequence(GetName g)
    {
        return "QMakeProject.EscapeSequence";
    }

    static TParseTree OctDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '7'), "QMakeProject.OctDigit")(p);
        }
        else
        {
            if (auto m = tuple(`OctDigit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '7'), "QMakeProject.OctDigit"), "OctDigit")(p);
                memo[tuple(`OctDigit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OctDigit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '7'), "QMakeProject.OctDigit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '7'), "QMakeProject.OctDigit"), "OctDigit")(TParseTree("", false,[], s));
        }
    }
    static string OctDigit(GetName g)
    {
        return "QMakeProject.OctDigit";
    }

    static TParseTree DecDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "QMakeProject.DecDigit")(p);
        }
        else
        {
            if (auto m = tuple(`DecDigit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "QMakeProject.DecDigit"), "DecDigit")(p);
                memo[tuple(`DecDigit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DecDigit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "QMakeProject.DecDigit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "QMakeProject.DecDigit"), "DecDigit")(TParseTree("", false,[], s));
        }
    }
    static string DecDigit(GetName g)
    {
        return "QMakeProject.DecDigit";
    }

    static TParseTree HexDigit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), "QMakeProject.HexDigit")(p);
        }
        else
        {
            if (auto m = tuple(`HexDigit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), "QMakeProject.HexDigit"), "HexDigit")(p);
                memo[tuple(`HexDigit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree HexDigit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), "QMakeProject.HexDigit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.charRange!('0', '9'), pegged.peg.charRange!('a', 'f'), pegged.peg.charRange!('A', 'F')), "QMakeProject.HexDigit"), "HexDigit")(TParseTree("", false,[], s));
        }
    }
    static string HexDigit(GetName g)
    {
        return "QMakeProject.HexDigit";
    }

    static TParseTree DecNumber(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(DecDigit), "QMakeProject.DecNumber")(p);
        }
        else
        {
            if (auto m = tuple(`DecNumber`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(DecDigit), "QMakeProject.DecNumber"), "DecNumber")(p);
                memo[tuple(`DecNumber`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DecNumber(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.oneOrMore!(DecDigit), "QMakeProject.DecNumber")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.oneOrMore!(DecDigit), "QMakeProject.DecNumber"), "DecNumber")(TParseTree("", false,[], s));
        }
    }
    static string DecNumber(GetName g)
    {
        return "QMakeProject.DecNumber";
    }

    static TParseTree EXPAND_MARKER(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("$$", "\\$\\$"), "QMakeProject.EXPAND_MARKER")(p);
        }
        else
        {
            if (auto m = tuple(`EXPAND_MARKER`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("$$", "\\$\\$"), "QMakeProject.EXPAND_MARKER"), "EXPAND_MARKER")(p);
                memo[tuple(`EXPAND_MARKER`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EXPAND_MARKER(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("$$", "\\$\\$"), "QMakeProject.EXPAND_MARKER")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("$$", "\\$\\$"), "QMakeProject.EXPAND_MARKER"), "EXPAND_MARKER")(TParseTree("", false,[], s));
        }
    }
    static string EXPAND_MARKER(GetName g)
    {
        return "QMakeProject.EXPAND_MARKER";
    }

    static TParseTree COMMA(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!(","), "QMakeProject.COMMA")(p);
        }
        else
        {
            if (auto m = tuple(`COMMA`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!(","), "QMakeProject.COMMA"), "COMMA")(p);
                memo[tuple(`COMMA`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree COMMA(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!(","), "QMakeProject.COMMA")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!(","), "QMakeProject.COMMA"), "COMMA")(TParseTree("", false,[], s));
        }
    }
    static string COMMA(GetName g)
    {
        return "QMakeProject.COMMA";
    }

    static TParseTree COMMA_WS(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(","), pegged.peg.discard!(pegged.peg.zeroOrMore!(space))), "QMakeProject.COMMA_WS")(p);
        }
        else
        {
            if (auto m = tuple(`COMMA_WS`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(","), pegged.peg.discard!(pegged.peg.zeroOrMore!(space))), "QMakeProject.COMMA_WS"), "COMMA_WS")(p);
                memo[tuple(`COMMA_WS`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree COMMA_WS(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(","), pegged.peg.discard!(pegged.peg.zeroOrMore!(space))), "QMakeProject.COMMA_WS")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(","), pegged.peg.discard!(pegged.peg.zeroOrMore!(space))), "QMakeProject.COMMA_WS"), "COMMA_WS")(TParseTree("", false,[], s));
        }
    }
    static string COMMA_WS(GetName g)
    {
        return "QMakeProject.COMMA_WS";
    }

    static TParseTree OPEN_PAR_WS(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("("), pegged.peg.discard!(pegged.peg.zeroOrMore!(space))), "QMakeProject.OPEN_PAR_WS")(p);
        }
        else
        {
            if (auto m = tuple(`OPEN_PAR_WS`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("("), pegged.peg.discard!(pegged.peg.zeroOrMore!(space))), "QMakeProject.OPEN_PAR_WS"), "OPEN_PAR_WS")(p);
                memo[tuple(`OPEN_PAR_WS`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree OPEN_PAR_WS(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("("), pegged.peg.discard!(pegged.peg.zeroOrMore!(space))), "QMakeProject.OPEN_PAR_WS")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("("), pegged.peg.discard!(pegged.peg.zeroOrMore!(space))), "QMakeProject.OPEN_PAR_WS"), "OPEN_PAR_WS")(TParseTree("", false,[], s));
        }
    }
    static string OPEN_PAR_WS(GetName g)
    {
        return "QMakeProject.OPEN_PAR_WS";
    }

    static TParseTree CLOSE_PAR_WS(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(")")), "QMakeProject.CLOSE_PAR_WS")(p);
        }
        else
        {
            if (auto m = tuple(`CLOSE_PAR_WS`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(")")), "QMakeProject.CLOSE_PAR_WS"), "CLOSE_PAR_WS")(p);
                memo[tuple(`CLOSE_PAR_WS`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CLOSE_PAR_WS(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(")")), "QMakeProject.CLOSE_PAR_WS")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!(")")), "QMakeProject.CLOSE_PAR_WS"), "CLOSE_PAR_WS")(TParseTree("", false,[], s));
        }
    }
    static string CLOSE_PAR_WS(GetName g)
    {
        return "QMakeProject.CLOSE_PAR_WS";
    }

    static TParseTree BACKSLASH(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("\\"), "QMakeProject.BACKSLASH")(p);
        }
        else
        {
            if (auto m = tuple(`BACKSLASH`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("\\"), "QMakeProject.BACKSLASH"), "BACKSLASH")(p);
                memo[tuple(`BACKSLASH`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BACKSLASH(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("\\"), "QMakeProject.BACKSLASH")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("\\"), "QMakeProject.BACKSLASH"), "BACKSLASH")(TParseTree("", false,[], s));
        }
    }
    static string BACKSLASH(GetName g)
    {
        return "QMakeProject.BACKSLASH";
    }

    static TParseTree SourceCharacter(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('\u0000', '\uFFFC'), "QMakeProject.SourceCharacter")(p);
        }
        else
        {
            if (auto m = tuple(`SourceCharacter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.charRange!('\u0000', '\uFFFC'), "QMakeProject.SourceCharacter"), "SourceCharacter")(p);
                memo[tuple(`SourceCharacter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SourceCharacter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('\u0000', '\uFFFC'), "QMakeProject.SourceCharacter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.charRange!('\u0000', '\uFFFC'), "QMakeProject.SourceCharacter"), "SourceCharacter")(TParseTree("", false,[], s));
        }
    }
    static string SourceCharacter(GetName g)
    {
        return "QMakeProject.SourceCharacter";
    }

    static TParseTree LineTerminator(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("\u000A"), pegged.peg.and!(pegged.peg.literal!("\u000D"), pegged.peg.literal!("\u000A")), pegged.peg.literal!("\u2028"), pegged.peg.literal!("\u2029")), "QMakeProject.LineTerminator")(p);
        }
        else
        {
            if (auto m = tuple(`LineTerminator`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("\u000A"), pegged.peg.and!(pegged.peg.literal!("\u000D"), pegged.peg.literal!("\u000A")), pegged.peg.literal!("\u2028"), pegged.peg.literal!("\u2029")), "QMakeProject.LineTerminator"), "LineTerminator")(p);
                memo[tuple(`LineTerminator`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LineTerminator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("\u000A"), pegged.peg.and!(pegged.peg.literal!("\u000D"), pegged.peg.literal!("\u000A")), pegged.peg.literal!("\u2028"), pegged.peg.literal!("\u2029")), "QMakeProject.LineTerminator")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.literal!("\u000A"), pegged.peg.and!(pegged.peg.literal!("\u000D"), pegged.peg.literal!("\u000A")), pegged.peg.literal!("\u2028"), pegged.peg.literal!("\u2029")), "QMakeProject.LineTerminator"), "LineTerminator")(TParseTree("", false,[], s));
        }
    }
    static string LineTerminator(GetName g)
    {
        return "QMakeProject.LineTerminator";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(Project(p));
        result.children = [result];
        result.name = "QMakeProject";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return QMakeProject(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return QMakeProject(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "QMakeProject";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericQMakeProject!(ParseTree).QMakeProject QMakeProject;

