/++
This module was automatically generated from the following grammar:


    QMakeProject:
        Project <- Statement* eoi
        Statement <- FunctionDeclaration / Assignment / Scope / Block / BooleanExpression / ReplaceFunctionCall / TestFunctionCall / Comment / EmptyStatement

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

        FunctionArgumentList       <- List(:COMMA_WS, :COMMA) / List(:space+, :space) / FunctionFirstArgument
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

        # eval(string)
        EvalTestFunctionCall <- "eval" OPEN_PAR_WS EvalArg CLOSE_PAR_WS
        EvalArg <- (QualifiedIdentifier :space* "=" :space* Statement) / Statement

        # cache(variablename, [set|add|sub] [transient] [super|stash], [source variablename])
        # Special case:
        # cache(, super)
        CacheTestFunctionCall       <- "cache" OPEN_PAR_WS CacheTestFunctionCallParams? CLOSE_PAR_WS
        CacheTestFunctionCallParams <- QualifiedIdentifier? (COMMA_WS CacheTestFunctionParam2)? (COMMA_WS QualifiedIdentifier)?
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
        rules["Block"] = toDelegate(&Block);
        rules["SingleLineBlock"] = toDelegate(&SingleLineBlock);
        rules["MultiLineBlock"] = toDelegate(&MultiLineBlock);
        rules["Comment"] = toDelegate(&Comment);
        rules["MultiLineComment"] = toDelegate(&MultiLineComment);
        rules["SingleLineComment"] = toDelegate(&SingleLineComment);
        rules["SingleLineCommentChars"] = toDelegate(&SingleLineCommentChars);
        rules["SingleLineCommentChar"] = toDelegate(&SingleLineCommentChar);
        rules["Assignment"] = toDelegate(&Assignment);
        rules["StringList"] = toDelegate(&StringList);
        rules["String"] = toDelegate(&String);
        rules["StringChars"] = toDelegate(&StringChars);
        rules["StringChar"] = toDelegate(&StringChar);
        rules["DirectAssignment"] = toDelegate(&DirectAssignment);
        rules["AppendAssignment"] = toDelegate(&AppendAssignment);
        rules["AppendUniqueAssignment"] = toDelegate(&AppendUniqueAssignment);
        rules["RemoveAssignment"] = toDelegate(&RemoveAssignment);
        rules["ReplaceAssignment"] = toDelegate(&ReplaceAssignment);
        rules["TestFunctionCall"] = toDelegate(&TestFunctionCall);
        rules["ReplaceFunctionCall"] = toDelegate(&ReplaceFunctionCall);
        rules["FunctionCall"] = toDelegate(&FunctionCall);
        rules["FunctionId"] = toDelegate(&FunctionId);
        rules["FunctionArgumentList"] = toDelegate(&FunctionArgumentList);
        rules["FunctionFirstArgument"] = toDelegate(&FunctionFirstArgument);
        rules["FunctionFirstArgumentImpl"] = toDelegate(&FunctionFirstArgumentImpl);
        rules["FunctionFirstArgumentString"] = toDelegate(&FunctionFirstArgumentString);
        rules["FunctionFirstArgumentStringChar"] = toDelegate(&FunctionFirstArgumentStringChar);
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
        rules["EvalTestFunctionCall"] = toDelegate(&EvalTestFunctionCall);
        rules["EvalArg"] = toDelegate(&EvalArg);
        rules["CacheTestFunctionCall"] = toDelegate(&CacheTestFunctionCall);
        rules["CacheTestFunctionCallParams"] = toDelegate(&CacheTestFunctionCallParams);
        rules["CacheTestFunctionParam2"] = toDelegate(&CacheTestFunctionParam2);
        rules["ContainsTestFunctionCall"] = toDelegate(&ContainsTestFunctionCall);
        rules["ReturnFunctionCall"] = toDelegate(&ReturnFunctionCall);
        rules["RequiresFunctionCall"] = toDelegate(&RequiresFunctionCall);
        rules["RegularString"] = toDelegate(&RegularString);
        rules["RegularStringChars"] = toDelegate(&RegularStringChars);
        rules["RegularStringChar"] = toDelegate(&RegularStringChar);
        rules["EnquotedString"] = toDelegate(&EnquotedString);
        rules["DoubleEnquotedString"] = toDelegate(&DoubleEnquotedString);
        rules["NonDoubleQuoteCharacter"] = toDelegate(&NonDoubleQuoteCharacter);
        rules["SingleEnquotedString"] = toDelegate(&SingleEnquotedString);
        rules["NonSingleQuoteCharacter"] = toDelegate(&NonSingleQuoteCharacter);
        rules["Identifier"] = toDelegate(&Identifier);
        rules["QMakeIdentifier"] = toDelegate(&QMakeIdentifier);
        rules["ExpandStatement"] = toDelegate(&ExpandStatement);
        rules["ProjectVariableExpandStatement"] = toDelegate(&ProjectVariableExpandStatement);
        rules["EnvironmentVariableExpandStatement"] = toDelegate(&EnvironmentVariableExpandStatement);
        rules["PropertyVariableExpandStatement"] = toDelegate(&PropertyVariableExpandStatement);
        rules["LValueImpl"] = toDelegate(&LValueImpl);
        rules["LValue"] = toDelegate(&LValue);
        rules["QualifiedIdentifier"] = toDelegate(&QualifiedIdentifier);
        rules["EscapeSequence"] = toDelegate(&EscapeSequence);
        rules["OctDigit"] = toDelegate(&OctDigit);
        rules["DecDigit"] = toDelegate(&DecDigit);
        rules["HexDigit"] = toDelegate(&HexDigit);
        rules["SINGLE_EXPAND_MARKER"] = toDelegate(&SINGLE_EXPAND_MARKER);
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
            return         pegged.peg.defined!(pegged.peg.or!(FunctionDeclaration, Assignment, Scope, Block, BooleanExpression, ReplaceFunctionCall, TestFunctionCall, Comment, EmptyStatement), "QMakeProject.Statement")(p);
        }
        else
        {
            if (auto m = tuple(`Statement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(FunctionDeclaration, Assignment, Scope, Block, BooleanExpression, ReplaceFunctionCall, TestFunctionCall, Comment, EmptyStatement), "QMakeProject.Statement"), "Statement")(p);
                memo[tuple(`Statement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Statement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(FunctionDeclaration, Assignment, Scope, Block, BooleanExpression, ReplaceFunctionCall, TestFunctionCall, Comment, EmptyStatement), "QMakeProject.Statement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(FunctionDeclaration, Assignment, Scope, Block, BooleanExpression, ReplaceFunctionCall, TestFunctionCall, Comment, EmptyStatement), "QMakeProject.Statement"), "Statement")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(eps, pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.EmptyStatement")(p);
        }
        else
        {
            if (auto m = tuple(`EmptyStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(eps, pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.EmptyStatement"), "EmptyStatement")(p);
                memo[tuple(`EmptyStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EmptyStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(eps, pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.EmptyStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(eps, pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.EmptyStatement"), "EmptyStatement")(TParseTree("", false,[], s));
        }
    }
    static string EmptyStatement(GetName g)
    {
        return "QMakeProject.EmptyStatement";
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.oneOrMore!(Statement), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), pegged.peg.literal!("}"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.MultiLineBlock")(p);
        }
        else
        {
            if (auto m = tuple(`MultiLineBlock`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.oneOrMore!(Statement), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), pegged.peg.literal!("}"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.MultiLineBlock"), "MultiLineBlock")(p);
                memo[tuple(`MultiLineBlock`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree MultiLineBlock(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.oneOrMore!(Statement), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), pegged.peg.literal!("}"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.MultiLineBlock")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.oneOrMore!(Statement), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), pegged.peg.literal!("}"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.MultiLineBlock"), "MultiLineBlock")(TParseTree("", false,[], s));
        }
    }
    static string MultiLineBlock(GetName g)
    {
        return "QMakeProject.MultiLineBlock";
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.option!(SingleLineCommentChars), pegged.peg.discard!(eol)), "QMakeProject.SingleLineComment")(p);
        }
        else
        {
            if (auto m = tuple(`SingleLineComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.option!(SingleLineCommentChars), pegged.peg.discard!(eol)), "QMakeProject.SingleLineComment"), "SingleLineComment")(p);
                memo[tuple(`SingleLineComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SingleLineComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.option!(SingleLineCommentChars), pegged.peg.discard!(eol)), "QMakeProject.SingleLineComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("#")), pegged.peg.option!(SingleLineCommentChars), pegged.peg.discard!(eol)), "QMakeProject.SingleLineComment"), "SingleLineComment")(TParseTree("", false,[], s));
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

    static TParseTree Assignment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DirectAssignment, AppendAssignment, AppendUniqueAssignment, RemoveAssignment, ReplaceAssignment), "QMakeProject.Assignment")(p);
        }
        else
        {
            if (auto m = tuple(`Assignment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(DirectAssignment, AppendAssignment, AppendUniqueAssignment, RemoveAssignment, ReplaceAssignment), "QMakeProject.Assignment"), "Assignment")(p);
                memo[tuple(`Assignment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Assignment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(DirectAssignment, AppendAssignment, AppendUniqueAssignment, RemoveAssignment, ReplaceAssignment), "QMakeProject.Assignment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(DirectAssignment, AppendAssignment, AppendUniqueAssignment, RemoveAssignment, ReplaceAssignment), "QMakeProject.Assignment"), "Assignment")(TParseTree("", false,[], s));
        }
    }
    static string Assignment(GetName g)
    {
        return "QMakeProject.Assignment";
    }

    static TParseTree StringList(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(String, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), String))), "QMakeProject.StringList")(p);
        }
        else
        {
            if (auto m = tuple(`StringList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(String, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), String))), "QMakeProject.StringList"), "StringList")(p);
                memo[tuple(`StringList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(String, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), String))), "QMakeProject.StringList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(String, pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), String))), "QMakeProject.StringList"), "StringList")(TParseTree("", false,[], s));
        }
    }
    static string StringList(GetName g)
    {
        return "QMakeProject.StringList";
    }

    static TParseTree String(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.option!(StringChars), "QMakeProject.String")(p);
        }
        else
        {
            if (auto m = tuple(`String`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.option!(StringChars), "QMakeProject.String"), "String")(p);
                memo[tuple(`String`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree String(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.option!(StringChars), "QMakeProject.String")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.option!(StringChars), "QMakeProject.String"), "String")(TParseTree("", false,[], s));
        }
    }
    static string String(GetName g)
    {
        return "QMakeProject.String";
    }

    static TParseTree StringChars(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(StringChar)), "QMakeProject.StringChars")(p);
        }
        else
        {
            if (auto m = tuple(`StringChars`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(StringChar)), "QMakeProject.StringChars"), "StringChars")(p);
                memo[tuple(`StringChars`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringChars(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(StringChar)), "QMakeProject.StringChars")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(StringChar)), "QMakeProject.StringChars"), "StringChars")(TParseTree("", false,[], s));
        }
    }
    static string StringChars(GetName g)
    {
        return "QMakeProject.StringChars";
    }

    static TParseTree StringChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(blank, LineTerminator)), SourceCharacter), "QMakeProject.StringChar")(p);
        }
        else
        {
            if (auto m = tuple(`StringChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(blank, LineTerminator)), SourceCharacter), "QMakeProject.StringChar"), "StringChar")(p);
                memo[tuple(`StringChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree StringChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(blank, LineTerminator)), SourceCharacter), "QMakeProject.StringChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(blank, LineTerminator)), SourceCharacter), "QMakeProject.StringChar"), "StringChar")(TParseTree("", false,[], s));
        }
    }
    static string StringChar(GetName g)
    {
        return "QMakeProject.StringChar";
    }

    static TParseTree DirectAssignment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.DirectAssignment")(p);
        }
        else
        {
            if (auto m = tuple(`DirectAssignment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.DirectAssignment"), "DirectAssignment")(p);
                memo[tuple(`DirectAssignment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DirectAssignment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.DirectAssignment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.DirectAssignment"), "DirectAssignment")(TParseTree("", false,[], s));
        }
    }
    static string DirectAssignment(GetName g)
    {
        return "QMakeProject.DirectAssignment";
    }

    static TParseTree AppendAssignment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("+="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.AppendAssignment")(p);
        }
        else
        {
            if (auto m = tuple(`AppendAssignment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("+="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.AppendAssignment"), "AppendAssignment")(p);
                memo[tuple(`AppendAssignment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AppendAssignment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("+="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.AppendAssignment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("+="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.AppendAssignment"), "AppendAssignment")(TParseTree("", false,[], s));
        }
    }
    static string AppendAssignment(GetName g)
    {
        return "QMakeProject.AppendAssignment";
    }

    static TParseTree AppendUniqueAssignment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("*="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.AppendUniqueAssignment")(p);
        }
        else
        {
            if (auto m = tuple(`AppendUniqueAssignment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("*="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.AppendUniqueAssignment"), "AppendUniqueAssignment")(p);
                memo[tuple(`AppendUniqueAssignment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AppendUniqueAssignment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("*="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.AppendUniqueAssignment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("*="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.AppendUniqueAssignment"), "AppendUniqueAssignment")(TParseTree("", false,[], s));
        }
    }
    static string AppendUniqueAssignment(GetName g)
    {
        return "QMakeProject.AppendUniqueAssignment";
    }

    static TParseTree RemoveAssignment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("-="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.RemoveAssignment")(p);
        }
        else
        {
            if (auto m = tuple(`RemoveAssignment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("-="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.RemoveAssignment"), "RemoveAssignment")(p);
                memo[tuple(`RemoveAssignment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RemoveAssignment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("-="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.RemoveAssignment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("-="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.RemoveAssignment"), "RemoveAssignment")(TParseTree("", false,[], s));
        }
    }
    static string RemoveAssignment(GetName g)
    {
        return "QMakeProject.RemoveAssignment";
    }

    static TParseTree ReplaceAssignment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("~="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.ReplaceAssignment")(p);
        }
        else
        {
            if (auto m = tuple(`ReplaceAssignment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("~="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.ReplaceAssignment"), "ReplaceAssignment")(p);
                memo[tuple(`ReplaceAssignment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReplaceAssignment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("~="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.ReplaceAssignment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("~="), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.option!(StringList), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.ReplaceAssignment"), "ReplaceAssignment")(TParseTree("", false,[], s));
        }
    }
    static string ReplaceAssignment(GetName g)
    {
        return "QMakeProject.ReplaceAssignment";
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
            return         pegged.peg.defined!(pegged.peg.and!(FunctionId, OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.FunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionId, OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.FunctionCall"), "FunctionCall")(p);
                memo[tuple(`FunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionId, OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.FunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionId, OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol))), "QMakeProject.FunctionCall"), "FunctionCall")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.or!(List!(pegged.peg.discard!(COMMA_WS), pegged.peg.discard!(COMMA)), List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionFirstArgument), "QMakeProject.FunctionArgumentList")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionArgumentList`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(List!(pegged.peg.discard!(COMMA_WS), pegged.peg.discard!(COMMA)), List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionFirstArgument), "QMakeProject.FunctionArgumentList"), "FunctionArgumentList")(p);
                memo[tuple(`FunctionArgumentList`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionArgumentList(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(List!(pegged.peg.discard!(COMMA_WS), pegged.peg.discard!(COMMA)), List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionFirstArgument), "QMakeProject.FunctionArgumentList")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(List!(pegged.peg.discard!(COMMA_WS), pegged.peg.discard!(COMMA)), List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionFirstArgument), "QMakeProject.FunctionArgumentList"), "FunctionArgumentList")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(FunctionFirstArgument, pegged.peg.oneOrMore!(pegged.peg.and!(delimRule, pegged.peg.option!(FunctionNextArgument!(delimChar))))), "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionFirstArgument, pegged.peg.oneOrMore!(pegged.peg.and!(delimRule, pegged.peg.option!(FunctionNextArgument!(delimChar))))), "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")"), "List_2")(p);
                memo[tuple("List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree List(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionFirstArgument, pegged.peg.oneOrMore!(pegged.peg.and!(delimRule, pegged.peg.option!(FunctionNextArgument!(delimChar))))), "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionFirstArgument, pegged.peg.oneOrMore!(pegged.peg.and!(delimRule, pegged.peg.option!(FunctionNextArgument!(delimChar))))), "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")"), "List_2")(TParseTree("", false,[], s));
        }
    }
    static string List(GetName g)
    {
        return "QMakeProject.List!(" ~ pegged.peg.getName!(delimRule)() ~ ", " ~ pegged.peg.getName!(delimChar) ~ ")";
    }

    }
    static TParseTree FunctionFirstArgument(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionFirstArgumentImpl, pegged.peg.zeroOrMore!(FunctionFirstArgumentImpl)), "QMakeProject.FunctionFirstArgument")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionFirstArgument`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionFirstArgumentImpl, pegged.peg.zeroOrMore!(FunctionFirstArgumentImpl)), "QMakeProject.FunctionFirstArgument"), "FunctionFirstArgument")(p);
                memo[tuple(`FunctionFirstArgument`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionFirstArgument(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionFirstArgumentImpl, pegged.peg.zeroOrMore!(FunctionFirstArgumentImpl)), "QMakeProject.FunctionFirstArgument")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionFirstArgumentImpl, pegged.peg.zeroOrMore!(FunctionFirstArgumentImpl)), "QMakeProject.FunctionFirstArgument"), "FunctionFirstArgument")(TParseTree("", false,[], s));
        }
    }
    static string FunctionFirstArgument(GetName g)
    {
        return "QMakeProject.FunctionFirstArgument";
    }

    static TParseTree FunctionFirstArgumentImpl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall, EnquotedString, FunctionFirstArgumentString), "QMakeProject.FunctionFirstArgumentImpl")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionFirstArgumentImpl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall, EnquotedString, FunctionFirstArgumentString), "QMakeProject.FunctionFirstArgumentImpl"), "FunctionFirstArgumentImpl")(p);
                memo[tuple(`FunctionFirstArgumentImpl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionFirstArgumentImpl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall, EnquotedString, FunctionFirstArgumentString), "QMakeProject.FunctionFirstArgumentImpl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall, EnquotedString, FunctionFirstArgumentString), "QMakeProject.FunctionFirstArgumentImpl"), "FunctionFirstArgumentImpl")(TParseTree("", false,[], s));
        }
    }
    static string FunctionFirstArgumentImpl(GetName g)
    {
        return "QMakeProject.FunctionFirstArgumentImpl";
    }

    static TParseTree FunctionFirstArgumentString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionFirstArgumentStringChar)), "QMakeProject.FunctionFirstArgumentString")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionFirstArgumentString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionFirstArgumentStringChar)), "QMakeProject.FunctionFirstArgumentString"), "FunctionFirstArgumentString")(p);
                memo[tuple(`FunctionFirstArgumentString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionFirstArgumentString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionFirstArgumentStringChar)), "QMakeProject.FunctionFirstArgumentString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionFirstArgumentStringChar)), "QMakeProject.FunctionFirstArgumentString"), "FunctionFirstArgumentString")(TParseTree("", false,[], s));
        }
    }
    static string FunctionFirstArgumentString(GetName g)
    {
        return "QMakeProject.FunctionFirstArgumentString";
    }

    static TParseTree FunctionFirstArgumentStringChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(eol, EXPAND_MARKER, space, COMMA, quote, doublequote, BACKSLASH, EndOfFunction)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionFirstArgumentStringChar")(p);
        }
        else
        {
            if (auto m = tuple(`FunctionFirstArgumentStringChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(eol, EXPAND_MARKER, space, COMMA, quote, doublequote, BACKSLASH, EndOfFunction)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionFirstArgumentStringChar"), "FunctionFirstArgumentStringChar")(p);
                memo[tuple(`FunctionFirstArgumentStringChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionFirstArgumentStringChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(eol, EXPAND_MARKER, space, COMMA, quote, doublequote, BACKSLASH, EndOfFunction)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionFirstArgumentStringChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(eol, EXPAND_MARKER, space, COMMA, quote, doublequote, BACKSLASH, EndOfFunction)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionFirstArgumentStringChar"), "FunctionFirstArgumentStringChar")(TParseTree("", false,[], s));
        }
    }
    static string FunctionFirstArgumentStringChar(GetName g)
    {
        return "QMakeProject.FunctionFirstArgumentStringChar";
    }

    template FunctionNextArgument(alias delim)
    {
    static TParseTree FunctionNextArgument(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionNextArgumentImpl!(delim), pegged.peg.zeroOrMore!(FunctionNextArgumentImpl!(delim))), "QMakeProject.FunctionNextArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionNextArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionNextArgumentImpl!(delim), pegged.peg.zeroOrMore!(FunctionNextArgumentImpl!(delim))), "QMakeProject.FunctionNextArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionNextArgument_1")(p);
                memo[tuple("FunctionNextArgument!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionNextArgument(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(FunctionNextArgumentImpl!(delim), pegged.peg.zeroOrMore!(FunctionNextArgumentImpl!(delim))), "QMakeProject.FunctionNextArgument!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(FunctionNextArgumentImpl!(delim), pegged.peg.zeroOrMore!(FunctionNextArgumentImpl!(delim))), "QMakeProject.FunctionNextArgument!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionNextArgument_1")(TParseTree("", false,[], s));
        }
    }
    static string FunctionNextArgument(GetName g)
    {
        return "QMakeProject.FunctionNextArgument!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template FunctionNextArgumentImpl(alias delim)
    {
    static TParseTree FunctionNextArgumentImpl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall, EnquotedString, FunctionNextArgumentString!(delim)), "QMakeProject.FunctionNextArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionNextArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall, EnquotedString, FunctionNextArgumentString!(delim)), "QMakeProject.FunctionNextArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionNextArgumentImpl_1")(p);
                memo[tuple("FunctionNextArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionNextArgumentImpl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall, EnquotedString, FunctionNextArgumentString!(delim)), "QMakeProject.FunctionNextArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ReplaceFunctionCall, ExpandStatement, TestFunctionCall, EnquotedString, FunctionNextArgumentString!(delim)), "QMakeProject.FunctionNextArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionNextArgumentImpl_1")(TParseTree("", false,[], s));
        }
    }
    static string FunctionNextArgumentImpl(GetName g)
    {
        return "QMakeProject.FunctionNextArgumentImpl!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template FunctionNextArgumentString(alias delim)
    {
    static TParseTree FunctionNextArgumentString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionNextArgumentStringChar!(delim))), "QMakeProject.FunctionNextArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionNextArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionNextArgumentStringChar!(delim))), "QMakeProject.FunctionNextArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionNextArgumentString_1")(p);
                memo[tuple("FunctionNextArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionNextArgumentString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionNextArgumentStringChar!(delim))), "QMakeProject.FunctionNextArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(FunctionNextArgumentStringChar!(delim))), "QMakeProject.FunctionNextArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionNextArgumentString_1")(TParseTree("", false,[], s));
        }
    }
    static string FunctionNextArgumentString(GetName g)
    {
        return "QMakeProject.FunctionNextArgumentString!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    template FunctionNextArgumentStringChar(alias delim)
    {
    static TParseTree FunctionNextArgumentStringChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(eol, EXPAND_MARKER, delim, quote, doublequote, BACKSLASH, EndOfFunction)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionNextArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")")(p);
        }
        else
        {
            if (auto m = tuple("FunctionNextArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")", p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(eol, EXPAND_MARKER, delim, quote, doublequote, BACKSLASH, EndOfFunction)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionNextArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionNextArgumentStringChar_1")(p);
                memo[tuple("FunctionNextArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")", p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree FunctionNextArgumentStringChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(eol, EXPAND_MARKER, delim, quote, doublequote, BACKSLASH, EndOfFunction)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionNextArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(eol, EXPAND_MARKER, delim, quote, doublequote, BACKSLASH, EndOfFunction)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.FunctionNextArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")"), "FunctionNextArgumentStringChar_1")(TParseTree("", false,[], s));
        }
    }
    static string FunctionNextArgumentStringChar(GetName g)
    {
        return "QMakeProject.FunctionNextArgumentStringChar!(" ~ pegged.peg.getName!(delim) ~ ")";
    }

    }
    static TParseTree EndOfFunction(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"), pegged.peg.literal!("/")), eoi, eol, pegged.peg.literal!("="), pegged.peg.literal!("+="), pegged.peg.literal!("*="), pegged.peg.literal!("-="), pegged.peg.literal!("~="), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("_"), pegged.peg.literal!("("), pegged.peg.literal!(")"), EXPAND_MARKER, pegged.peg.literal!("@"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!(":"), pegged.peg.literal!("|"))), "QMakeProject.EndOfFunction")(p);
        }
        else
        {
            if (auto m = tuple(`EndOfFunction`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"), pegged.peg.literal!("/")), eoi, eol, pegged.peg.literal!("="), pegged.peg.literal!("+="), pegged.peg.literal!("*="), pegged.peg.literal!("-="), pegged.peg.literal!("~="), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("_"), pegged.peg.literal!("("), pegged.peg.literal!(")"), EXPAND_MARKER, pegged.peg.literal!("@"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!(":"), pegged.peg.literal!("|"))), "QMakeProject.EndOfFunction"), "EndOfFunction")(p);
                memo[tuple(`EndOfFunction`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EndOfFunction(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"), pegged.peg.literal!("/")), eoi, eol, pegged.peg.literal!("="), pegged.peg.literal!("+="), pegged.peg.literal!("*="), pegged.peg.literal!("-="), pegged.peg.literal!("~="), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("_"), pegged.peg.literal!("("), pegged.peg.literal!(")"), EXPAND_MARKER, pegged.peg.literal!("@"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!(":"), pegged.peg.literal!("|"))), "QMakeProject.EndOfFunction")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!(")"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*"), pegged.peg.literal!("/")), eoi, eol, pegged.peg.literal!("="), pegged.peg.literal!("+="), pegged.peg.literal!("*="), pegged.peg.literal!("-="), pegged.peg.literal!("~="), pegged.peg.literal!(","), pegged.peg.literal!("."), pegged.peg.literal!("_"), pegged.peg.literal!("("), pegged.peg.literal!(")"), EXPAND_MARKER, pegged.peg.literal!("@"), pegged.peg.literal!("{"), pegged.peg.literal!("}"), pegged.peg.literal!(":"), pegged.peg.literal!("|"))), "QMakeProject.EndOfFunction"), "EndOfFunction")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineReplace"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), Block), "QMakeProject.ReplaceFunctionDeclaration")(p);
        }
        else
        {
            if (auto m = tuple(`ReplaceFunctionDeclaration`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineReplace"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), Block), "QMakeProject.ReplaceFunctionDeclaration"), "ReplaceFunctionDeclaration")(p);
                memo[tuple(`ReplaceFunctionDeclaration`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReplaceFunctionDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineReplace"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), Block), "QMakeProject.ReplaceFunctionDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineReplace"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), Block), "QMakeProject.ReplaceFunctionDeclaration"), "ReplaceFunctionDeclaration")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineTest"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), Block), "QMakeProject.TestFunctionDeclaration")(p);
        }
        else
        {
            if (auto m = tuple(`TestFunctionDeclaration`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineTest"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), Block), "QMakeProject.TestFunctionDeclaration"), "TestFunctionDeclaration")(p);
                memo[tuple(`TestFunctionDeclaration`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TestFunctionDeclaration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineTest"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), Block), "QMakeProject.TestFunctionDeclaration")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("defineTest"), OPEN_PAR_WS, pegged.peg.option!(FunctionArgumentList), CLOSE_PAR_WS, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.discard!(pegged.peg.zeroOrMore!(eol)), Block), "QMakeProject.TestFunctionDeclaration"), "TestFunctionDeclaration")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(QualifiedIdentifier), pegged.peg.option!(pegged.peg.and!(COMMA_WS, CacheTestFunctionParam2)), pegged.peg.option!(pegged.peg.and!(COMMA_WS, QualifiedIdentifier))), "QMakeProject.CacheTestFunctionCallParams")(p);
        }
        else
        {
            if (auto m = tuple(`CacheTestFunctionCallParams`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(QualifiedIdentifier), pegged.peg.option!(pegged.peg.and!(COMMA_WS, CacheTestFunctionParam2)), pegged.peg.option!(pegged.peg.and!(COMMA_WS, QualifiedIdentifier))), "QMakeProject.CacheTestFunctionCallParams"), "CacheTestFunctionCallParams")(p);
                memo[tuple(`CacheTestFunctionCallParams`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree CacheTestFunctionCallParams(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(QualifiedIdentifier), pegged.peg.option!(pegged.peg.and!(COMMA_WS, CacheTestFunctionParam2)), pegged.peg.option!(pegged.peg.and!(COMMA_WS, QualifiedIdentifier))), "QMakeProject.CacheTestFunctionCallParams")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.option!(QualifiedIdentifier), pegged.peg.option!(pegged.peg.and!(COMMA_WS, CacheTestFunctionParam2)), pegged.peg.option!(pegged.peg.and!(COMMA_WS, QualifiedIdentifier))), "QMakeProject.CacheTestFunctionCallParams"), "CacheTestFunctionCallParams")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("return"), OPEN_PAR_WS, pegged.peg.option!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionFirstArgument, Statement)), CLOSE_PAR_WS), "QMakeProject.ReturnFunctionCall")(p);
        }
        else
        {
            if (auto m = tuple(`ReturnFunctionCall`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("return"), OPEN_PAR_WS, pegged.peg.option!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionFirstArgument, Statement)), CLOSE_PAR_WS), "QMakeProject.ReturnFunctionCall"), "ReturnFunctionCall")(p);
                memo[tuple(`ReturnFunctionCall`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ReturnFunctionCall(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("return"), OPEN_PAR_WS, pegged.peg.option!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionFirstArgument, Statement)), CLOSE_PAR_WS), "QMakeProject.ReturnFunctionCall")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("return"), OPEN_PAR_WS, pegged.peg.option!(pegged.peg.or!(List!(pegged.peg.discard!(pegged.peg.oneOrMore!(space)), pegged.peg.discard!(space)), FunctionFirstArgument, Statement)), CLOSE_PAR_WS), "QMakeProject.ReturnFunctionCall"), "ReturnFunctionCall")(TParseTree("", false,[], s));
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

    static TParseTree RegularString(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.option!(RegularStringChars), "QMakeProject.RegularString")(p);
        }
        else
        {
            if (auto m = tuple(`RegularString`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.option!(RegularStringChars), "QMakeProject.RegularString"), "RegularString")(p);
                memo[tuple(`RegularString`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RegularString(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.option!(RegularStringChars), "QMakeProject.RegularString")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.option!(RegularStringChars), "QMakeProject.RegularString"), "RegularString")(TParseTree("", false,[], s));
        }
    }
    static string RegularString(GetName g)
    {
        return "QMakeProject.RegularString";
    }

    static TParseTree RegularStringChars(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(RegularStringChar)), "QMakeProject.RegularStringChars")(p);
        }
        else
        {
            if (auto m = tuple(`RegularStringChars`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(RegularStringChar)), "QMakeProject.RegularStringChars"), "RegularStringChars")(p);
                memo[tuple(`RegularStringChars`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RegularStringChars(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(RegularStringChar)), "QMakeProject.RegularStringChars")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(RegularStringChar)), "QMakeProject.RegularStringChars"), "RegularStringChars")(TParseTree("", false,[], s));
        }
    }
    static string RegularStringChars(GetName g)
    {
        return "QMakeProject.RegularStringChars";
    }

    static TParseTree RegularStringChar(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(blank, quote, doublequote)), SourceCharacter), "QMakeProject.RegularStringChar")(p);
        }
        else
        {
            if (auto m = tuple(`RegularStringChar`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(blank, quote, doublequote)), SourceCharacter), "QMakeProject.RegularStringChar"), "RegularStringChar")(p);
                memo[tuple(`RegularStringChar`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree RegularStringChar(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(blank, quote, doublequote)), SourceCharacter), "QMakeProject.RegularStringChar")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(blank, quote, doublequote)), SourceCharacter), "QMakeProject.RegularStringChar"), "RegularStringChar")(TParseTree("", false,[], s));
        }
    }
    static string RegularStringChar(GetName g)
    {
        return "QMakeProject.RegularStringChar";
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
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(doublequote, BACKSLASH)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonDoubleQuoteCharacter")(p);
        }
        else
        {
            if (auto m = tuple(`NonDoubleQuoteCharacter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(doublequote, BACKSLASH)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonDoubleQuoteCharacter"), "NonDoubleQuoteCharacter")(p);
                memo[tuple(`NonDoubleQuoteCharacter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonDoubleQuoteCharacter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(doublequote, BACKSLASH)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonDoubleQuoteCharacter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(doublequote, BACKSLASH)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonDoubleQuoteCharacter"), "NonDoubleQuoteCharacter")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(quote, BACKSLASH)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonSingleQuoteCharacter")(p);
        }
        else
        {
            if (auto m = tuple(`NonSingleQuoteCharacter`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(quote, BACKSLASH)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonSingleQuoteCharacter"), "NonSingleQuoteCharacter")(p);
                memo[tuple(`NonSingleQuoteCharacter`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree NonSingleQuoteCharacter(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(quote, BACKSLASH)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonSingleQuoteCharacter")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.or!(quote, BACKSLASH)), SourceCharacter), pegged.peg.and!(BACKSLASH, EscapeSequence)), "QMakeProject.NonSingleQuoteCharacter"), "NonSingleQuoteCharacter")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*")))), "QMakeProject.QMakeIdentifier")(p);
        }
        else
        {
            if (auto m = tuple(`QMakeIdentifier`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*")))), "QMakeProject.QMakeIdentifier"), "QMakeIdentifier")(p);
                memo[tuple(`QMakeIdentifier`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree QMakeIdentifier(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*")))), "QMakeProject.QMakeIdentifier")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("_"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("-"), pegged.peg.literal!(`\`), pegged.peg.literal!("+"), pegged.peg.literal!(`\`), pegged.peg.literal!("*")))), "QMakeProject.QMakeIdentifier"), "QMakeIdentifier")(TParseTree("", false,[], s));
        }
    }
    static string QMakeIdentifier(GetName g)
    {
        return "QMakeProject.QMakeIdentifier";
    }

    static TParseTree ExpandStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ProjectVariableExpandStatement, EnvironmentVariableExpandStatement, PropertyVariableExpandStatement), "QMakeProject.ExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ProjectVariableExpandStatement, EnvironmentVariableExpandStatement, PropertyVariableExpandStatement), "QMakeProject.ExpandStatement"), "ExpandStatement")(p);
                memo[tuple(`ExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ProjectVariableExpandStatement, EnvironmentVariableExpandStatement, PropertyVariableExpandStatement), "QMakeProject.ExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ProjectVariableExpandStatement, EnvironmentVariableExpandStatement, PropertyVariableExpandStatement), "QMakeProject.ExpandStatement"), "ExpandStatement")(TParseTree("", false,[], s));
        }
    }
    static string ExpandStatement(GetName g)
    {
        return "QMakeProject.ExpandStatement";
    }

    static TParseTree ProjectVariableExpandStatement(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier), pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("}")), pegged.peg.and!(EXPAND_MARKER, doublequote, ExpandStatement, doublequote)), "QMakeProject.ProjectVariableExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`ProjectVariableExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier), pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("}")), pegged.peg.and!(EXPAND_MARKER, doublequote, ExpandStatement, doublequote)), "QMakeProject.ProjectVariableExpandStatement"), "ProjectVariableExpandStatement")(p);
                memo[tuple(`ProjectVariableExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree ProjectVariableExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier), pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("}")), pegged.peg.and!(EXPAND_MARKER, doublequote, ExpandStatement, doublequote)), "QMakeProject.ProjectVariableExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier), pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("{"), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("}")), pegged.peg.and!(EXPAND_MARKER, doublequote, ExpandStatement, doublequote)), "QMakeProject.ProjectVariableExpandStatement"), "ProjectVariableExpandStatement")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(EXPAND_MARKER, SINGLE_EXPAND_MARKER), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.EnvironmentVariableExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`EnvironmentVariableExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(EXPAND_MARKER, SINGLE_EXPAND_MARKER), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.EnvironmentVariableExpandStatement"), "EnvironmentVariableExpandStatement")(p);
                memo[tuple(`EnvironmentVariableExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EnvironmentVariableExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(EXPAND_MARKER, SINGLE_EXPAND_MARKER), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.EnvironmentVariableExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.or!(EXPAND_MARKER, SINGLE_EXPAND_MARKER), OPEN_PAR_WS, QualifiedIdentifier, CLOSE_PAR_WS), "QMakeProject.EnvironmentVariableExpandStatement"), "EnvironmentVariableExpandStatement")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("["), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.option!(pegged.peg.keywords!("/get", "/src")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("]")), "QMakeProject.PropertyVariableExpandStatement")(p);
        }
        else
        {
            if (auto m = tuple(`PropertyVariableExpandStatement`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("["), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.option!(pegged.peg.keywords!("/get", "/src")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("]")), "QMakeProject.PropertyVariableExpandStatement"), "PropertyVariableExpandStatement")(p);
                memo[tuple(`PropertyVariableExpandStatement`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree PropertyVariableExpandStatement(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("["), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.option!(pegged.peg.keywords!("/get", "/src")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("]")), "QMakeProject.PropertyVariableExpandStatement")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(EXPAND_MARKER, pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("["), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), QualifiedIdentifier, pegged.peg.option!(pegged.peg.keywords!("/get", "/src")), pegged.peg.discard!(pegged.peg.zeroOrMore!(space)), pegged.peg.literal!("]")), "QMakeProject.PropertyVariableExpandStatement"), "PropertyVariableExpandStatement")(TParseTree("", false,[], s));
        }
    }
    static string PropertyVariableExpandStatement(GetName g)
    {
        return "QMakeProject.PropertyVariableExpandStatement";
    }

    static TParseTree LValueImpl(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ExpandStatement, QMakeIdentifier), "QMakeProject.LValueImpl")(p);
        }
        else
        {
            if (auto m = tuple(`LValueImpl`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(ExpandStatement, QMakeIdentifier), "QMakeProject.LValueImpl"), "LValueImpl")(p);
                memo[tuple(`LValueImpl`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LValueImpl(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(ExpandStatement, QMakeIdentifier), "QMakeProject.LValueImpl")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(ExpandStatement, QMakeIdentifier), "QMakeProject.LValueImpl"), "LValueImpl")(TParseTree("", false,[], s));
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
            return         pegged.peg.defined!(pegged.peg.or!(quote, doublequote, BACKSLASH, pegged.peg.literal!("$"), pegged.peg.literal!("."), pegged.peg.literal!("?"), pegged.peg.literal!("a"), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit, HexDigit, HexDigit))), pegged.peg.fuse!(pegged.peg.and!(OctDigit, OctDigit, OctDigit)), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit))), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"), pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("{"), pegged.peg.literal!("}")), "QMakeProject.EscapeSequence")(p);
        }
        else
        {
            if (auto m = tuple(`EscapeSequence`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(quote, doublequote, BACKSLASH, pegged.peg.literal!("$"), pegged.peg.literal!("."), pegged.peg.literal!("?"), pegged.peg.literal!("a"), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit, HexDigit, HexDigit))), pegged.peg.fuse!(pegged.peg.and!(OctDigit, OctDigit, OctDigit)), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit))), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"), pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("{"), pegged.peg.literal!("}")), "QMakeProject.EscapeSequence"), "EscapeSequence")(p);
                memo[tuple(`EscapeSequence`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EscapeSequence(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(quote, doublequote, BACKSLASH, pegged.peg.literal!("$"), pegged.peg.literal!("."), pegged.peg.literal!("?"), pegged.peg.literal!("a"), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit, HexDigit, HexDigit))), pegged.peg.fuse!(pegged.peg.and!(OctDigit, OctDigit, OctDigit)), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit))), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"), pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("{"), pegged.peg.literal!("}")), "QMakeProject.EscapeSequence")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(quote, doublequote, BACKSLASH, pegged.peg.literal!("$"), pegged.peg.literal!("."), pegged.peg.literal!("?"), pegged.peg.literal!("a"), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit, HexDigit, HexDigit))), pegged.peg.fuse!(pegged.peg.and!(OctDigit, OctDigit, OctDigit)), pegged.peg.and!(pegged.peg.literal!("x"), pegged.peg.fuse!(pegged.peg.and!(HexDigit, HexDigit))), pegged.peg.literal!("b"), pegged.peg.literal!("f"), pegged.peg.literal!("n"), pegged.peg.literal!("r"), pegged.peg.literal!("t"), pegged.peg.literal!("v"), pegged.peg.literal!("+"), pegged.peg.literal!("-"), pegged.peg.literal!("*"), pegged.peg.literal!("("), pegged.peg.literal!(")"), pegged.peg.literal!("["), pegged.peg.literal!("]"), pegged.peg.literal!("{"), pegged.peg.literal!("}")), "QMakeProject.EscapeSequence"), "EscapeSequence")(TParseTree("", false,[], s));
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

    static TParseTree SINGLE_EXPAND_MARKER(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("$"), "QMakeProject.SINGLE_EXPAND_MARKER")(p);
        }
        else
        {
            if (auto m = tuple(`SINGLE_EXPAND_MARKER`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("$"), "QMakeProject.SINGLE_EXPAND_MARKER"), "SINGLE_EXPAND_MARKER")(p);
                memo[tuple(`SINGLE_EXPAND_MARKER`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree SINGLE_EXPAND_MARKER(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("$"), "QMakeProject.SINGLE_EXPAND_MARKER")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("$"), "QMakeProject.SINGLE_EXPAND_MARKER"), "SINGLE_EXPAND_MARKER")(TParseTree("", false,[], s));
        }
    }
    static string SINGLE_EXPAND_MARKER(GetName g)
    {
        return "QMakeProject.SINGLE_EXPAND_MARKER";
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
            return         pegged.peg.defined!(pegged.peg.keywords!("\u000A", "\u000D", "\u2028", "\u2029"), "QMakeProject.LineTerminator")(p);
        }
        else
        {
            if (auto m = tuple(`LineTerminator`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("\u000A", "\u000D", "\u2028", "\u2029"), "QMakeProject.LineTerminator"), "LineTerminator")(p);
                memo[tuple(`LineTerminator`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LineTerminator(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("\u000A", "\u000D", "\u2028", "\u2029"), "QMakeProject.LineTerminator")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("\u000A", "\u000D", "\u2028", "\u2029"), "QMakeProject.LineTerminator"), "LineTerminator")(TParseTree("", false,[], s));
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

