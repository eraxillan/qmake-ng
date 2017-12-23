module qmakegrammar;

enum QMakeGrammar = `
    QMakeProject:
        Project <- Statement+ eoi
        Statement <- Scope / Block / TestFunctionCall / Assignment / Comment / EmptyStatement

        # No input
        EmptyStatement <- eps eol*

        # Code block
        Block           <- SingleLineBlock / MultiLineBlock
        SingleLineBlock <- :space* "@" :space* Statement
        MultiLineBlock  <- :space* "{" :space* :eol+ :space* Statement+ :space* :eol* "}" :space* :eol*

        # Comment, single-line and multi-line (extension)
        Comment                         <~ MultiLineComment / SingleLineComment
        MultiLineComment                <~ :"#*" (!"*#" SourceCharacter)* :"*#"
        SingleLineComment               <- :"#" SingleLineCommentChars? eol
        SingleLineCommentChars          <- SingleLineCommentChar+
        SingleLineCommentChar           <- !LineTerminator SourceCharacter

        # Variable or variable property assignment
        Assignment <- (DirectAssignment / AppendAssignment / AppendUniqueAssignment / RemoveAssignment / ReplaceAssignment)
 
        StringList              <- String (:space* String)*
        String                  <- StringChars?
        StringChars             <- ~(StringChar+)
        #StringChar              <- !(blank / LineTerminator / "#" / "," / "(" / ")" / quote / doublequote ) SourceCharacter
        StringChar              <- !(blank / LineTerminator) SourceCharacter
        
        DirectAssignment        <- QualifiedIdentifier :space* "="  :space* StringList? :eol*
        AppendAssignment        <- QualifiedIdentifier :space* "+=" :space* StringList? :eol*
        AppendUniqueAssignment  <- QualifiedIdentifier :space* "*=" :space* StringList? :eol*
        RemoveAssignment        <- QualifiedIdentifier :space* "-=" :space* StringList? :eol*
        ReplaceAssignment       <- QualifiedIdentifier :space* "~=" :space* StringList? :eol*

        # Test function call
        # E.g.:
        # message("Starting project build...")
        TestFunctionCall <- FunctionCall

        # Replace function call (must be rvalue only)
        # E.g.:
        # $$escape_expand("One\nTwo\nThree")
        ReplaceFunctionCall <- "$$" FunctionCall
        
        FunctionCall <- Identifier :space* "(" :space* FunctionArgumentList? :space* ")" :space* :eol*
        FunctionArgumentList    <- WhitespaceSeparatedList / CommaSeparatedList / FunctionArgumentString
        CommaSeparatedList      <- FunctionArgumentString (:space* "," :space* FunctionArgumentString)+
        WhitespaceSeparatedList <- FunctionArgumentString (:space+             FunctionArgumentString)+
        FunctionArgumentString  <- ReplaceFunctionCall / TestFunctionCall / EnquotedString / RegularFunctionArgumentString        
        RegularFunctionArgumentString <- ~(RegularFunctionArgumentStringChar+)
        RegularFunctionArgumentStringChar <- !(space / "," / quote / doublequote / EndOfFunction) SourceCharacter

        # NOTE: function arguments can contain "("/")" themselves, so we need special rule to detect function argument list end
        EndOfFunction <- ")" :space* (eoi / eol / "," / "(" / ")" / "$$" / "@" / "{" / ":" / "|")

        # Conditional statement
        # E.g.:
        # CONFIG(debug, debug|release):buildmode = debug
        Scope            <- BooleanExpression ScopeTrueBranch ScopeFalseBranch*
        ScopeTrueBranch  <- Block
        ScopeFalseBranch <- "else" :space* Block

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
        IfTestFunctionCall           <- "if" :space* "(" :space* BooleanExpression :space* ")"
        BooleanAtom                  <- Identifier | ExpandStatement | TestFunctionCall | BooleanConst
        BooleanConst                 <- "true" | "false"


        Expression <- EmptyStatement | RawString
        
        ExpandStatement <- "$$" :space* QualifiedIdentifier / "$$" :space* "{" :space* QualifiedIdentifier :space* "}"

        # Raw string: can contain any character except of EOL
        RawString       <- RawStringChars?
        RawStringChars  <- ~(RawStringChar+)
        RawStringChar   <- !LineTerminator SourceCharacter

        # Regular string: can contain any character except of spacing/EOL/quotes
        RegularString       <- RegularStringChars?
        RegularStringChars  <- ~(RegularStringChar+)
        RegularStringChar   <- !(blank / quote / doublequote) SourceCharacter

        # Enquoted string: can contain any character except of quote
        EnquotedString            <- DoubleEnquotedString / SingleEnquotedString
        DoubleEnquotedString      <- doublequote ~(NonDoubleQuoteCharacter*) doublequote
        NonDoubleQuoteCharacter   <- !doublequote .
        SingleEnquotedString      <- quote ~(NonSingleQuoteCharacter*) quote
        NonSingleQuoteCharacter   <- !quote .

        #Identifier <- identifier
        Identifier  <~ [a-zA-Z_] [a-zA-Z_0-9\-]*
        QMakeIdentifier <~ [a-zA-Z_0-9]*
        QualifiedIdentifier <~ (Identifier / ExpandStatement) ('.' (QMakeIdentifier / ExpandStatement))*

        SourceCharacter <- [\u0000-\uFFFC]
        LineTerminator  <- "\u000A" / "\u000D" / "\u2028" / "\u2029"
    `;

