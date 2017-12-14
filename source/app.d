import std.typecons : No;

import std.algorithm;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;

import pegged.grammar;

static string preprocessLines(string[] strLinesArray)
{
    const auto STR_HASH = "#";
    const auto STR_BACKSLASH = "\\";
    
    string[] result;
    for (int lineIndex = 0; lineIndex < strLinesArray.length; lineIndex++)
    {
        auto strLine = strLinesArray[lineIndex];
        strLine = strip(strLine);

        auto hashIndex = indexOf(strLine, STR_HASH);
        if (hashIndex >= 0)
        {
            if (hashIndex == 0)
                writeln("ProParser::preprocessLines: skip comment line");
            else
                writeln("ProParser::preprocessLines: cutting off inline comment");

            strLine = strLine[0 .. hashIndex];
            strLine = strip(strLine);
        }

        bool isMultiLine = false;
        int startLineIndex = -1;
        int endLineIndex = -1;
        if (strLine.endsWith(STR_BACKSLASH))
        {
            isMultiLine = true;

            auto strMultiLine = strip(strLine[0 .. strLine.length - 1]);
            auto j = lineIndex + 1;
            for ( ; j < strLinesArray.length; j++)
            {
                strLine = strLinesArray[j];
                strLine = strip(strLine);

                bool endsWithBackslash = strLine.endsWith("\\");
                strMultiLine ~= endsWithBackslash ? " " ~ strip(strLine[0 .. strLine.length - 1]) : " " ~ strLine;

                if (!endsWithBackslash)
                    break;
            }

            startLineIndex = lineIndex;
            endLineIndex = j;
            lineIndex = j;
            strLine = strMultiLine;
        }
        
        // FIXME: workaround for grammar ambiguity - cannot distingush AND-colon and scope statement expression end colon
        // Replace last colon (":") with "@";
        // Exceptions:
        // - there is an "{" on this line
        // - (TODO) the colon is inside quotes/doublequotes
        auto lastColonIndex = strLine.lastIndexOf(':');
        if ((lastColonIndex != -1) && !strLine.endsWith("{"))
        {
            strLine.replaceInPlace(lastColonIndex, lastColonIndex + 1, "@");
        }
        // Replace "else:" with "else@"
        // FIXME: cover all cases!
        strLine = strLine.replace("else:", "else@");

/*        if (isMultiLine)
            writeln("Multi-line " ~ std.conv.to!string(startLineIndex + 1) ~ " - " ~ std.conv.to!string(endLineIndex + 1) ~ ": |" ~ strLine ~ "|");
        else
            writeln("Line " ~ std.conv.to!string(lineIndex + 1) ~ ": |" ~ strLine ~ "|");*/

//        if (!strLine.empty())
            result ~= strLine;
    }

    return result.join("\n");
}

bool tryParseProject(string fileName)
{
    mixin(grammar(`
    QMakeProject:
        Project <- Statement+ eoi
        Statement <- Block | Comment | Assignment | TestFunctionCall | Scope | EmptyStatement

        # No input
        EmptyStatement <- eps eol*

        # Code block
        Block           <- SingleLineBlock / MultiLineBlock
        SingleLineBlock <- :space* "@" :space* Statement
        MultiLineBlock  <- :space* "{" :space* :eol+ :space* (Statement)* :space* :eol* "}" :space* :eol*

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
        TestFunctionCall < Identifier "(" FunctionArgumentList? ")"
        FunctionArgumentList < FunctionArgumentString ((:"," FunctionArgumentString)* | (:blank* FunctionArgumentString)*)
        FunctionArgumentString <- EnquotedString | RegularFunctionArgumentString | ReplaceFunctionCall | TestFunctionCall
        RegularFunctionArgumentString <- ~(RegularFunctionArgumentStringChar+)
        RegularFunctionArgumentStringChar <- !(blank / "," / ")" / quote / doublequote ) SourceCharacter

# FIXME: function arguments can contain "("/")" themselves, so we need special rule to detect function argument list end
        EndOfFunction <- ")" :space* (eol* | "@" | "{" | ":" | "|")

        # Replace function call (must be rvalue only)
        # E.g.:
        # $$escape_expand("One\nTwo\nThree")
        ReplaceFunctionCall < "$$" Identifier "(" FunctionArgumentList? ")"
        
        # Conditional statement
        # E.g.:
        # CONFIG(debug, debug|release):buildmode = debug
        Scope            <- BooleanExpression ScopeTrueBranch ScopeFalseBranch*
        ScopeTrueBranch  <- Block
        ScopeFalseBranch <- "else" :space* Block
#        ScopeFalseBranch <- ("else" :space* "@" :space* Statement) | ("else" :space* Block)

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

        Identifier <- identifier
        # Identifier  <~ [a-zA-Z_] [a-zA-Z_0-9]*
        QMakeIdentifier <~ [a-zA-Z_0-9]*
        QualifiedIdentifier <~ (Identifier / ExpandStatement) ('.' (QMakeIdentifier / ExpandStatement))*

        SourceCharacter <- [\u0000-\uFFFC]
        LineTerminator  <- "\u000A" / "\u000D" / "\u2028" / "\u2029"
    `));
    
    auto proFileContents = std.file.readText(fileName);
    proFileContents = preprocessLines(splitLines(proFileContents));
        
    auto parseTree = QMakeProject(proFileContents);
    if (!parseTree.successful)
    {
        writeln("Parsing failed:");
        writeln(parseTree);
    }
    return parseTree.successful;
}

unittest
{
    assert(tryParseProject("tests/linux-clang-qmake.conf"));
}

void main()
{
/*    if (!tryParseProject("tests/android-base-head.conf"))
        return;
    writeln("Test tests/android-base-head.conf passed\n\n");
    if (!tryParseProject("tests/scope_1.pro"))
        return;
    writeln("Test tests/scope_1.pro passed\n\n");
    if (!tryParseProject("tests/scope_2.pro"))
        return;
    writeln("Test tests/scope_2.pro passed\n\n");
    if (!tryParseProject("tests/scope_3.pro"))
        return;
    writeln("Test tests/scope_3.pro passed\n\n");
    if (!tryParseProject("tests/qml_module.prf"))
        return;
    writeln("Test tests/qml_module.prf passed\n\n"); */

    // Runtime parsing
    // Iterate over all *.d files in current directory ("") and all its subdirectories
    auto projectFiles = dirEntries("/home/eraxillan/Qt/5.9.1/gcc_64/mkspecs", SpanMode.depth).filter!(
        f => f.name.endsWith(".pro") || f.name.endsWith(".pri") || f.name.endsWith(".prf") || f.name.endsWith(".conf")
    );
    int successfulCount = 0, failedCount = 0;
    foreach (d; projectFiles)
    {
        writeln("\n");
        writeln(d.name);

        if (tryParseProject(d.name))
        {
            successfulCount++;
            writeln("SUCCESS");
        } else failedCount++;
    }
    
    int totalCount = successfulCount + failedCount;
    writeln("Successfully parsed: " ~ std.conv.to!string(successfulCount));
    writeln("Failed to parse: " ~ std.conv.to!string(failedCount));
}

