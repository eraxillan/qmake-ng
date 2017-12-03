import std.typecons : No;

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
        // Replace last colon (":") with "@"
        auto lastColonIndex = strLine.lastIndexOf(':');
        if (lastColonIndex != -1)
        {
            strLine.replaceInPlace(lastColonIndex, lastColonIndex + 1, "@");
        }

        if (isMultiLine)
            writeln("Multi-line " ~ std.conv.to!string(startLineIndex + 1) ~ " - " ~ std.conv.to!string(endLineIndex + 1) ~ ": |" ~ strLine ~ "|");
        else
            writeln("Line " ~ std.conv.to!string(lineIndex + 1) ~ ": |" ~ strLine ~ "|");

        result ~= strLine;
    }

    return result.join("\n");
}

void main()
{
    mixin(grammar(`
    QMakeProject:
        Project <- Statement+ eoi
        Statement <- Block | Comment | Assignment | FunctionCall | Scope | EmptyStatement

        # No input
        EmptyStatement <- eps

        # Code block
        Block <- :space* "{" :space* :eol+ :space* (Statement)+ :space* "}" :space* :eol*

        # Comment, single-line and multi-line (extension)
        Comment                         <~ MultiLineComment / SingleLineComment
        MultiLineComment                <~ :"#*" (!"*#" SourceCharacter)* :"*#"
        SingleLineComment               <- :"#" SingleLineCommentChars? eol
        SingleLineCommentChars          <- SingleLineCommentChar+
        SingleLineCommentChar           <- !LineTerminator SourceCharacter

        # Variable assignment
        Assignment <- (DirectAssignment / AppendAssignment / AppendUniqueAssignment / RemoveAssignment / ReplaceAssignment) eol?
        DirectAssignment        < Identifier "=" Expression
        AppendAssignment        < Identifier "+=" Expression
        AppendUniqueAssignment  < Identifier "*=" Expression
        RemoveAssignment        < Identifier "-=" Expression
        ReplaceAssignment       < Identifier "~=" Expression

        # Test function call
        # E.g.:
        # message("Starting project build...")
        FunctionCall < Identifier "(" FunctionArgumentList? ")"
        FunctionArgumentList < FunctionArgumentString (:"," FunctionArgumentString)*
        FunctionArgumentString <- EnquotedString | RegularFunctionArgumentString
        RegularFunctionArgumentString <- ~(RegularFunctionArgumentStringChar+)
        RegularFunctionArgumentStringChar <- !(blank / "," / ")" / quote / doublequote ) SourceCharacter
        
        # Conditional statement
        # E.g.:
        # CONFIG(debug, debug|release):buildmode = debug
        Scope       < BooleanExpression ("@" Statement | Block) ElsePart*
        ElsePart    < "else@" Statement | "else" Block

        # E.g.:
        # var1: message("scope 1.1")
        # !exists($$QMAKE_QT_CONFIG)|!include($$QMAKE_QT_CONFIG, "", true) {
        BooleanExpression           <- LogicalORExpression
        LogicalORExpression         <- LogicalANDExpression (:space* "|" :space* LogicalANDExpression)*
        LogicalANDExpression        <- LogicalNOTExpression (:space* ":" :space* LogicalNOTExpression)*
        LogicalNOTExpression        <- (:space* "!" :space*)? PrimaryBooleanExpression
        PrimaryBooleanExpression    <- '(' BooleanExpression ')'
                                     / BooleanAtom

        BooleanAtom                 <- Identifier | FunctionCall | BooleanConst
        BooleanConst                <- "true" | "false"


        Expression <- RawString

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

        SourceCharacter <- [\u0000-\uFFFC]
        LineTerminator  <- "\u000A" / "\u000D" / "\u2028" / "\u2029"
    `));

    // FIXME: add readFromFile function

    // Runtime parsing
    writeln("Reading project file...");
    auto proFileContents = std.file.readText("/home/eraxillan/Qt/5.9.1/gcc_64/mkspecs/features/qt_config.prf");
//    auto proFileContents = std.file.readText("/home/eraxillan/Projects/bankirureader_cmake/rubankireader.pro");
    proFileContents = preprocessLines(splitLines(proFileContents));
    
    auto parseTree = QMakeProject(proFileContents);
    if (parseTree.successful)
        writeln("Parse tree '" ~ parseTree.name ~ "':\n");
    else
        writeln("Project parsing failed: ");

    writeln(parseTree);
}

