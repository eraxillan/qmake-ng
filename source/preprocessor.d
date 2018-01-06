module preprocessor;

import std.typecons : No;

import std.algorithm;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;
import std.range;

const auto STR_HASH = '#';
const auto STR_DQUOTE = '"';
const auto STR_OPEN_CURLY_BRACE = '{';
const auto STR_COLON = ':';
const auto STR_DOG = '@';
const auto STR_BACKSLASH = '\\';

struct QuotesInfo
{
    int indexOpen;
    int indexClose;
    bool success;
};

static QuotesInfo detectPairedCharacter(char openChar, char closeChar, string strLine, string subStr, int index)
{    
    int[] stack;
    
    // Search for opening double quote: skip paired DQs until we find unpaired one
    for (auto i = index - 1; i >= 0; i--)
    {
        if (strLine[i] == openChar)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
        return QuotesInfo(-1, -1, false);
    auto indexOpen = stack[$ - 1];
    stack = [];

    // Search for closing double quote (unpaired)
    for (auto i = index + 1; i < strLine.length; i++)
    {
        if (strLine[i] == closeChar)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
        return QuotesInfo(-1, -1, false);
    auto indexClose = stack[0];
    stack = [];

    return QuotesInfo(indexOpen, indexClose, true);
}

static string cutInlineComment(string strLine)
{
    bool commentFound;
    return cutInlineComment(strLine, commentFound);
}

static string cutInlineComment(string strLine, ref bool commentFound)
{
    string result = strLine;

    auto hashIndex = indexOf(strLine, STR_HASH);
    if (hashIndex >= 0)
    {
        if (hashIndex == 0)
        {
            commentFound = true;
            writeln("ProParser::preprocessLines: skip comment line");
        }
        else
            writeln("ProParser::preprocessLines: cutting off inline comment");

        result = strLine[0 .. hashIndex];
        result = strip(result);
    }
    
    return result;
}

static bool isWhitespace(char c)
{
    return (c == ' ') || (c == '\t');
}

static string enquoteContainsArgument(string strLine)
{
    const auto CONTAINS_STR = "contains(";

    string result;

    // contains(id, regex)
    // NOTE: regex may contain paired parenthesis
    long containsIndex = 0, newContainsEndIndex = -1;
    while (true)
    {
        // FIXME: implement regex search using "contains\\s*\\("
        containsIndex = strLine.indexOf(CONTAINS_STR, containsIndex);
        if (containsIndex == -1)
        {
            // strLine[secondArgumentEndIndex + 2 .. $]
            result ~= strLine[newContainsEndIndex == -1 ? 0 : newContainsEndIndex .. $];
            break;
        }
        
//        writeln("containsIndex = " ~ std.conv.to!string(containsIndex));

        containsIndex += CONTAINS_STR.length;
        
        // Find first argument end
        auto commaIndex = strLine.indexOf(",", containsIndex);
        if (commaIndex == -1)
        {
            assert(0);
            //continue;
        }
        // Skip whitespaces
        auto secondArgumentBeginIndex = commaIndex;
        do { secondArgumentBeginIndex++; } while (isWhitespace(strLine[secondArgumentBeginIndex]) && (secondArgumentBeginIndex < strLine.length));

        // Search for second argument end - skip paired parenthesis
        auto secondArgumentEndIndex = secondArgumentBeginIndex;
        long[] parenthesisStack;
        for (auto i = secondArgumentBeginIndex; i < strLine.length; i++)
        {
            if (strLine[i] == '(')
                parenthesisStack ~= i;
            else if (strLine[i] == ')')
            {
                if (parenthesisStack.empty)
                {
                    secondArgumentEndIndex = i - 1;
                    break;
                }
                
                parenthesisStack.popBack();
            }
        }
        if (secondArgumentEndIndex == secondArgumentBeginIndex)
        {
            assert(0);
            //continue;
        }
        
        // Enquote second argument value
        auto secondArgument = strLine[secondArgumentBeginIndex .. secondArgumentEndIndex + 1];
        auto secondArgumentQuoted = secondArgument;
        if (secondArgument[0] != '"' && secondArgument[$-1] != '"')
            secondArgumentQuoted = '"' ~ secondArgument ~ '"';
        result ~= strLine[newContainsEndIndex == -1 ? 0 : newContainsEndIndex .. commaIndex + 1] ~ " ";
        result ~= secondArgumentQuoted ~ ")";
        
        newContainsEndIndex = secondArgumentEndIndex + 2;
    }
    
    return result;
}

static string preprocessLines(string[] strLinesArray)
{    
    string[] result;
    for (int lineIndex = 0; lineIndex < strLinesArray.length; lineIndex++)
    {
        auto strLine = strLinesArray[lineIndex];
        strLine = strip(strLine);
        strLine = cutInlineComment(strLine);

        bool isMultiLine = false;
        int startLineIndex = -1;
        int endLineIndex = -1;
        if (strLine.endsWith(STR_BACKSLASH))
        {
            isMultiLine = true;

            auto strMultiLine = strip(strLine[0 .. $ - 1]);
            auto j = lineIndex + 1;
            for ( ; j < strLinesArray.length; j++)
            {
                strLine = strLinesArray[j];
                strLine = strip(strLine);
                
                bool commentFound = false;
                strLine = cutInlineComment(strLine, commentFound);

                bool endsWithBackslash = strLine.endsWith(STR_BACKSLASH);
                strMultiLine ~= endsWithBackslash ? " " ~ strip(strLine[0 .. strLine.length - 1]) : " " ~ strLine;

                // NOTE: comment line will be replaced with empty one by cutInlineComment() function call
                if (!commentFound)
                    if (!endsWithBackslash || strLine.empty)
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
        // - the colon is inside quotes/doublequotes
        // - the colon is a part of assignment operator
        if (!strLine.empty && !strLine.endsWith(STR_OPEN_CURLY_BRACE))
        {
            for (int i = cast(int)strLine.length - 1; i >= 0; i--)
            {
                if (strLine[i] == STR_COLON)
                {
                    auto info1 = detectPairedCharacter(STR_DQUOTE, STR_DQUOTE, strLine, "" ~ STR_COLON, i);
                    if (info1.success)
                    {
                        i = info1.indexOpen - 1;
                        continue;
                    }
                    
                    // testFunc(): x = y ... : ...
                    auto lastEqIndex = strLine.lastIndexOf('=');
                    if (lastEqIndex != -1)
                    {
                        if (i >= lastEqIndex)
                            continue;
                    }
                    
                    strLine.replaceInPlace(i, i + 1, "" ~ STR_DOG);
                    writeln("ProParser::preprocessLines: single-line block statement detected");
                    break;
                }
            }
        }
        else if (strLine.endsWith(STR_OPEN_CURLY_BRACE))
        {
            // Remove reduntant colon
            // E.g.: contains(TEMPLATE, ".*app"):!build_pass: {
            auto lastColonIndex = strLine.lastIndexOf(STR_COLON);
            if (lastColonIndex != -1)
            {
                bool reduntant = true;
                for (int i = cast(int)lastColonIndex + 1; i < cast(int)strLine.length - 1; i++)
                {
                    if (strLine[i] != ' ' && strLine[i] != '\t') { reduntant = false; break; }
                }
                if (reduntant)
                    strLine.replaceInPlace(lastColonIndex, lastColonIndex + 1, "");
            }
        }
        
        // Replace "else:" with "else@"
        // FIXME: cover all cases!
        strLine = strLine.replace("else:", "else" ~ STR_DOG);
        
        // Enquote contains test function second argument
        strLine = enquoteContainsArgument(strLine);

        if (isMultiLine)
            writeln("Multi-line " ~ std.conv.to!string(startLineIndex + 1) ~ " - " ~ std.conv.to!string(endLineIndex + 1) ~ ": |" ~ strLine ~ "|");
        else
            writeln("Line " ~ std.conv.to!string(lineIndex + 1) ~ ": |" ~ strLine ~ "|");

//        if (!strLine.empty())
            result ~= strLine;
    }

    return result.join("\n");
}

