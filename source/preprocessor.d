/****************************************************************************
**
** Copyright (C) 2018 Alexander Kamyshnikov
** Contact: axill777@gmail.com
**
** This file is part of the qmake-ng application, replacement of the Qt Toolkit one.
**
** Foobar is free software: you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation, either version 3 of the License, or
** (at your option) any later version.
**
** Foobar is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with qmake-ng.  If not, see <http://www.gnu.org/licenses/>.
**
****************************************************************************/

module preprocessor;

import std.typecons : No;

import std.experimental.logger;

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
const auto STR_OPENING_PARENTHESIS = '(';
const auto STR_CLOSING_PARENTHESIS = ')';
const auto STR_COLON = ':';
const auto STR_COMMA = ',';
const auto STR_DOG = '@';
const auto STR_BACKSLASH = '\\';

const auto CONTAINS_STR = "contains(";
const auto QTCONFIG_STR = "qtConfig(";

struct QuotesInfo
{
    long indexOpen;
    long indexClose;
    bool success;
};

static QuotesInfo detectFunctionArgument(string functionName, string strLine, string subStr, long index)
{
    long[] parenthesisStack;

    for (long i = 0; i < index; i++)
    {
        if (strLine[i] == STR_OPENING_PARENTHESIS)
            parenthesisStack ~= i;
        else if (strLine[i] == STR_CLOSING_PARENTHESIS)
        {
            if (!parenthesisStack.empty)
                parenthesisStack.popBack();
        }
    }
    if (parenthesisStack.length != 1)
    {
        trace("no open parenthesis found, stack.length = " ~ std.conv.to!string(parenthesisStack.length));
        return QuotesInfo(-1, -1, false);
    }
    auto indexOpen = parenthesisStack[0];
    parenthesisStack = [];
    assert(parenthesisStack.length == 0);
    
    for (long i = index + 1; i < strLine.length; i++)
    {
        if (strLine[i] == '(')
            parenthesisStack ~= i;
        else if (strLine[i] == ')')
        {
            if (!parenthesisStack.empty)
                parenthesisStack.popBack();
            
            parenthesisStack ~= i;
        }
    }
    if (parenthesisStack.length != 1)
    {
        trace("no close parenthesis found, stack.length = " ~ std.conv.to!string(parenthesisStack.length));
        return QuotesInfo(-1, -1, false);
    }
    auto indexClose = parenthesisStack[0];
    parenthesisStack = [];
    
    auto thisFunctionName = strLine[indexOpen - functionName.length .. indexOpen];
    if (thisFunctionName != functionName)
    {
        trace("non-ambiguous function name = " ~ thisFunctionName);
        return QuotesInfo(-1, -1, false);
    }
    
    return QuotesInfo(indexOpen, indexClose, true);
}

static QuotesInfo detectPairedCharacter(char openChar, char closeChar, string strLine, string subStr, long index)
{    
    long[] stack;
    
    // Search for opening separator: skip paired characters until we find unpaired one
    for (auto i = index - 1; i >= 0; i--)
    {
        if (strLine[i] == openChar)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
    {
        trace("no open char '" ~ openChar ~ "' before index " ~ std.conv.to!string(index)
                ~ ", stack.length = " ~ std.conv.to!string(stack.length));
        return QuotesInfo(-1, -1, false);
    }
    auto indexOpen = stack[0];
    stack = [];

    // Search for closing double quote (unpaired)
    for (auto i = index + 1; i < strLine.length; i++)
    {
        if (strLine[i] == closeChar)
            stack ~= i;
    }
    if (stack.empty || (stack.length % 2 == 0))
    {
        trace("no close char '" ~ closeChar ~ "' before index " ~ std.conv.to!string(index)
                ~ ", stack.length = " ~ std.conv.to!string(stack.length));
        return QuotesInfo(-1, -1, false);
    }
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
            trace("skip comment line");
        }
        else
            trace("cutting off inline comment");

        result = strLine[0 .. hashIndex];
        result = strip(result);
    }
    
    return result;
}

static bool isWhitespace(char c)
{
    return (c == ' ') || (c == '\t');
}

// qtConfig(opengl(es1|es2)?)
// contains(var, regex)
static string enquoteFunctionArgument(string functionName, int argumentIndex, string strLine)
{
    string result;

    // contains(id, regex)
    // NOTE: regex may contain paired parenthesis
    long functionIndex = 0, newFunctionEndIndex = -1;
    while (true)
    {
        // FIXME: implement regex search using "contains\\s*\\("
        functionIndex = strLine.indexOf(functionName, functionIndex);
        if (functionIndex == -1)
        {
            result ~= strLine[newFunctionEndIndex == -1 ? 0 : newFunctionEndIndex .. $];
            break;
        }
        
        trace("ambiguous function detected at index " ~ std.conv.to!string(functionIndex));

        functionIndex += functionName.length;
        
        // Skip previous arguments
        auto commaIndex = functionIndex;
        auto currentArgumentIndex = 1;
        while (currentArgumentIndex < argumentIndex)
        {
            commaIndex = strLine.indexOf(STR_COMMA, commaIndex);
            if (commaIndex == -1)
            {
                trace("ambiguous function '" ~ functionName ~ "' argument count is " ~ std.conv.to!string(currentArgumentIndex));
                break;
            }
            
            currentArgumentIndex++;
        }        
        assert(commaIndex >= 0);
        
        trace("strLine[commaIndex] = '" ~ strLine[commaIndex] ~ "'");
        if (argumentIndex >= 2)
            commaIndex++;
        
        // Skip whitespaces between comma and next argument value
        // FIXME: move this code to separate function
        auto secondArgumentBeginIndex = commaIndex;
        while (isWhitespace(strLine[secondArgumentBeginIndex]) && (secondArgumentBeginIndex < strLine.length))
            secondArgumentBeginIndex++;

        // Search for second argument end - skip paired parenthesis
        auto secondArgumentEndIndex = secondArgumentBeginIndex;
        trace("strLine[secondArgumentEndIndex] = '" ~ strLine[secondArgumentEndIndex] ~ "'");
        long[] parenthesisStack;
        for (auto i = secondArgumentBeginIndex; i < strLine.length; i++)
        {
            if (strLine[i] == '(')
                parenthesisStack ~= i;
            else if (strLine[i] == ')')
            {
                if (parenthesisStack.empty)
                {
                    secondArgumentEndIndex = i;
                    break;
                }
                
                parenthesisStack.popBack();
            }
        }
        
        string wsSuffix = "";
        if (argumentIndex >= 2)
            wsSuffix = " ";
        
        // Enquote specified argument value
        auto secondArgument = strLine[secondArgumentBeginIndex .. secondArgumentEndIndex];
        auto secondArgumentQuoted = secondArgument;
        if (secondArgument[0] != '"' && secondArgument[$-1] != '"')
            secondArgumentQuoted = '"' ~ secondArgument ~ '"';
        result ~= strLine[newFunctionEndIndex == -1 ? 0 : newFunctionEndIndex .. commaIndex] ~ wsSuffix;
        result ~= secondArgumentQuoted ~ ")";
        
        trace("secondArgument = '" ~ secondArgument ~ "'");
        trace("secondArgumentQuoted = '" ~ secondArgumentQuoted ~ "'");
        
        newFunctionEndIndex = secondArgumentEndIndex + 1;
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
            for (long i = strLine.length - 1; i >= 0; i--)
            {
                if (strLine[i] == STR_COLON)
                {
                    trace("colon detected at index " ~ std.conv.to!string(i));

                    auto info1 = detectPairedCharacter(STR_DQUOTE, STR_DQUOTE, strLine, "" ~ STR_COLON, i);
                    if (info1.success)
                    {
                        i = info1.indexOpen - 1;
                        trace("quote begin = " ~ std.conv.to!string(info1.indexOpen));
                        trace("quote end = " ~ std.conv.to!string(info1.indexClose));
                        trace("colon inside quotes detected, go back to index " ~ std.conv.to!string(i));
                        continue;
                    }
                    
                    auto info2 = detectFunctionArgument("requires", strLine, "" ~ STR_COLON, i);
                    if (info2.success)
                    {
                        i = info2.indexOpen - 1;
                        trace("function begin = " ~ std.conv.to!string(info2.indexOpen));
                        trace("function end = " ~ std.conv.to!string(info2.indexClose));
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
                    trace("single-line block statement detected");
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
        strLine = strLine.replace("else:", "else" ~ STR_DOG);
        
        // Enquote contains test function second argument
        strLine = enquoteFunctionArgument(CONTAINS_STR, 2, strLine);
        
        // Enquote qtConfig test function first argument
        strLine = enquoteFunctionArgument(QTCONFIG_STR, 1, strLine);

        if (isMultiLine)
            trace("Multi-line " ~ std.conv.to!string(startLineIndex + 1) ~ " - " ~ std.conv.to!string(endLineIndex + 1) ~ ": |" ~ strLine ~ "|");
        else
            trace("Line " ~ std.conv.to!string(lineIndex + 1) ~ ": |" ~ strLine ~ "|");

        result ~= strLine;
    }

    return result.join("\n");
}

unittest
{
	// FIXME: implement
}
