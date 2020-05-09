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

module source.qmakeexception;

static import std.conv;

public:

/*********************
 * Thrown if we reach code that is not implemented yet.
 */
class NotImplementedException : Exception
{
    /**
     * Initialize with a message.
     */
    this(string message, int lineNo = __LINE__, string functionName = __FUNCTION__) @trusted
    {
        super(functionName ~ ":" ~ std.conv.to!string(lineNo) ~ ": " ~ message);
    }

    /// Convenience functions that throw an `NotImplementedException`.
    static void opCall(string msg)
    {
        throw new NotImplementedException(msg);
    }

    /// ditto
    static void opCall()
    {
        throw new NotImplementedException(null);
    }
}

class EvalFailedException : Exception
{
    this(string message, int lineNo = __LINE__, string functionName = __FUNCTION__) @trusted
    {
        super(functionName ~ ":" ~ std.conv.to!string(lineNo) ~ ": " ~ message);
    }

    static void opCall(string msg)
    {
        throw new EvalFailedException(msg);
    }

    static void opCall()
    {
        throw new EvalFailedException(null);
    }
}

class EvalLogicalException : Exception
{
    this(string message, int lineNo = __LINE__, string functionName = __FUNCTION__) @trusted
    {
        super(functionName ~ ":" ~ std.conv.to!string(lineNo) ~ ": " ~ message);
    }

    static void opCall(string msg)
    {
        throw new EvalLogicalException(msg);
    }

    static void opCall()
    {
        throw new EvalLogicalException(null);
    }
}

class EvalVariableException : Exception
{
    this(string message, int lineNo = __LINE__, string functionName = __FUNCTION__) @trusted
    {
        super(functionName ~ ":" ~ std.conv.to!string(lineNo) ~ ": " ~ message);
    }

    static void opCall(string msg)
    {
        throw new EvalVariableException(msg);
    }

    static void opCall()
    {
        throw new EvalVariableException(null);
    }
}

class EvalFunctionException : Exception
{
    this(string message, int lineNo = __LINE__, string functionName = __FUNCTION__) @trusted
    {
        super(functionName ~ ":" ~ std.conv.to!string(lineNo) ~ ": " ~ message);
    }

    static void opCall(string msg)
    {
        throw new EvalFunctionException(msg);
    }

    static void opCall()
    {
        throw new EvalFunctionException(null);
    }
}

class CollectionException : Exception
{
    this(string message, int lineNo = __LINE__, string functionName = __FUNCTION__) @trusted
    {
        super(functionName ~ ":" ~ std.conv.to!string(lineNo) ~ ": " ~ message);
    }

    static void opCall(string msg)
    {
        throw new CollectionException(msg);
    }

    static void opCall()
    {
        throw new CollectionException(null);
    }
}

class EscapeSequenceException : Exception
{
    this(string message, int lineNo = __LINE__, string functionName = __FUNCTION__) @trusted
    {
        super(functionName ~ ":" ~ std.conv.to!string(lineNo) ~ ": " ~ message);
    }

    static void opCall(string msg)
    {
        throw new EscapeSequenceException(msg);
    }

    static void opCall()
    {
        throw new EscapeSequenceException(null);
    }
}
