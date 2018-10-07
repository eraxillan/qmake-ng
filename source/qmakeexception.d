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

module qmakeexception;

/*********************
 * Thrown if we reach code that is not implemented yet.
 */
public class NotImplementedException : Exception
{
/**
Initialize with a message and an error code.
*/
    this(string message) @trusted
    {
        super(message);
    }

/** Convenience functions that throw an `NotImplementedException`. */
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


/*********************
 * Thrown if evaluation logic errors happen.
 */
public class EvalLogicException : Exception
{
/**
Initialize with a message and an error code.
*/
    this(string message) @trusted
    {
        super(message);
    }

/** Convenience functions that throw an `EvalLogicException`. */
    static void opCall(string msg)
    {
        throw new EvalLogicException(msg);
    }

/// ditto
    static void opCall()
    {
        throw new EvalLogicException(null);
    }
}
