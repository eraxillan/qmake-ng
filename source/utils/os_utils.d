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

module source.utils.os_utils;

import std.experimental.logger;

import std.string;

// -------------------------------------------------------------------------------------------------
public:

void setupDatetimeLocale()
{
    import std.process : environment;
    import core.stdc.locale: setlocale, LC_ALL, LC_TIME;

    auto value = environment.get("LC_TIME");
    if (value is null)
    {
        warning("LC_TIME is not defined: use en_US.UTF-8 by default");
        setlocale(LC_TIME, "en_US.UTF-8");
    }
    else
        setlocale(LC_TIME, value.ptr);
}

auto getDateTimeString()
{
    import core.stdc.time : time, localtime, strftime;
    auto unixTime = time(null);
    auto tmVar = localtime(&unixTime);
    char[256] buffer;
    auto len = strftime(buffer.ptr, 80, toStringz("%a %b. %d %T %Y"), tmVar);
    auto prettyStr = buffer[0 .. len].idup;
    prettyStr = toLower(prettyStr);
    return prettyStr;
}

string getProcessOutput(const string command)
{
    import std.process : executeShell;

    auto outputData = executeShell(command);

    if (outputData.status != 0)
    {
        error("Command '%s' failed with code %d", command, outputData.status);
        return "";
    }

    return outputData.output.strip();
}
