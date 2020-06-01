/****************************************************************************
**
** Copyright (C) 2020 Alexander Kamyshnikov
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

module source.logger;

static import std.file;
static import std.path;

import core.stdc.errno;
import std.exception;
import std.stdio;
import std.string;

class NgLogger
{
private:
    string m_includeLogFilePath;
    string m_variableLogFilePath;
    File m_includeLogFile;
    File m_variableLogFile;

    string m_ws = "|-";

    this()
    {
        immutable(string) applicationDir = std.path.dirName(std.file.thisExePath());
        m_includeLogFilePath = std.path.buildPath(applicationDir, "project_load_graph.log");
        m_variableLogFilePath = std.path.buildPath(applicationDir, "project_variable.log");

        try
        {
            m_includeLogFile = File(m_includeLogFilePath, "w");
        }
        catch (ErrnoException ex)
        {
            switch (ex.errno)
            {
                case EPERM:
                case EACCES:
                    writeln("[NgLogger] Unable to open '%s': permission denied", m_includeLogFilePath);
                    break;

                default:
                    writeln("[NgLogger] Unable to open '%s': unknown error with code '%d'", m_includeLogFilePath, ex.errno);
                    break;
            }
        }

        try
        {
            m_variableLogFile = File(m_variableLogFilePath, "w");
        }
        catch (ErrnoException ex)
        {
            switch (ex.errno)
            {
                case EPERM:
                case EACCES:
                    writeln("[NgLogger] Unable to open '%s': permission denied", m_variableLogFilePath);
                    break;

                default:
                    writeln("[NgLogger] Unable to open '%s': unknown error with code '%d'", m_variableLogFilePath, ex.errno);
                    break;
            }
        }

        // NOTE: file will be closed automatically
    }

    // TLS flag, each thread has its own
    static bool instantiated_;

    // "True" global
    __gshared NgLogger instance_;

    void increaseIndentation()
    {
        m_ws ~= "--";
    }

    void decreaseIndentation()
    {
        m_ws = chop(chop(m_ws));
    }

public:
    static NgLogger get()
    {
        // Since every thread has its own instantiated_ variable,
        // there is no need for synchronization here.
        if (!instantiated_)
        {
            synchronized (NgLogger.classinfo)
            {
                if (!instance_)
                {
                    instance_ = new NgLogger();
                }
                instantiated_ = true;
            }
        }
        return instance_;
    }

public:
    // TODO: use std.outbuffer.OutBuffer class instead of direct writing to disk
    // TODO: add variadic variants of logging functions

    void traceIncludeBegin(const string fileName)
    {
        m_includeLogFile.writeln(m_ws ~ "load subproject begin: " ~ fileName);
        increaseIndentation();
    }

    void traceIncludeEnd(const string fileName)
    {
        decreaseIndentation();
        m_includeLogFile.writeln(m_ws ~ "load subproject   end: " ~ fileName);
    }

    void traceLoadBegin(const string fileName)
    {
        m_includeLogFile.writeln(m_ws ~ "load feature begin: " ~ fileName);
        increaseIndentation();
    }

    void traceLoadEnd(const string fileName)
    {
        decreaseIndentation();
        m_includeLogFile.writeln(m_ws ~ "load feature   end: " ~ fileName);
    }

    void traceMkspecLoadBegin(const string fileName)
    {
        m_includeLogFile.writeln(m_ws ~ "load mkspec begin: " ~ fileName);
        increaseIndentation();
    }

    void traceMkspecLoadEnd(const string fileName)
    {
        decreaseIndentation();
        m_includeLogFile.writeln(m_ws ~ "load mkspec   end: " ~ fileName);
    }

    void traceProjectLoadBegin(const string fileName)
    {
        m_includeLogFile.writeln(m_ws ~ "load project begin: " ~ fileName);
        increaseIndentation();
    }

    void traceProjectLoadEnd(const string fileName)
    {
        decreaseIndentation();
        m_includeLogFile.writeln(m_ws ~ "load project   end: " ~ fileName);
    }

    void traceProjectVariableAssignment(const string name, const string[] value, const bool isBuiltin)
    {
        if (name == "PWD")
            m_variableLogFile.writeln("\n===============================================================================================");

        {
            string ws;
            for (int i = 2; i < m_ws.length; i++) ws ~= " ";
            const string builtinMark = ws ~ (isBuiltin ? "[builtin] " : "[user] ");
            if (value.empty)
                m_variableLogFile.writeln(builtinMark ~ name ~ " = " ~ "<empty>");
            else if (value.length == 1)
                m_variableLogFile.writeln(builtinMark ~ name ~ " = " ~ "'" ~ value[0] ~ "'");
            else
                m_variableLogFile.writeln(builtinMark ~ name ~ " = ", value);
        }

        if (name == "_PRO_FILE_PWD_")
            m_variableLogFile.writeln("\n");
    }

    void traceProjectVariableUnset(const string name, const bool isBuiltin)
    {
        string ws;
        for (int i = 2; i < m_ws.length; i++) ws ~= " ";
        const string builtinMark = ws ~ (isBuiltin ? "[builtin] " : "[user] ");
        m_variableLogFile.writeln(builtinMark ~ "unset " ~ name);
    }
}
