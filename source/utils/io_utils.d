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

module source.utils.io_utils;

import std.experimental.logger;

static import std.file;

import std.array;
import std.utf;
import std.range;
import std.regex;

// -------------------------------------------------------------------------------------------------
public:

bool isValidFilePath(const string path)
{
    static import std.file;
    return (!path.empty && std.file.exists(path) && std.file.isFile(path));
}

bool isValidDirectoryPath(const string path)
{
    static import std.file;
    return (!path.empty && std.file.exists(path) && std.file.isDir(path));
}

// -------------------------------------------------------------------------------------------------
private:

// NOTE: Qt QString internal encoding is UTF-16.
// `const QChar *QString::unicode() const`: Returns a Unicode (UTF-16) representation of the string. 
// `ushort QChar::unicode() const`: Returns the numeric Unicode UTF-16 value of the QChar.

// bool isSpecialChar(ushort c, const uchar (&iqm)[16])
bool isSpecialChar(const wchar c, const wchar[16] iqm)
in
{
    assert(iqm.length == 16);
}
do
{
    if ((c < iqm.length * 8) && (iqm[c / 8] & (1 << (c & 7))))
        return true;
    return false;
}

// bool hasSpecialChars(const QString &arg, const uchar (&iqm)[16])
bool hasSpecialChars(const string arg, const wchar[16] iqm)
{
    for (long x = arg.length - 1; x >= 0; --x)
    {
        if (isSpecialChar(toUTF16(arg)[x], iqm))
            return true;
    }
    return false;
}

// QString IoUtils::shellQuoteUnix(const QString &arg)
string shellQuoteUnix(const string arg)
{
    // Chars that should be quoted
    static const wchar[16] iqm = [
        0xff, 0xff, 0xff, 0xff, 0xdf, 0x07, 0x00, 0xd8,
        0x00, 0x00, 0x00, 0x38, 0x01, 0x00, 0x00, 0x78
    ]; // 0-32 \'"$`<>|;&(){}*?#!~[]

    if (arg.empty)
        return "''";

    string ret = arg.dup;
    if (hasSpecialChars(ret, iqm))
    {
        ret = ret.replace('\'', "'\\''");
        ret = '\'' ~ ret;
        ret ~= '\'';
    }
    return ret;
}

// QString IoUtils::shellQuoteWin(const QString &arg)
string shellQuoteWin(const string arg)
{
    // Chars that should be quoted
    // - control chars & space
    // - the shell meta chars "&()<>^|
    // - the potential separators ,;=
    static const wchar[16] iqm = [
        0xff, 0xff, 0xff, 0xff, 0x45, 0x13, 0x00, 0x78,
        0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x10
    ];
    // Shell meta chars that need escaping.
    static const wchar[16] ism = [
        0x00, 0x00, 0x00, 0x00, 0x40, 0x03, 0x00, 0x50,
        0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x10
    ]; // &()<>^|

    if (arg.empty)
        return "\"\"";

    string ret = arg.dup;
    if (hasSpecialChars(ret, iqm))
    {
        // The process-level standard quoting allows escaping quotes with backslashes (note
        // that backslashes don't escape themselves, unless they are followed by a quote).
        // Consequently, quotes are escaped and their preceding backslashes are doubled.
//        ret.replace(QRegExp("(\\\\*)\""), "\\1\\1\\\"");
        ret = replaceAll(ret, regex("(\\\\*)\""), "\\1\\1\\\"");

        // Trailing backslashes must be doubled as well, as they are followed by a quote.
//        ret.replace(QRegExp("(\\\\+)$"), "\\1\\1");
        ret = replaceAll(ret, regex("(\\\\+)$"), "\\1\\1");

        // However, the shell also interprets the command, and no backslash-escaping exists
        // there - a quote always toggles the quoting state, but is nonetheless passed down
        // to the called process verbatim. In the unquoted state, the circumflex escapes
        // meta chars (including itself and quotes), and is removed from the command.
        bool quoted = true;
        for (int i = 0; i < ret.length; i++)
        {
            wchar c = toUTF16(ret)[i];
            if (c == '"')
                quoted = !quoted;
            else if (!quoted && isSpecialChar(c, ism))
                ret.insertInPlace(i++, '^');
        }
        if (!quoted)
            ret ~= '^';
        ret ~= '"';
        ret = '"' ~ ret;
    }

    return ret;
}

public:
string shellQuote(const string arg)
{
    // NOTE: https://dlang.org/spec/version.html
    version (Windows)
    {
        return shellQuoteWin(arg);
    }
    else
    {
        return shellQuoteUnix(arg);
    }
}
