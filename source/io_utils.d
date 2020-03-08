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

module source.io_utils;

import std.experimental.logger;

static import std.file;

import std.range;

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
