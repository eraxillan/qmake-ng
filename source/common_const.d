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

module common_const;

public const auto STR_EMPTY = "";

public const auto STR_WS = " ";

public const auto STR_HASH = "#";
public const auto STR_BACKSLASH = "\\";

public const auto STR_OPENING_PARENTHESIS = "(";
public const auto STR_CLOSING_PARENTHESIS = ")";
public const auto STR_OPENING_CURLY_BRACE = "{";
public const auto STR_CLOSING_CURLY_BRACE = "}";

public const auto STR_DOUBLE_QUOTE = "\"";
public const auto STR_SINGLE_QUOTE = "'";

public const auto STR_COMMA = ",";

public const auto STR_DOG = "@";
public const auto STR_EXCLAMATION_MARK = "!";
public const auto STR_COLON = ":";
public const auto STR_VERTICAL_BAR = "|";

public const auto STR_EXPAND_MARKER = "$$";
public const auto STR_FUNCTION_EXPAND_MARKER = "$$";

// Assignment operators
public const auto STR_EQUALS = "=";             // TEMPLATE = app
public const auto STR_PLUS_EQUALS = "+=";       // DEFINES += USE_MY_STUFF
public const auto STR_ASTERISK_EQUALS = "*=";   // DEFINES *= USE_MY_STUFF
public const auto STR_MINUS_EQUALS = "-=";      // DEFINES -= USE_MY_STUFF
public const auto STR_TILDE_EQUALS = "~=";      // DEFINES ~= s/QT_[DT].+/QT
