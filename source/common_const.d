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

public:
const string QMAKE_VERSION_STR = "3.1";

const auto STR_EMPTY = "";

const auto CHAR_WS = ' ';
const auto STR_WS = " ";

const auto STR_HASH = "#";
const auto STR_DOLLAR = "$";
const auto STR_WHITESPACE = "\t";
const auto CHAR_BACKSLASH = '\\';
const auto STR_BACKSLASH = "\\";

const auto CHAR_UNDERSCORE = '_';

const auto CHAR_OPENING_PARENTHESIS = '(';
const auto CHAR_CLOSING_PARENTHESIS = ')';
const auto STR_OPENING_PARENTHESIS = "(";
const auto STR_CLOSING_PARENTHESIS = ")";
const auto STR_OPENING_CURLY_BRACE = "{";
const auto STR_CLOSING_CURLY_BRACE = "}";

const auto CHAR_DOUBLE_QUOTE = '"';
const auto CHAR_SINGLE_QUOTE = '\'';
const auto STR_DOUBLE_QUOTE = "\"";
const auto STR_SINGLE_QUOTE = "'";

const auto STR_SEMICOLON = ";";

const auto CHAR_DOT = '.';
const auto CHAR_COMMA = ',';
const auto STR_COMMA = ",";

const auto CHAR_DOG = '@';
const auto STR_DOG = "@";
const auto STR_EXCLAMATION_MARK = "!";
const auto CHAR_COLON = ':';
const auto STR_COLON = ":";
const auto STR_VERTICAL_BAR = "|";

const auto CHAR_SINGLE_EXPAND_MARKER = '$';
const auto STR_SINGLE_EXPAND_MARKER = "$";
// FIXME: single dollar means "expand variable at the makefile processing, not project file one"
const auto STR_GENERATOR_EXPAND_MARKER = "${";
const auto STR_VARIABLE_EXPAND_MARKER = "$${";
const auto STR_ENV_VARIABLE_EXPAND_MARKER = "$$(";
const auto STR_PROPERTY_EXPAND_MARKER = "$$[";
const auto STR_PROPERTY_GET_SUFFIX = "/get";

const auto STR_EXPAND_MARKER = "$$";
const auto STR_FUNCTION_EXPAND_MARKER = "$$";

// Assignment operators
const auto STR_EQUALS = "=";             // TEMPLATE = app
const auto STR_PLUS_EQUALS = "+=";       // DEFINES += USE_MY_STUFF
const auto STR_ASTERISK_EQUALS = "*=";   // DEFINES *= USE_MY_STUFF
const auto STR_MINUS_EQUALS = "-=";      // DEFINES -= USE_MY_STUFF
const auto STR_TILDE_EQUALS = "~=";      // DEFINES ~= s/QT_[DT].+/QT

// Boolean constants
const auto STR_TRUE = "true";
const auto STR_FALSE = "false";

const auto STR_HOSTBUILD = "host_build";

const auto MKSPECS_DIR = "mkspecs";
const auto FEATURES_DIR = "features";

const auto QMAKE_SPEC_PRE_FILE = "spec_pre.prf";
const auto QMAKE_SPEC_POST_FILE = "spec_post.prf";

const auto QMAKE_PRE_FILE = "default_pre.prf";
const auto QMAKE_POST_FILE = "default_post.prf";

