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

module project;

//import std.typecons : No;

import std.experimental.logger;

import std.algorithm;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;

import preprocessor;
import qmakeparser;

class Project
{
	const bool tryParse(string fileName)
	{
		import qmakeparser;
		
		trace("Trying to parse project file '" ~ fileName ~ "'...");

		auto proFileContents = std.file.readText(fileName);
		proFileContents = preprocessLines(splitLines(proFileContents));

		auto parseTree = QMakeProject(proFileContents);
		if (!parseTree.successful)
		{
			trace(parseTree);
			trace("Parsing project file '" ~ fileName ~ "' FAILED:");
		}
		else
			trace("Project file '" ~ fileName ~ "' successfully parsed");

		return parseTree.successful;
	}
}

unittest
{
	auto pro = new Project();

	assert(pro.tryParse("tests/variables.pro"));
	assert(pro.tryParse("tests/contains.pro"));
    assert(pro.tryParse("tests/eval.pro"));
    assert(pro.tryParse("tests/test_function_call_1.pro"));
    assert(pro.tryParse("tests/test_function_call_2.pro"));
    assert(pro.tryParse("tests/block_1.pro"));
    assert(pro.tryParse("tests/block_2.pro"));
    assert(pro.tryParse("tests/block_3.pro"));
    assert(pro.tryParse("tests/block_4.pro"));
    assert(pro.tryParse("tests/scope_1.pro"));
    assert(pro.tryParse("tests/scope_2.pro"));
    assert(pro.tryParse("tests/scope_3.pro"));
    assert(pro.tryParse("tests/scope_4.pro"));
    assert(pro.tryParse("tests/scope_5.pro"));
    assert(pro.tryParse("tests/scope_6.pro"));
    assert(pro.tryParse("tests/qt_parts.prf"));
    assert(pro.tryParse("tests/default_post.prf"));
    assert(pro.tryParse("tests/qml_module.prf"));
    assert(pro.tryParse("tests/android-base-head.conf"));
    assert(pro.tryParse("tests/qt_module_headers.prf"));
    assert(pro.tryParse("tests/qt_functions.prf"));
    assert(pro.tryParse("tests/uikit_qt.prf"));
    assert(pro.tryParse("tests/compositor_api.pri"));
    assert(pro.tryParse("tests/linux-clang-qmake.conf"));
}
