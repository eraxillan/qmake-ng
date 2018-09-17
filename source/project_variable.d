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

module project_variable;

import std.experimental.logger;

import std.uni;
import std.algorithm;
import std.conv;
import std.stdio;
import std.file;
import std.getopt;
import std.path;
import std.string;
import std.range;

import common_const;

// -------------------------------------------------------------------------------------------------

public enum VariableType
{
    UNKNOWN = -1,
    RAW_STRING = 0,           // string with any characters, e.g. argument of message() test function: message(I am a raw string)
    NUMBER,                   // integer number, e.g. qmake debug level: debug(1, This text will be shown under debug level 1)
    STRING,                   // string without whitespaces/commas, e.g. variable name
    STRING_LIST,    	  	  // array of strings without whitespaces/commas
    RESTRICTED_STRING,        // string without whitespaces/commas with value from the specified array, e.g. TEMPLATE = app|...|lib
    RESTRICTED_STRING_LIST,   // array of such strings
    OBJECT,                   // object with properties (e.g. host.arch)
    OBJECT_LIST,              // list of objects with properties described above
    // TODO:
//    FILE_PATH,
//    DIR_PATH
	COUNT
}

public struct ProVariable
{
	string name;
	VariableType type;
	string[] valueRange;
	string[] value;
	// FIXME: add other fields
//	        canBeEmpty: true,
//            platform: undefined,
//            template: undefined,
//            isReadOnly: false,
//            isRare: false

	this(in string _name, in VariableType _type, string[] _valueRange, string[] _value)
	{
		name = _name;
		type = _type;
		valueRange = _valueRange;
		value = _value;
	}

	ProVariable dup() const @property
    {
        auto result = ProVariable(name.dup, type, valueRange.dup, value.dup);
        return result;
    }
	/+
	ProVariable idup() const @property
    {
        auto result = ProVariable(name.idup, type, valueRange.idup, value.idup);
        return result;
    }
	+/
}

// --------------------------------------------------------------------------------------------------------------------------------------------------

private string[] initConfigVariableValue()
{
    return [
        "lex", "yacc", "debug", "exceptions", "depend_includepath", "testcase_targets",
        "import_plugins", "import_qpa_plugin", "file_copies", "qmake_use", "qt", "warn_on",
        "release", "link_prl", "incremental", "shared", "linux", "unix", "posix",
        "gcc", "qml_debug"
    ];
}

private string[] initConfigVariableRange()
{
    return [
        "release", "debug", "debug_and_release", "debug_and_release_target", "qml_debug",
        "qmake_use", "build_all", "autogen_precompile_source", "ordered", "precompile_header", "depend_includepath",
        "warn_on", "warn_off", "exceptions", "exceptions_off",
        "rtti", "rtti_off", "stl", "stl_off", "thread",
        "c++11", "c++14",
        "lex", "yacc",
        "create_prl", "link_prl", "incremental",
        "qt", "x11", "testcase", "testcase_targets", "insignificant_test",
        "windows", "console", "shared", "dll", "static", "staticlib",
        "linux", "unix", "posix", "gcc",
        "plugin", "import_plugins", "import_qpa_plugin",
        "designer", "no_lflags_merge", "file_copies",
        "flat", "embed_manifest_dll", "embed_manifest_exe",
        "app_bundle", "lib_bundle",
        "largefile", "separate_debug_info"
    ];
}

private string[] initQtVariableValue()
{
    return ["core", "gui"];
}

private string[] initQtVariableRange()
{
    return [
        "core", "gui", "widgets", "network", "multimedia", "sql", "testlib", "multimediawidgets", "qml", "quick",
        "axcontainer", "axserver",
        "3dcore", "3drender", "3dinput", "3dlogic", "3dextras",
        "enginio", "androidextras", "bluetooth", "concurrent", "dbus", "location",
        "macextras", "nfc", "opengl", "positioning", "printsupport", "purchasing",
        "quickcontrols2", "quickwidgets", "script", "scripttools", "scxml",
        "sensors", "serialbus", "serialport", "svg", "webchannel", "webengine", "websockets", "webview",
        "winextras", "x11extras", "xml", "xmlpatterns", "charts", "datavisualization"
    ];
}

private string[] initQtArchVariableRange()
{
	// FIXME: read-only variable
	return [];
}

private string[] initQtArchVariableValue()
{
	return ["x86_64"];
}

private string[] initTemplateVariableRange()
{
    return ["app", "lib", "aux", "subdirs", "vcsubdirs", "vcapp", "vclib"];
}

private string[] initTemplateVariableValue()
{
    return ["app"];
}

private string[] initQmakeCompilerVariableRange()
{
	return [
        "dummy_compiler",
        "msvc", "gcc",
        "clang", "llvm", "clang_cl",
        "intel_icc", "intel_icl",
        "rim_qcc", "ghs", "sun_cc",
        // unsupported
        "wr_dcc"
	];
}

private string[] initQmakeCompilerVariableValue()
{
	return ["msvc"];
}

private string[] initQmakePlatformVariableRange()
{
	return [
        "dummy_platform",
        "unix", "posix",
        "mingw", "cygwin",
        "win32", "winrt",
        "mac", "macos", "osx", "macx", "darwin",
        "ios", "tvos", "watchos", "uikit",
        "android",
        "linux",
        "nacl",
        "bsd", "freebsd", "openbsd", "netbsd",
        "aix", "solaris", "hpux",
        "vxworks", "qnx", "integrity",
        "lynxos", "haiku", "boot2qt", "hurd",
    ];
}

private string[] initQmakePlatformVariableValue()
{
	return ["win32"];
}

// --------------------------------------------------------------------------------------------------------------------------------------------------

public immutable ProVariable[string] builtinVariables;

static this()
{
    import std.exception : assumeUnique;

    ProVariable[string] temp1; // mutable buffer
	temp1["CONFIG"] = ProVariable("CONFIG", VariableType.RESTRICTED_STRING_LIST, initConfigVariableRange(), initConfigVariableValue());
	temp1["DEFINES"] = ProVariable("DEFINES", VariableType.STRING_LIST, [], []);
	temp1["DESTDIR"] = ProVariable("DESTDIR", VariableType.STRING, [], [STR_EMPTY]);
	temp1["DISTFILES"] = ProVariable("DISTFILES", VariableType.STRING_LIST, [], []);
	temp1["FORMS"] = ProVariable("FORMS", VariableType.STRING_LIST, [], []);
	temp1["HEADERS"] = ProVariable("HEADERS", VariableType.STRING_LIST, [], []);
	temp1["INCLUDEPATH"] = ProVariable("INCLUDEPATH", VariableType.STRING_LIST, [], []);
	temp1["INSTALLS"] = ProVariable("INSTALLS", VariableType.OBJECT_LIST, [], []);
	temp1["LIBS"] = ProVariable("LIBS", VariableType.STRING_LIST, [], []);
	temp1["LITERAL_HASH"] = ProVariable("LITERAL_HASH", VariableType.STRING, [], [STR_HASH]);
	temp1["MOC_DIR"] = ProVariable("MOC_DIR", VariableType.STRING, [], [STR_EMPTY]);
	temp1["OBJECTS"] = ProVariable("OBJECTS", VariableType.STRING_LIST, [], []);
	temp1["OBJECTS_DIR"] = ProVariable("OBJECTS_DIR", VariableType.STRING, [], [STR_EMPTY]);
	temp1["PRECOMPILED_HEADER"] = ProVariable("PRECOMPILED_HEADER", VariableType.STRING, [], [STR_EMPTY]);
	
	temp1["PWD"] = ProVariable("PWD", VariableType.STRING, [], [STR_EMPTY]);
	temp1["OUT_PWD"] = ProVariable("OUT_PWD", VariableType.STRING, [], [STR_EMPTY]);
	temp1["_PRO_FILE_"] = ProVariable("_PRO_FILE_", VariableType.STRING, [], [STR_EMPTY]);
	temp1["_PRO_FILE_PWD_"] = ProVariable("_PRO_FILE_PWD_", VariableType.STRING, [], [STR_EMPTY]);
	
	temp1["QMAKE_CFLAGS"] = ProVariable("QMAKE_CFLAGS", VariableType.STRING_LIST, [], []);
	temp1["QMAKE_COMPILER"] = ProVariable("QMAKE_COMPILER", VariableType.RESTRICTED_STRING_LIST, initQmakeCompilerVariableRange(), initQmakeCompilerVariableValue());
	temp1["QMAKE_PLATFORM"] = ProVariable("QMAKE_PLATFORM", VariableType.RESTRICTED_STRING_LIST, initQmakePlatformVariableRange(), initQmakePlatformVariableValue());
	temp1["QT"] = ProVariable("QT", VariableType.RESTRICTED_STRING_LIST, initQtVariableRange(), initQtVariableValue());
	temp1["QT_ARCH"] = ProVariable("QT_ARCH", VariableType.RESTRICTED_STRING, initQtArchVariableRange(), initQtArchVariableValue());
	temp1["SOURCES"] = ProVariable("SOURCES", VariableType.STRING_LIST, [], []);
	temp1["SUBDIRS"] = ProVariable("SUBDIRS", VariableType.OBJECT_LIST, [], []);
	temp1["TARGET"] = ProVariable("TARGET", VariableType.STRING, [], [STR_EMPTY]);
	temp1["TEMPLATE"] = ProVariable("TEMPLATE", VariableType.RESTRICTED_STRING, initTemplateVariableRange(), initTemplateVariableValue());
	temp1["UI_DIR"] = ProVariable("UI_DIR", VariableType.STRING, [], [STR_EMPTY]);
	temp1["VERSION"] = ProVariable("VERSION", VariableType.STRING, [], [STR_EMPTY]);
    temp1.rehash; // for faster lookups
    builtinVariables = assumeUnique(temp1);
}
