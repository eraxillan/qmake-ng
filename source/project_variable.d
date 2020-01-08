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

// FIXME: rename to `DataType` and move to the separate module
public enum VariableType
{
    UNKNOWN = -1,
    VOID = 0,
    BOOLEAN = 1,
    RAW_STRING,               // string with any characters, e.g. argument of message() test function: message(I am a raw string)
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
public bool isStringType(in VariableType type)
{
    return type == VariableType.RAW_STRING || type == VariableType.STRING || type == VariableType.RESTRICTED_STRING;
}
public bool isStringListType(in VariableType type)
{
    return type == VariableType.STRING_LIST || type == VariableType.RESTRICTED_STRING_LIST;
}

// FIXME: implement read-only built-in variables
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

private string[] initQmakeSpecVariableValue()
{
    // FIXME: platform autodetect
    return ["/opt/Qt/5.11.3/gcc_64/mkspecs/linux-g++"];
}

private string[] initConfigVariableValue()
{
    // FIXME: platform autodetect
    return [
        "lex", "yacc", "debug", "exceptions", "depend_includepath", "testcase_targets",
        "import_plugins", "import_qpa_plugin", "file_copies", "qmake_use", "qt", "warn_on",
        "release", "link_prl", "incremental", "shared", "linux", "unix", "posix",
        "gcc", "qml_debug"
    ];
}

private string[] initConfigVariableRange()
{
    // FIXME: CONFIG variable can be extended with user-defined values! this list is uncomplete
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
        "largefile", "separate_debug_info", "copy_dir_files"
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
	return ["i386", "x86_64", "arm"];
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

// --------------------------------------------------------------------------------------------------------------------------------------------------

public immutable ProVariable[string] builtinVariables;

shared static this()
{
    import std.exception : assumeUnique;

    ProVariable[string] temp1; // mutable buffer

    temp1["DIR_SEPARATOR"] = ProVariable("DIR_SEPARATOR", VariableType.STRING, [], []);
    temp1["DIRLIST_SEPARATOR"] = ProVariable("DIRLIST_SEPARATOR", VariableType.STRING, [], []);
    temp1["QMAKE_DIR_SEP"] = ProVariable("QMAKE_DIR_SEP", VariableType.STRING, [], []);
    temp1["QMAKE_DIRLIST_SEP"] = ProVariable("QMAKE_DIRLIST_SEP", VariableType.STRING, [], []);

    temp1["_DATE_"] = ProVariable("_DATE_", VariableType.STRING, [], []);
    temp1["QMAKE_QMAKE"] = ProVariable("QMAKE_QMAKE", VariableType.STRING, [], []);
    temp1["QMAKE_ARGS"] = ProVariable("QMAKE_ARGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_QTCONF"] = ProVariable("QMAKE_QTCONF", VariableType.STRING, [], []);
    temp1["QMAKE_PATH_ENV"] = ProVariable("QMAKE_PATH_ENV", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_MODULE_PATH"] = ProVariable("QMAKE_MODULE_PATH", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SH"] = ProVariable("QMAKE_SH", VariableType.STRING, [], []);

    temp1["QMAKE_HOST.cpu_count"] = ProVariable("QMAKE_HOST.cpu_count", VariableType.STRING, [], []);
    temp1["QMAKE_HOST.os"] = ProVariable("QMAKE_HOST.os", VariableType.STRING, [], []);
    temp1["QMAKE_HOST.name"] = ProVariable("QMAKE_HOST.name", VariableType.STRING, [], []);
    temp1["QMAKE_HOST.version"] = ProVariable("QMAKE_HOST.version", VariableType.STRING, [], []);
    temp1["QMAKE_HOST.version_string"] = ProVariable("QMAKE_HOST.version_string", VariableType.STRING, [], []);
    temp1["QMAKE_HOST.arch"] = ProVariable("QMAKE_HOST.arch", VariableType.STRING, [], []);

    temp1["QMAKE_TARGET.arch"] = ProVariable("QMAKE_TARGET.arch", VariableType.STRING, [], []);

    // qtconfig.pri
    temp1["QT_BUILDABI"] = ProVariable("QT_BUILDABI", VariableType.STRING, [], []);
    temp1["QT.global.enabled_features"] = ProVariable("QT.global.enabled_features", VariableType.STRING_LIST, [], []);
    temp1["QT.global.disabled_features"] = ProVariable("QT.global.disabled_features", VariableType.STRING_LIST, [], []);
    temp1["QT_VERSION"] = ProVariable("QT_VERSION", VariableType.STRING, [], []);
    temp1["QT_MAJOR_VERSION"] = ProVariable("QT_MAJOR_VERSION", VariableType.STRING, [], []);
    temp1["QT_MINOR_VERSION"] = ProVariable("QT_MINOR_VERSION", VariableType.STRING, [], []);
    temp1["QT_PATCH_VERSION"] = ProVariable("QT_PATCH_VERSION", VariableType.STRING, [], []);
    temp1["QT_GCC_MAJOR_VERSION"] = ProVariable("QT_GCC_MAJOR_VERSION", VariableType.STRING, [], []);
    temp1["QT_GCC_MINOR_VERSION"] = ProVariable("QT_GCC_MINOR_VERSION", VariableType.STRING, [], []);
    temp1["QT_GCC_PATCH_VERSION"] = ProVariable("QT_GCC_PATCH_VERSION", VariableType.STRING, [], []);
    temp1["QT_EDITION"] = ProVariable("QT_EDITION", VariableType.STRING, [], []);
    temp1["QT_LICHECK"] = ProVariable("QT_LICHECK", VariableType.STRING, [], []);
    temp1["QT_RELEASE_DATE"] = ProVariable("QT_RELEASE_DATE", VariableType.STRING, [], []);

    temp1["QMAKE_EXT_C"] = ProVariable("QMAKE_EXT_C", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_CPP"] = ProVariable("QMAKE_EXT_CPP", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_CPP_MOC"] = ProVariable("QMAKE_EXT_CPP_MOC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_H"] = ProVariable("QMAKE_EXT_H", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_H_MOC"] = ProVariable("QMAKE_EXT_H_MOC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_JS"] = ProVariable("QMAKE_EXT_JS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_LEX"] = ProVariable("QMAKE_EXT_LEX", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_LIBTOOL"] = ProVariable("QMAKE_EXT_LIBTOOL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_OBJ"] = ProVariable("QMAKE_EXT_OBJ", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_OBJC"] = ProVariable("QMAKE_EXT_OBJC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_OBJCXX"] = ProVariable("QMAKE_EXT_OBJCXX", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_PKGCONFIG"] = ProVariable("QMAKE_EXT_PKGCONFIG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_PRL"] = ProVariable("QMAKE_EXT_PRL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_RES"] = ProVariable("QMAKE_EXT_RES", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_UI"] = ProVariable("QMAKE_EXT_UI", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_YACC"] = ProVariable("QMAKE_EXT_YACC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_H_MOD_MOC"] = ProVariable("QMAKE_H_MOD_MOC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CPP_MOD_MOC"] = ProVariable("QMAKE_CPP_MOD_MOC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_MOD_LEX"] = ProVariable("QMAKE_MOD_LEX", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_MOD_YACC"] = ProVariable("QMAKE_MOD_YACC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXT_C"] = ProVariable("QMAKE_EXT_C", VariableType.STRING_LIST, [], []);

	temp1["CONFIG"] = ProVariable("CONFIG", VariableType.RESTRICTED_STRING_LIST, initConfigVariableRange(), initConfigVariableValue());
    temp1["QT_CONFIG"] = ProVariable("QT_CONFIG", VariableType.STRING, initConfigVariableRange(), []);

	temp1["DEFINES"] = ProVariable("DEFINES", VariableType.STRING_LIST, [], []);
	temp1["DESTDIR"] = ProVariable("DESTDIR", VariableType.STRING, [], []);
	temp1["DISTFILES"] = ProVariable("DISTFILES", VariableType.STRING_LIST, [], []);
	temp1["FORMS"] = ProVariable("FORMS", VariableType.STRING_LIST, [], []);
	temp1["HEADERS"] = ProVariable("HEADERS", VariableType.STRING_LIST, [], []);
	temp1["INCLUDEPATH"] = ProVariable("INCLUDEPATH", VariableType.STRING_LIST, [], []);
	temp1["INSTALLS"] = ProVariable("INSTALLS", VariableType.OBJECT_LIST, [], []);
	temp1["LIBS"] = ProVariable("LIBS", VariableType.STRING_LIST, [], []);
	temp1["LITERAL_HASH"] = ProVariable("LITERAL_HASH", VariableType.STRING, [], [STR_HASH]);
	temp1["MOC_DIR"] = ProVariable("MOC_DIR", VariableType.STRING, [], []);
	temp1["OBJECTS"] = ProVariable("OBJECTS", VariableType.STRING_LIST, [], []);
	temp1["OBJECTS_DIR"] = ProVariable("OBJECTS_DIR", VariableType.STRING, [], []);
	temp1["PRECOMPILED_HEADER"] = ProVariable("PRECOMPILED_HEADER", VariableType.STRING, [], []);
	
	temp1["PWD"] = ProVariable("PWD", VariableType.STRING, [], []);
	temp1["OUT_PWD"] = ProVariable("OUT_PWD", VariableType.STRING, [], []);
	temp1["_PRO_FILE_"] = ProVariable("_PRO_FILE_", VariableType.STRING, [], []);
	temp1["_PRO_FILE_PWD_"] = ProVariable("_PRO_FILE_PWD_", VariableType.STRING, [], []);
	
    temp1["QMAKESPEC"] = ProVariable("QMAKE_SPEC", VariableType.STRING_LIST, [], initQmakeSpecVariableValue());
    temp1["MAKEFILE_GENERATOR"] = ProVariable("MAKEFILE_GENERATOR", VariableType.STRING, [], []);
    temp1["TARGET_PLATFORM"] = ProVariable("TARGET_PLATFORM", VariableType.STRING, [], []);

	temp1["QMAKE_CFLAGS"] = ProVariable("QMAKE_CFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_PIC"] = ProVariable("QMAKE_CFLAGS_PIC", VariableType.STRING_LIST, [], []);

    temp1["QMAKE_COMPILER"] = ProVariable("QMAKE_COMPILER", VariableType.RESTRICTED_STRING_LIST, initQmakeCompilerVariableRange(), []);
	temp1["QMAKE_PLATFORM"] = ProVariable("QMAKE_PLATFORM", VariableType.RESTRICTED_STRING_LIST, initQmakePlatformVariableRange(), []);

	temp1["QT"] = ProVariable("QT", VariableType.RESTRICTED_STRING_LIST, initQtVariableRange(), initQtVariableValue());
	temp1["QT_ARCH"] = ProVariable("QT_ARCH", VariableType.RESTRICTED_STRING, initQtArchVariableRange(), []);
	temp1["SOURCES"] = ProVariable("SOURCES", VariableType.STRING_LIST, [], []);
	temp1["SUBDIRS"] = ProVariable("SUBDIRS", VariableType.OBJECT_LIST, [], []);
	temp1["TARGET"] = ProVariable("TARGET", VariableType.STRING, [], []);
	temp1["TEMPLATE"] = ProVariable("TEMPLATE", VariableType.RESTRICTED_STRING, initTemplateVariableRange(), initTemplateVariableValue());
	temp1["UI_DIR"] = ProVariable("UI_DIR", VariableType.STRING, [], []);
	temp1["VERSION"] = ProVariable("VERSION", VariableType.STRING, [], []);
    
    temp1["QMAKE_AR"] = ProVariable("QMAKE_AR", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_AR_LTCG"] = ProVariable("QMAKE_AR_LTCG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CC"] = ProVariable("QMAKE_CC", VariableType.STRING, [], []);
    temp1["QMAKE_CD"] = ProVariable("QMAKE_CD", VariableType.STRING, [], []);
    temp1["QMAKE_CFLAGS_AESNI"] = ProVariable("QMAKE_CFLAGS_AESNI", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_APP"] = ProVariable("QMAKE_CFLAGS_APP", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX"] = ProVariable("QMAKE_CFLAGS_AVX", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX2"] = ProVariable("QMAKE_CFLAGS_AVX2", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX512BW"] = ProVariable("QMAKE_CFLAGS_AVX512BW", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX512CD"] = ProVariable("QMAKE_CFLAGS_AVX512CD", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX512DQ"] = ProVariable("QMAKE_CFLAGS_AVX512DQ", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX512ER"] = ProVariable("QMAKE_CFLAGS_AVX512ER", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX512F"] = ProVariable("QMAKE_CFLAGS_AVX512F", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX512IFMA"] = ProVariable("QMAKE_CFLAGS_AVX512IFMA", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX512PF"] = ProVariable("QMAKE_CFLAGS_AVX512PF", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX512VBMI"] = ProVariable("QMAKE_CFLAGS_AVX512VBMI", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_AVX512VL"] = ProVariable("QMAKE_CFLAGS_AVX512VL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_DEBUG"] = ProVariable("QMAKE_CFLAGS_DEBUG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_DEPS"] = ProVariable("QMAKE_CFLAGS_DEPS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_DISABLE_LTCG"] = ProVariable("QMAKE_CFLAGS_DISABLE_LTCG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_EXCEPTIONS_OFF"] = ProVariable("QMAKE_CFLAGS_EXCEPTIONS_OFF", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_F16C"] = ProVariable("QMAKE_CFLAGS_F16C", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_HIDESYMS"] = ProVariable("QMAKE_CFLAGS_HIDESYMS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_ISYSTEM"] = ProVariable("QMAKE_CFLAGS_ISYSTEM", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_LTCG"] = ProVariable("QMAKE_CFLAGS_LTCG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_LTCG_FATOBJECTS"] = ProVariable("QMAKE_CFLAGS_LTCG_FATOBJECTS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_MIPS_DSP"] = ProVariable("QMAKE_CFLAGS_MIPS_DSP", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_MIPS_DSPR2"] = ProVariable("QMAKE_CFLAGS_MIPS_DSPR2", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_NEON"] = ProVariable("QMAKE_CFLAGS_NEON", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_OPTIMIZE"] = ProVariable("QMAKE_CFLAGS_OPTIMIZE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_OPTIMIZE_DEBUG"] = ProVariable("QMAKE_CFLAGS_OPTIMIZE_DEBUG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_OPTIMIZE_FULL"] = ProVariable("QMAKE_CFLAGS_OPTIMIZE_FULL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_OPTIMIZE_SIZE"] = ProVariable("QMAKE_CFLAGS_OPTIMIZE_SIZE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_PRECOMPILE"] = ProVariable("QMAKE_CFLAGS_PRECOMPILE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_RDRND"] = ProVariable("QMAKE_CFLAGS_RDRND", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_RELEASE"] = ProVariable("QMAKE_CFLAGS_RELEASE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_RELEASE_WITH_DEBUGINFO"] = ProVariable("QMAKE_CFLAGS_RELEASE_WITH_DEBUGINFO", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_SHANI"] = ProVariable("QMAKE_CFLAGS_SHANI", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_SHLIB"] = ProVariable("QMAKE_CFLAGS_SHLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_SPLIT_SECTIONS"] = ProVariable("QMAKE_CFLAGS_SPLIT_SECTIONS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_SSE2"] = ProVariable("QMAKE_CFLAGS_SSE2", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_SSE3"] = ProVariable("QMAKE_CFLAGS_SSE3", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_SSE4_1"] = ProVariable("QMAKE_CFLAGS_SSE4_1", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_SSE4_2"] = ProVariable("QMAKE_CFLAGS_SSE4_2", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_SSSE3"] = ProVariable("QMAKE_CFLAGS_SSSE3", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_STATIC_LIB"] = ProVariable("QMAKE_CFLAGS_STATIC_LIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_THREAD"] = ProVariable("QMAKE_CFLAGS_THREAD", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_USE_PRECOMPILE"] = ProVariable("QMAKE_CFLAGS_USE_PRECOMPILE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_WARN_OFF"] = ProVariable("QMAKE_CFLAGS_WARN_OFF", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_WARN_ON"] = ProVariable("QMAKE_CFLAGS_WARN_ON", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CFLAGS_YACC"] = ProVariable("QMAKE_CFLAGS_YACC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CHK_DIR_EXISTS"] = ProVariable("QMAKE_CHK_DIR_EXISTS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CHK_EXISTS"] = ProVariable("QMAKE_CHK_EXISTS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_COMMON_SANITIZE_CFLAGS"] = ProVariable("QMAKE_COMMON_SANITIZE_CFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_COMMON_SANITIZE_CXXFLAGS"] = ProVariable("QMAKE_COMMON_SANITIZE_CXXFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_COPY"] = ProVariable("QMAKE_COPY", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_COPY_DIR"] = ProVariable("QMAKE_COPY_DIR", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_COPY_FILE"] = ProVariable("QMAKE_COPY_FILE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXX"] = ProVariable("QMAKE_CXX", VariableType.STRING, [], []);
    temp1["QMAKE_CXXFLAGS"] = ProVariable("QMAKE_CXXFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_APP"] = ProVariable("QMAKE_CXXFLAGS_APP", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_CXX11"] = ProVariable("QMAKE_CXXFLAGS_CXX11", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_CXX14"] = ProVariable("QMAKE_CXXFLAGS_CXX14", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_CXX1Z"] = ProVariable("QMAKE_CXXFLAGS_CXX1Z", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_DEBUG"] = ProVariable("QMAKE_CXXFLAGS_DEBUG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_DEPS"] = ProVariable("QMAKE_CXXFLAGS_DEPS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_DISABLE_LTCG"] = ProVariable("QMAKE_CXXFLAGS_DISABLE_LTCG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_EXCEPTIONS_OFF"] = ProVariable("QMAKE_CXXFLAGS_EXCEPTIONS_OFF", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_GNUCXX11"] = ProVariable("QMAKE_CXXFLAGS_GNUCXX11", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_GNUCXX14"] = ProVariable("QMAKE_CXXFLAGS_GNUCXX14", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_GNUCXX1Z"] = ProVariable("QMAKE_CXXFLAGS_GNUCXX1Z", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_HIDESYMS"] = ProVariable("QMAKE_CXXFLAGS_HIDESYMS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_LTCG"] = ProVariable("QMAKE_CXXFLAGS_LTCG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_LTCG_FATOBJECTS"] = ProVariable("QMAKE_CXXFLAGS_LTCG_FATOBJECTS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_PRECOMPILE"] = ProVariable("QMAKE_CXXFLAGS_PRECOMPILE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_RELEASE"] = ProVariable("QMAKE_CXXFLAGS_RELEASE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_RELEASE_WITH_DEBUGINFO"] = ProVariable("QMAKE_CXXFLAGS_RELEASE_WITH_DEBUGINFO", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_SHLIB"] = ProVariable("QMAKE_CXXFLAGS_SHLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_SPLIT_SECTIONS"] = ProVariable("QMAKE_CXXFLAGS_SPLIT_SECTIONS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_STATIC_LIB"] = ProVariable("QMAKE_CXXFLAGS_STATIC_LIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_THREAD"] = ProVariable("QMAKE_CXXFLAGS_THREAD", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_USE_PRECOMPILE"] = ProVariable("QMAKE_CXXFLAGS_USE_PRECOMPILE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_WARN_OFF"] = ProVariable("QMAKE_CXXFLAGS_WARN_OFF", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_WARN_ON"] = ProVariable("QMAKE_CXXFLAGS_WARN_ON", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_CXXFLAGS_YACC"] = ProVariable("QMAKE_CXXFLAGS_YACC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_DEFINES_WAYLAND"] = ProVariable("QMAKE_DEFINES_WAYLAND", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_DEL_DIR"] = ProVariable("QMAKE_DEL_DIR", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_DEL_FILE"] = ProVariable("QMAKE_DEL_FILE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_DEL_TREE"] = ProVariable("QMAKE_DEL_TREE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXTENSION_SHLIB"] = ProVariable("QMAKE_EXTENSION_SHLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_EXTENSION_STATICLIB"] = ProVariable("QMAKE_EXTENSION_STATICLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_GZIP"] = ProVariable("QMAKE_GZIP", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INCDIR"] = ProVariable("QMAKE_INCDIR", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INCDIR_EGL"] = ProVariable("QMAKE_INCDIR_EGL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INCDIR_OPENGL"] = ProVariable("QMAKE_INCDIR_OPENGL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INCDIR_OPENGL_ES2"] = ProVariable("QMAKE_INCDIR_OPENGL_ES2", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INCDIR_OPENVG"] = ProVariable("QMAKE_INCDIR_OPENVG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INCDIR_WAYLAND"] = ProVariable("QMAKE_INCDIR_WAYLAND", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INCDIR_X11"] = ProVariable("QMAKE_INCDIR_X11", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INCREMENTAL_STYLE"] = ProVariable("QMAKE_INCREMENTAL_STYLE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INSTALL_DIR"] = ProVariable("QMAKE_INSTALL_DIR", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INSTALL_FILE"] = ProVariable("QMAKE_INSTALL_FILE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_INSTALL_PROGRAM"] = ProVariable("QMAKE_INSTALL_PROGRAM", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LEX"] = ProVariable("QMAKE_LEX", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LEXFLAGS"] = ProVariable("QMAKE_LEXFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS"] = ProVariable("QMAKE_LFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_APP"] = ProVariable("QMAKE_LFLAGS_APP", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_BSYMBOLIC_FUNC"] = ProVariable("QMAKE_LFLAGS_BSYMBOLIC_FUNC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_CXX11"] = ProVariable("QMAKE_LFLAGS_CXX11", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_CXX14"] = ProVariable("QMAKE_LFLAGS_CXX14", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_CXX1Z"] = ProVariable("QMAKE_LFLAGS_CXX1Z", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_DEBUG"] = ProVariable("QMAKE_LFLAGS_DEBUG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_DYNAMIC_LIST"] = ProVariable("QMAKE_LFLAGS_DYNAMIC_LIST", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_EXCEPTIONS_OFF"] = ProVariable("QMAKE_LFLAGS_EXCEPTIONS_OFF", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_GCSECTIONS"] = ProVariable("QMAKE_LFLAGS_GCSECTIONS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_GDB_INDEX"] = ProVariable("QMAKE_LFLAGS_GDB_INDEX", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_LTCG"] = ProVariable("QMAKE_LFLAGS_LTCG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_NEW_DTAGS"] = ProVariable("QMAKE_LFLAGS_NEW_DTAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_NOUNDEF"] = ProVariable("QMAKE_LFLAGS_NOUNDEF", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_PLUGIN"] = ProVariable("QMAKE_LFLAGS_PLUGIN", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_RELEASE"] = ProVariable("QMAKE_LFLAGS_RELEASE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_RELEASE_WITH_DEBUGINFO"] = ProVariable("QMAKE_LFLAGS_RELEASE_WITH_DEBUGINFO", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_REL_RPATH"] = ProVariable("QMAKE_LFLAGS_REL_RPATH", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_RPATH"] = ProVariable("QMAKE_LFLAGS_RPATH", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_RPATHLINK"] = ProVariable("QMAKE_LFLAGS_RPATHLINK", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_SHLIB"] = ProVariable("QMAKE_LFLAGS_SHLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_SONAME"] = ProVariable("QMAKE_LFLAGS_SONAME", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_THREAD"] = ProVariable("QMAKE_LFLAGS_THREAD", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_USE_GOLD"] = ProVariable("QMAKE_LFLAGS_USE_GOLD", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LFLAGS_VERSION_SCRIPT"] = ProVariable("QMAKE_LFLAGS_VERSION_SCRIPT", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBDIR"] = ProVariable("QMAKE_LIBDIR", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBDIR_EGL"] = ProVariable("QMAKE_LIBDIR_EGL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBDIR_OPENGL"] = ProVariable("QMAKE_LIBDIR_OPENGL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBDIR_OPENGL_ES2"] = ProVariable("QMAKE_LIBDIR_OPENGL_ES2", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBDIR_OPENVG"] = ProVariable("QMAKE_LIBDIR_OPENVG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBDIR_WAYLAND"] = ProVariable("QMAKE_LIBDIR_WAYLAND", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBDIR_X11"] = ProVariable("QMAKE_LIBDIR_X11", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS"] = ProVariable("QMAKE_LIBS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_DYNLOAD"] = ProVariable("QMAKE_LIBS_DYNLOAD", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_EGL"] = ProVariable("QMAKE_LIBS_EGL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_OPENGL"] = ProVariable("QMAKE_LIBS_OPENGL", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_OPENGL_ES2"] = ProVariable("QMAKE_LIBS_OPENGL_ES2", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_OPENVG"] = ProVariable("QMAKE_LIBS_OPENVG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_THREAD"] = ProVariable("QMAKE_LIBS_THREAD", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_VULKAN"] = ProVariable("QMAKE_LIBS_VULKAN", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_WAYLAND_CLIENT"] = ProVariable("QMAKE_LIBS_WAYLAND_CLIENT", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_WAYLAND_SERVER"] = ProVariable("QMAKE_LIBS_WAYLAND_SERVER", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LIBS_X11"] = ProVariable("QMAKE_LIBS_X11", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LINK"] = ProVariable("QMAKE_LINK", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LINK_C"] = ProVariable("QMAKE_LINK_C", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LINK_C_SHLIB"] = ProVariable("QMAKE_LINK_C_SHLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LINK_SHLIB"] = ProVariable("QMAKE_LINK_SHLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_LN_SHLIB"] = ProVariable("QMAKE_LN_SHLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_MKDIR"] = ProVariable("QMAKE_MKDIR", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_MKDIR_CMD"] = ProVariable("QMAKE_MKDIR_CMD", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_MOVE"] = ProVariable("QMAKE_MOVE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_NM"] = ProVariable("QMAKE_NM", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_NM_LTCG"] = ProVariable("QMAKE_NM_LTCG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_OBJCOPY"] = ProVariable("QMAKE_OBJCOPY", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_PCH_OUTPUT_EXT"] = ProVariable("QMAKE_PCH_OUTPUT_EXT", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_PREFIX_SHLIB"] = ProVariable("QMAKE_PREFIX_SHLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_PREFIX_STATICLIB"] = ProVariable("QMAKE_PREFIX_STATICLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_QT_CONFIG"] = ProVariable("QMAKE_QT_CONFIG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_RANLIB"] = ProVariable("QMAKE_RANLIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_RANLIB_LTCG"] = ProVariable("QMAKE_RANLIB_LTCG", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_REL_RPATH_BASE"] = ProVariable("QMAKE_REL_RPATH_BASE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_ADDRESS_CFLAGS"] = ProVariable("QMAKE_SANITIZE_ADDRESS_CFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_ADDRESS_CXXFLAGS"] = ProVariable("QMAKE_SANITIZE_ADDRESS_CXXFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_ADDRESS_LFLAGS"] = ProVariable("QMAKE_SANITIZE_ADDRESS_LFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_MEMORY_CFLAGS"] = ProVariable("QMAKE_SANITIZE_MEMORY_CFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_MEMORY_CXXFLAGS"] = ProVariable("QMAKE_SANITIZE_MEMORY_CXXFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_MEMORY_LFLAGS"] = ProVariable("QMAKE_SANITIZE_MEMORY_LFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_THREAD_CFLAGS"] = ProVariable("QMAKE_SANITIZE_THREAD_CFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_THREAD_CXXFLAGS"] = ProVariable("QMAKE_SANITIZE_THREAD_CXXFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_THREAD_LFLAGS"] = ProVariable("QMAKE_SANITIZE_THREAD_LFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_UNDEFINED_CFLAGS"] = ProVariable("QMAKE_SANITIZE_UNDEFINED_CFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_UNDEFINED_CXXFLAGS"] = ProVariable("QMAKE_SANITIZE_UNDEFINED_CXXFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SANITIZE_UNDEFINED_LFLAGS"] = ProVariable("QMAKE_SANITIZE_UNDEFINED_LFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SHELL_NULL_DEVICE"] = ProVariable("QMAKE_SHELL_NULL_DEVICE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_STREAM_EDITOR"] = ProVariable("QMAKE_STREAM_EDITOR", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_STRIP"] = ProVariable("QMAKE_STRIP", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_STRIPFLAGS_LIB"] = ProVariable("QMAKE_STRIPFLAGS_LIB", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SYMBOLIC_LINK"] = ProVariable("QMAKE_SYMBOLIC_LINK", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_SYSTEM_NULL_DEVICE"] = ProVariable("QMAKE_SYSTEM_NULL_DEVICE", VariableType.STRING, [], []);
    temp1["QMAKE_TAR"] = ProVariable("QMAKE_TAR", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_WAYLAND_SCANNER"] = ProVariable("QMAKE_WAYLAND_SCANNER", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_YACC"] = ProVariable("QMAKE_YACC", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_YACCFLAGS"] = ProVariable("QMAKE_YACCFLAGS", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_YACCFLAGS_MANGLE"] = ProVariable("QMAKE_YACCFLAGS_MANGLE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_YACC_HEADER"] = ProVariable("QMAKE_YACC_HEADER", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_YACC_SOURCE"] = ProVariable("QMAKE_YACC_SOURCE", VariableType.STRING_LIST, [], []);
    temp1["QMAKE_ZIP"] = ProVariable("QMAKE_ZIP", VariableType.STRING_LIST, [], []);

    temp1.rehash; // for faster lookups
    builtinVariables = assumeUnique(temp1);
}
