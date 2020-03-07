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

module source.persistent_property;

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
import std.regex;
import std.process;

import source.common_const;
import source.qmakeexception;
import source.qt;

public:

class PersistentPropertyStorage
{
private:
    string[string] m_values;

public:
    @disable this();

    this(immutable QtVersionInfo qtInfo, const string mkSpec)
    {
        reload(qtInfo, mkSpec);
    }

    void reload(immutable QtVersionInfo qtInfo, const string mkSpec)
    {
        m_values["QT_SYSROOT"] = "";

        m_values["QT_INSTALL_PREFIX"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_PREFIX/raw"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_PREFIX/get"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_PREFIX/src"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_PREFIX/dev"] = qtInfo.qtPlatformDir;
        
        m_values["QT_INSTALL_ARCHDATA"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_ARCHDATA/raw"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_ARCHDATA/get"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_ARCHDATA/src"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_ARCHDATA/dev"] = qtInfo.qtPlatformDir;
        
        m_values["QT_INSTALL_DATA"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_DATA/raw"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_DATA/get"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_DATA/src"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_DATA/dev"] = qtInfo.qtPlatformDir;
        
        m_values["QT_INSTALL_DOCS"] = qtInfo.qtDocsDir;
        m_values["QT_INSTALL_DOCS/raw"] = qtInfo.qtDocsDir;
        m_values["QT_INSTALL_DOCS/get"] = qtInfo.qtDocsDir;
        m_values["QT_INSTALL_DOCS/src"] = qtInfo.qtDocsDir;
        m_values["QT_INSTALL_DOCS/dev"] = qtInfo.qtDocsDir;
        
        m_values["QT_INSTALL_HEADERS"] = qtInfo.qtIncludeDir;
        m_values["QT_INSTALL_HEADERS/raw"] = qtInfo.qtIncludeDir;
        m_values["QT_INSTALL_HEADERS/get"] = qtInfo.qtIncludeDir;
        m_values["QT_INSTALL_HEADERS/src"] = qtInfo.qtIncludeDir;
        m_values["QT_INSTALL_HEADERS/dev"] = qtInfo.qtIncludeDir;
        
        m_values["QT_INSTALL_LIBS"] = qtInfo.qtLibraryDir;
        m_values["QT_INSTALL_LIBS/raw"] = qtInfo.qtLibraryDir;
        m_values["QT_INSTALL_LIBS/get"] = qtInfo.qtLibraryDir;
        m_values["QT_INSTALL_LIBS/src"] = qtInfo.qtLibraryDir;
        m_values["QT_INSTALL_LIBS/dev"] = qtInfo.qtLibraryDir;
        
        m_values["QT_INSTALL_LIBEXECS"] = qtInfo.qtLibexecDir;
        m_values["QT_INSTALL_LIBEXECS/raw"] = qtInfo.qtLibexecDir;
        m_values["QT_INSTALL_LIBEXECS/get"] = qtInfo.qtLibexecDir;
        m_values["QT_INSTALL_LIBEXECS/src"] = qtInfo.qtLibexecDir;
        m_values["QT_INSTALL_LIBEXECS/dev"] = qtInfo.qtLibexecDir;
        
        m_values["QT_INSTALL_BINS"] = qtInfo.qtBinaryDir;
        m_values["QT_INSTALL_BINS/raw"] = qtInfo.qtBinaryDir;
        m_values["QT_INSTALL_BINS/get"] = qtInfo.qtBinaryDir;
        m_values["QT_INSTALL_BINS/src"] = qtInfo.qtBinaryDir;
        m_values["QT_INSTALL_BINS/dev"] = qtInfo.qtBinaryDir;
        
        m_values["QT_INSTALL_TESTS"] = qtInfo.qtTestsDir;
        m_values["QT_INSTALL_TESTS/raw"] = qtInfo.qtTestsDir;
        m_values["QT_INSTALL_TESTS/get"] = qtInfo.qtTestsDir;
        m_values["QT_INSTALL_TESTS/src"] = qtInfo.qtTestsDir;
        m_values["QT_INSTALL_TESTS/dev"] = qtInfo.qtTestsDir;
        
        m_values["QT_INSTALL_PLUGINS"] = qtInfo.qtPluginsDir;
        m_values["QT_INSTALL_PLUGINS/raw"] = qtInfo.qtPluginsDir;
        m_values["QT_INSTALL_PLUGINS/get"] = qtInfo.qtPluginsDir;
        m_values["QT_INSTALL_PLUGINS/src"] = qtInfo.qtPluginsDir;
        m_values["QT_INSTALL_PLUGINS/dev"] = qtInfo.qtPluginsDir;
        
        m_values["QT_INSTALL_IMPORTS"] = qtInfo.qtImportsDir;
        m_values["QT_INSTALL_IMPORTS/raw"] = qtInfo.qtImportsDir;
        m_values["QT_INSTALL_IMPORTS/get"] = qtInfo.qtImportsDir;
        m_values["QT_INSTALL_IMPORTS/src"] = qtInfo.qtImportsDir;
        m_values["QT_INSTALL_IMPORTS/dev"] = qtInfo.qtImportsDir;

        m_values["QT_INSTALL_QML"] = qtInfo.qtQmlDir;
        m_values["QT_INSTALL_QML/raw"] = qtInfo.qtQmlDir;
        m_values["QT_INSTALL_QML/get"] = qtInfo.qtQmlDir;
        m_values["QT_INSTALL_QML/src"] = qtInfo.qtQmlDir;
        m_values["QT_INSTALL_QML/dev"] = qtInfo.qtQmlDir;

        m_values["QT_INSTALL_TRANSLATIONS"] = qtInfo.qtTranslationsDir;
        m_values["QT_INSTALL_TRANSLATIONS/raw"] = qtInfo.qtTranslationsDir;
        m_values["QT_INSTALL_TRANSLATIONS/get"] = qtInfo.qtTranslationsDir;
        m_values["QT_INSTALL_TRANSLATIONS/src"] = qtInfo.qtTranslationsDir;
        m_values["QT_INSTALL_TRANSLATIONS/dev"] = qtInfo.qtTranslationsDir;

        m_values["QT_INSTALL_CONFIGURATION"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_CONFIGURATION/raw"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_CONFIGURATION/get"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_CONFIGURATION/src"] = qtInfo.qtPlatformDir;
        m_values["QT_INSTALL_CONFIGURATION/dev"] = qtInfo.qtPlatformDir;

        m_values["QT_INSTALL_EXAMPLES"] = qtInfo.qtExamplesDir;
        m_values["QT_INSTALL_EXAMPLES/raw"] = qtInfo.qtExamplesDir;
        m_values["QT_INSTALL_EXAMPLES/get"] = qtInfo.qtExamplesDir;
        m_values["QT_INSTALL_EXAMPLES/src"] = qtInfo.qtExamplesDir;
        m_values["QT_INSTALL_EXAMPLES/dev"] = qtInfo.qtExamplesDir;

        m_values["QT_INSTALL_DEMOS"] = qtInfo.qtExamplesDir;
        m_values["QT_INSTALL_DEMOS/raw"] = qtInfo.qtExamplesDir;
        m_values["QT_INSTALL_DEMOS/get"] = qtInfo.qtExamplesDir;
        m_values["QT_INSTALL_DEMOS/src"] = qtInfo.qtExamplesDir;
        m_values["QT_INSTALL_DEMOS/dev"] = qtInfo.qtExamplesDir;
        
        m_values["QT_HOST_PREFIX"] = qtInfo.qtPlatformDir;
        m_values["QT_HOST_PREFIX/get"] = qtInfo.qtPlatformDir;
        m_values["QT_HOST_PREFIX/src"] = qtInfo.qtPlatformDir;

        m_values["QT_HOST_DATA"] = qtInfo.qtPlatformDir;
        m_values["QT_HOST_DATA/get"] = qtInfo.qtPlatformDir;
        m_values["QT_HOST_DATA/src"] = qtInfo.qtPlatformDir;

        m_values["QT_HOST_BINS"] = qtInfo.qtBinaryDir;
        m_values["QT_HOST_BINS/get"] = qtInfo.qtBinaryDir;
        m_values["QT_HOST_BINS/src"] = qtInfo.qtBinaryDir;

        m_values["QT_HOST_LIBS"] = qtInfo.qtLibraryDir;
        m_values["QT_HOST_LIBS/get"] = qtInfo.qtLibraryDir;
        m_values["QT_HOST_LIBS/src"] = qtInfo.qtLibraryDir;

        m_values["QMAKE_SPEC"] = mkSpec;
        m_values["QMAKE_XSPEC"] = mkSpec;
        m_values["QMAKE_VERSION"] = QMAKE_VERSION_STR;
        m_values["QT_VERSION"] = qtInfo.qtVersionStr;

        // Internal for mkspec/feature eval
        m_values["QMAKEMODULES"] = "";
        m_values["QMAKE_MKSPECS"] = qtInfo.qtMkspecsDir;

        m_values["CROSS_COMPILE"] = "";
    }

    string value(const string name)
    {
        if ((name in m_values) !is null)
            return m_values[name];

        // FIXME: implement
        throw new NotImplementedException("Persistent storage not implemented yet");
    }

    bool hasValue(const string v)
    {
        return ((v in m_values) !is null);
    }

    void setValue(const string var, const string val)
    {
        // FIXME: implement
        throw new NotImplementedException("Persistent storage not implemented yet");
    }

    void remove(const string var)
    {
        // FIXME: implement
        throw new NotImplementedException("Persistent storage not implemented yet");
    }
}

struct PropertyDescription
{
    string name;
    QLibraryInfo.LibraryLocation loc;
    bool raw;
    bool singular;

    this(string name, QLibraryInfo.LibraryLocation loc, bool raw, bool singular)
    {
        this.name = name;
        this.loc = loc;
        this.raw = raw;
        this.singular = singular;
    }
}

private:

static PropertyDescription[] propList = [
    PropertyDescription("QT_SYSROOT", QLibraryInfo.LibraryLocation.SysrootPath, true, true),
    PropertyDescription("QT_INSTALL_PREFIX", QLibraryInfo.LibraryLocation.PrefixPath, false, false),
    PropertyDescription("QT_INSTALL_ARCHDATA", QLibraryInfo.LibraryLocation.ArchDataPath, false, false),
    PropertyDescription("QT_INSTALL_DATA", QLibraryInfo.LibraryLocation.DataPath, false, false),
    PropertyDescription("QT_INSTALL_DOCS", QLibraryInfo.LibraryLocation.DocumentationPath, false, false),
    PropertyDescription("QT_INSTALL_HEADERS", QLibraryInfo.LibraryLocation.HeadersPath, false, false),
    PropertyDescription("QT_INSTALL_LIBS", QLibraryInfo.LibraryLocation.LibrariesPath, false, false),
    PropertyDescription("QT_INSTALL_LIBEXECS", QLibraryInfo.LibraryLocation.LibraryExecutablesPath, false, false),
    PropertyDescription("QT_INSTALL_BINS", QLibraryInfo.LibraryLocation.BinariesPath, false, false),
    PropertyDescription("QT_INSTALL_TESTS", QLibraryInfo.LibraryLocation.TestsPath, false, false),
    PropertyDescription("QT_INSTALL_PLUGINS", QLibraryInfo.LibraryLocation.PluginsPath, false, false),
    PropertyDescription("QT_INSTALL_IMPORTS", QLibraryInfo.LibraryLocation.ImportsPath, false, false),
    PropertyDescription("QT_INSTALL_QML", QLibraryInfo.LibraryLocation.Qml2ImportsPath, false, false),
    PropertyDescription("QT_INSTALL_TRANSLATIONS", QLibraryInfo.LibraryLocation.TranslationsPath, false, false),
    PropertyDescription("QT_INSTALL_CONFIGURATION", QLibraryInfo.LibraryLocation.SettingsPath, false, false),
    PropertyDescription("QT_INSTALL_EXAMPLES", QLibraryInfo.LibraryLocation.ExamplesPath, false, false),
    PropertyDescription("QT_INSTALL_DEMOS", QLibraryInfo.LibraryLocation.ExamplesPath, false, false), // Just backwards compat
    PropertyDescription("QT_HOST_PREFIX", QLibraryInfo.LibraryLocation.HostPrefixPath, true, false),
    PropertyDescription("QT_HOST_DATA", QLibraryInfo.LibraryLocation.HostDataPath, true, false),
    PropertyDescription("QT_HOST_BINS", QLibraryInfo.LibraryLocation.HostBinariesPath, true, false),
    PropertyDescription("QT_HOST_LIBS", QLibraryInfo.LibraryLocation.HostLibrariesPath, true, false),
    PropertyDescription("QMAKE_SPEC", QLibraryInfo.LibraryLocation.HostSpecPath, true, true),
    PropertyDescription("QMAKE_XSPEC", QLibraryInfo.LibraryLocation.TargetSpecPath, true, true)
];

class QLibraryInfo
{
    enum LibraryLocation
    {
        PrefixPath = 0,
        DocumentationPath,
        HeadersPath,
        LibrariesPath,
        LibraryExecutablesPath,
        BinariesPath,
        PluginsPath,
        ImportsPath,
        Qml2ImportsPath,
        ArchDataPath,
        DataPath,
        TranslationsPath,
        ExamplesPath,
        TestsPath,
        // Insert new values above this line
        // Please read the comments in qlibraryinfo.cpp before adding
        // These are not subject to binary compatibility constraints
        SysrootPath,
        SysrootifyPrefixPath,
        HostBinariesPath,
        HostLibrariesPath,
        HostDataPath,
        TargetSpecPath,
        HostSpecPath,
        HostPrefixPath,
        LastHostPath = HostPrefixPath,
        SettingsPath = 100
    }
    enum PathGroup { FinalPaths, EffectivePaths, EffectiveSourcePaths, DevicePaths }
}
