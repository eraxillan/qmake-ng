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

module persistent_property;
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
import common_const;
import qmakeexception;

class PersistentPropertyStorage
{
    private string[string] m_values;

    this()
    {
        reload();
    }

    public void reload()
    {
        m_values["QT_SYSROOT"] = "";

        m_values["QT_INSTALL_PREFIX"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_PREFIX/raw"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_PREFIX/get"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_PREFIX/src"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_PREFIX/dev"] = "/opt/Qt/5.11.1/gcc_64";
        
        m_values["QT_INSTALL_ARCHDATA"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_ARCHDATA/raw"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_ARCHDATA/get"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_ARCHDATA/src"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_ARCHDATA/dev"] = "/opt/Qt/5.11.1/gcc_64";
        
        m_values["QT_INSTALL_DATA"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_DATA/raw"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_DATA/get"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_DATA/src"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_DATA/dev"] = "/opt/Qt/5.11.1/gcc_64";
        
        m_values["QT_INSTALL_DOCS"] = "/opt/Qt/Docs/Qt-5.11.1";
        m_values["QT_INSTALL_DOCS/raw"] = "/opt/Qt/Docs/Qt-5.11.1";
        m_values["QT_INSTALL_DOCS/get"] = "/opt/Qt/Docs/Qt-5.11.1";
        m_values["QT_INSTALL_DOCS/src"] = "/opt/Qt/Docs/Qt-5.11.1";
        m_values["QT_INSTALL_DOCS/dev"] = "/opt/Qt/Docs/Qt-5.11.1";
        
        m_values["QT_INSTALL_HEADERS"] = "/opt/Qt/5.11.1/gcc_64/include";
        m_values["QT_INSTALL_HEADERS/raw"] = "/opt/Qt/5.11.1/gcc_64/include";
        m_values["QT_INSTALL_HEADERS/get"] = "/opt/Qt/5.11.1/gcc_64/include";
        m_values["QT_INSTALL_HEADERS/src"] = "/opt/Qt/5.11.1/gcc_64/include";
        m_values["QT_INSTALL_HEADERS/dev"] = "/opt/Qt/5.11.1/gcc_64/include";
        
        m_values["QT_INSTALL_LIBS"] = "/opt/Qt/5.11.1/gcc_64/lib";
        m_values["QT_INSTALL_LIBS/raw"] = "/opt/Qt/5.11.1/gcc_64/lib";
        m_values["QT_INSTALL_LIBS/get"] = "/opt/Qt/5.11.1/gcc_64/lib";
        m_values["QT_INSTALL_LIBS/src"] = "/opt/Qt/5.11.1/gcc_64/lib";
        m_values["QT_INSTALL_LIBS/dev"] = "/opt/Qt/5.11.1/gcc_64/lib";
        
        m_values["QT_INSTALL_LIBEXECS"] = "/opt/Qt/5.11.1/gcc_64/libexec";
        m_values["QT_INSTALL_LIBEXECS/raw"] = "/opt/Qt/5.11.1/gcc_64/libexec";
        m_values["QT_INSTALL_LIBEXECS/get"] = "/opt/Qt/5.11.1/gcc_64/libexec";
        m_values["QT_INSTALL_LIBEXECS/src"] = "/opt/Qt/5.11.1/gcc_64/libexec";
        m_values["QT_INSTALL_LIBEXECS/dev"] = "/opt/Qt/5.11.1/gcc_64/libexec";
        
        m_values["QT_INSTALL_BINS"] = "/opt/Qt/5.11.1/gcc_64/bin";
        m_values["QT_INSTALL_BINS/raw"] = "/opt/Qt/5.11.1/gcc_64/bin";
        m_values["QT_INSTALL_BINS/get"] = "/opt/Qt/5.11.1/gcc_64/bin";
        m_values["QT_INSTALL_BINS/src"] = "/opt/Qt/5.11.1/gcc_64/bin";
        m_values["QT_INSTALL_BINS/dev"] = "/opt/Qt/5.11.1/gcc_64/bin";
        
        m_values["QT_INSTALL_TESTS"] = "/opt/Qt/5.11.1/gcc_64/tests";
        m_values["QT_INSTALL_TESTS/raw"] = "/opt/Qt/5.11.1/gcc_64/tests";
        m_values["QT_INSTALL_TESTS/get"] = "/opt/Qt/5.11.1/gcc_64/tests";
        m_values["QT_INSTALL_TESTS/src"] = "/opt/Qt/5.11.1/gcc_64/tests";
        m_values["QT_INSTALL_TESTS/dev"] = "/opt/Qt/5.11.1/gcc_64/tests";
        
        m_values["QT_INSTALL_PLUGINS"] = "/opt/Qt/5.11.1/gcc_64/plugins";
        m_values["QT_INSTALL_PLUGINS/raw"] = "/opt/Qt/5.11.1/gcc_64/plugins";
        m_values["QT_INSTALL_PLUGINS/get"] = "/opt/Qt/5.11.1/gcc_64/plugins";
        m_values["QT_INSTALL_PLUGINS/src"] = "/opt/Qt/5.11.1/gcc_64/plugins";
        m_values["QT_INSTALL_PLUGINS/dev"] = "/opt/Qt/5.11.1/gcc_64/plugins";
        
        m_values["QT_INSTALL_IMPORTS"] = "/opt/Qt/5.11.1/gcc_64/imports";
        m_values["QT_INSTALL_IMPORTS/raw"] = "/opt/Qt/5.11.1/gcc_64/imports";
        m_values["QT_INSTALL_IMPORTS/get"] = "/opt/Qt/5.11.1/gcc_64/imports";
        m_values["QT_INSTALL_IMPORTS/src"] = "/opt/Qt/5.11.1/gcc_64/imports";
        m_values["QT_INSTALL_IMPORTS/dev"] = "/opt/Qt/5.11.1/gcc_64/imports";

        m_values["QT_INSTALL_QML"] = "/opt/Qt/5.11.1/gcc_64/qml";
        m_values["QT_INSTALL_QML/raw"] = "/opt/Qt/5.11.1/gcc_64/qml";
        m_values["QT_INSTALL_QML/get"] = "/opt/Qt/5.11.1/gcc_64/qml";
        m_values["QT_INSTALL_QML/src"] = "/opt/Qt/5.11.1/gcc_64/qml";
        m_values["QT_INSTALL_QML/dev"] = "/opt/Qt/5.11.1/gcc_64/qml";

        m_values["QT_INSTALL_TRANSLATIONS"] = "/opt/Qt/5.11.1/gcc_64/translations";
        m_values["QT_INSTALL_TRANSLATIONS/raw"] = "/opt/Qt/5.11.1/gcc_64/translations";
        m_values["QT_INSTALL_TRANSLATIONS/get"] = "/opt/Qt/5.11.1/gcc_64/translations";
        m_values["QT_INSTALL_TRANSLATIONS/src"] = "/opt/Qt/5.11.1/gcc_64/translations";
        m_values["QT_INSTALL_TRANSLATIONS/dev"] = "/opt/Qt/5.11.1/gcc_64/translations";

        m_values["QT_INSTALL_CONFIGURATION"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_CONFIGURATION/raw"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_CONFIGURATION/get"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_CONFIGURATION/src"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_INSTALL_CONFIGURATION/dev"] = "/opt/Qt/5.11.1/gcc_64";

        m_values["QT_INSTALL_EXAMPLES"] = "/opt/Qt/Examples/Qt-5.11.1";
        m_values["QT_INSTALL_EXAMPLES/raw"] = "/opt/Qt/Examples/Qt-5.11.1";
        m_values["QT_INSTALL_EXAMPLES/get"] = "/opt/Qt/Examples/Qt-5.11.1";
        m_values["QT_INSTALL_EXAMPLES/src"] = "/opt/Qt/Examples/Qt-5.11.1";
        m_values["QT_INSTALL_EXAMPLES/dev"] = "/opt/Qt/Examples/Qt-5.11.1";

        m_values["QT_INSTALL_DEMOS"] = "/opt/Qt/Examples/Qt-5.11.1";
        m_values["QT_INSTALL_DEMOS/raw"] = "/opt/Qt/Examples/Qt-5.11.1";
        m_values["QT_INSTALL_DEMOS/get"] = "/opt/Qt/Examples/Qt-5.11.1";
        m_values["QT_INSTALL_DEMOS/src"] = "/opt/Qt/Examples/Qt-5.11.1";
        m_values["QT_INSTALL_DEMOS/dev"] = "/opt/Qt/Examples/Qt-5.11.1";
        
        m_values["QT_HOST_PREFIX"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_HOST_PREFIX/get"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_HOST_PREFIX/src"] = "/opt/Qt/5.11.1/gcc_64";

        m_values["QT_HOST_DATA"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_HOST_DATA/get"] = "/opt/Qt/5.11.1/gcc_64";
        m_values["QT_HOST_DATA/src"] = "/opt/Qt/5.11.1/gcc_64";

        m_values["QT_HOST_BINS"] = "/opt/Qt/5.11.1/gcc_64/bin";
        m_values["QT_HOST_BINS/get"] = "/opt/Qt/5.11.1/gcc_64/bin";
        m_values["QT_HOST_BINS/src"] = "/opt/Qt/5.11.1/gcc_64/bin";

        m_values["QT_HOST_LIBS"] = "/opt/Qt/5.11.1/gcc_64/lib";
        m_values["QT_HOST_LIBS/get"] = "/opt/Qt/5.11.1/gcc_64/lib";
        m_values["QT_HOST_LIBS/src"] = "/opt/Qt/5.11.1/gcc_64/lib";

        m_values["QMAKE_SPEC"] = "linux-g++";
        m_values["QMAKE_XSPEC"] = "linux-g++";
        m_values["QMAKE_VERSION"] = "3.1";
        m_values["QT_VERSION"] = "5.11.1";

        // Internal for mkspec/feature eval
        m_values["QMAKEMODULES"] = "";
        m_values["QMAKE_MKSPECS"] = "/opt/Qt/5.11.1/gcc_64/mkspecs";
    }

    public string value(in string name)
    {
        if ((name in m_values) !is null)
            return m_values[name];

        // FIXME: implement
        throw new NotImplementedException("Persistent storage not implemented yet");
    }

    public bool hasValue(in string v)
    {
        return ((v in m_values) !is null);
    }

    public void setValue(in string var, in string val)
    {
        // FIXME: implement
        throw new NotImplementedException("Persistent storage not implemented yet");
    }

    public void remove(in string var)
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

private static PropertyDescription[] propList = [
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
