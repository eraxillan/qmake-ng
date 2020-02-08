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

import std.experimental.logger;

static import std.file;
static import std.path;
import std.stdio;
import std.range;
import std.conv;
import std.string;
import common_const;
import common_utils;
import project_context;
import project_variable;

void setupQtEnvironmentVariables(in QtVersionInfo qtInfo)
{
    import std.process: environment;

    // Set Qt-specific environment variables like Qt Creator do
    environment["QTDIR"] = qtInfo.qtPlatformDir;
    environment["PATH"] = qtInfo.qtBinaryDir ~ std.path.pathSeparator ~ environment["PATH"];
    environment["DYLD_FRAMEWORK_PATH"] = qtInfo.qtLibraryDir;
    environment["DYLD_LIBRARY_PATH"] = qtInfo.qtLibraryDir;
}

void setupQtProjectVariables(ref ProExecutionContext context, in QtVersionInfo qtInfo, in string mkSpec)
{
    const(string) qtMkspecsDir = std.path.buildPath(qtInfo.qtPlatformDir, MKSPECS_DIR, mkSpec);
    const(string) qtFeaturesDir = std.path.buildPath(qtInfo.qtPlatformDir, MKSPECS_DIR, FEATURES_DIR);
    assert(std.file.exists(qtMkspecsDir) && std.file.isDir(qtMkspecsDir));
    assert(std.file.exists(qtFeaturesDir) && std.file.isDir(qtFeaturesDir));

    context.assignVariable("QMAKESPEC", [qtMkspecsDir], VariableType.STRING_LIST);
    // QMAKE_NG_EXTENSION
    context.assignVariable("QMAKESPEC_FEATURES", [qtFeaturesDir], VariableType.STRING_LIST);
}

QtVersionInfo getQtVersion(in string qtBinaryDir)
out (result)
{
    assert(std.file.exists(result.qtRootDir) && std.file.isDir(result.qtRootDir));
    assert(std.file.exists(result.qtPlatformDir) && std.file.isDir(result.qtPlatformDir));
    assert(std.file.exists(result.qtMkspecsDir) && std.file.isDir(result.qtMkspecsDir));
    assert(std.file.exists(result.qtBinaryDir) && std.file.isDir(result.qtBinaryDir));
    assert(std.file.exists(result.qtLibraryDir) && std.file.isDir(result.qtLibraryDir));
    assert(std.file.exists(result.qtIncludeDir) && std.file.isDir(result.qtIncludeDir));
    // NOTE: libexec, tests, imports dirs can be absent
    // assert(std.file.exists(result.qtLibexecDir) && std.file.isDir(result.qtLibexecDir));
    // assert(std.file.exists(result.qtTestsDir) && std.file.isDir(result.qtTestsDir));
    assert(std.file.exists(result.qtPluginsDir) && std.file.isDir(result.qtPluginsDir));
    // assert(std.file.exists(result.qtImportsDir) && std.file.isDir(result.qtImportsDir));
    assert(std.file.exists(result.qtQmlDir) && std.file.isDir(result.qtQmlDir));
    assert(std.file.exists(result.qtTranslationsDir) && std.file.isDir(result.qtTranslationsDir));

    assert(std.file.exists(result.qtDocsDir) && std.file.isDir(result.qtDocsDir));
    assert(std.file.exists(result.qtExamplesDir) && std.file.isDir(result.qtExamplesDir));

    /*assert(std.file.exists(result.) && std.file.isDir(result.));
    assert(std.file.exists(result.) && std.file.isDir(result.));
    assert(std.file.exists(result.) && std.file.isDir(result.));
    assert(std.file.exists(result.) && std.file.isDir(result.));*/
}
do
{
    // Evaluate other significant Qt directories
    string qtRootDir = std.path.buildNormalizedPath(qtBinaryDir, "..", "..", "..");
    string qtPlatformDir = std.path.buildNormalizedPath(qtBinaryDir, "..");

    string qtMkspecsDir = std.path.buildNormalizedPath(qtPlatformDir, MKSPECS_DIR);
    string qtLibraryDir = std.path.buildNormalizedPath(qtPlatformDir, "lib");
    string qtIncludeDir = std.path.buildNormalizedPath(qtPlatformDir, "include");
    string qtLibexecDir = std.path.buildNormalizedPath(qtPlatformDir, "libexec");
    string qtTestsDir = std.path.buildNormalizedPath(qtPlatformDir, "tests");
    string qtPluginsDir = std.path.buildNormalizedPath(qtPlatformDir, "plugins");
    string qtImportsDir = std.path.buildNormalizedPath(qtPlatformDir, "imports");
    string qtQmlDir = std.path.buildNormalizedPath(qtPlatformDir, "qml");
    string qtTranslationsDir = std.path.buildNormalizedPath(qtPlatformDir, "translations");

    string qtVersionStr = std.path.baseName(std.path.buildNormalizedPath(qtPlatformDir, ".."));
    string qtPlatformStr = std.path.baseName(qtPlatformDir);

    string qtDocsDir = std.path.buildNormalizedPath(qtRootDir, "Docs", "Qt-" ~ qtVersionStr);
    string qtExamplesDir = std.path.buildNormalizedPath(qtRootDir, "Examples", "Qt-" ~ qtVersionStr);

    writefln("Qt root dir = " ~ qtRootDir);
    writefln("Qt platform dir = " ~ qtPlatformDir);
    writefln("Qt mkspecs dir = " ~ qtMkspecsDir);
    writefln("Qt binary dir = " ~ qtBinaryDir);
    writefln("Qt library dir = " ~ qtLibraryDir);
    writefln("Qt include dir = " ~ qtIncludeDir);
    writefln("Qt libexec dir = " ~ qtLibexecDir);
    writefln("Qt tests dir = " ~ qtTestsDir);
    writefln("Qt plugins dir = " ~ qtPluginsDir);
    writefln("Qt imports dir = " ~ qtImportsDir);
    writefln("Qt qml dir = " ~ qtQmlDir);
    writefln("Qt translations dir = " ~ qtTranslationsDir);

    writefln("Qt Docs dir = " ~ qtDocsDir);
    writefln("Qt Examples dir = " ~ qtExamplesDir);

    writefln("Qt version = " ~ qtVersionStr);
    writefln("Qt platform = " ~ qtPlatformStr);

    assert(!qtLibraryDir.empty && std.file.exists(qtLibraryDir) && std.file.isDir(qtLibraryDir),
        "Invalid Qt library directory path");
    assert(!qtPlatformDir.empty && std.file.exists(qtPlatformDir) && std.file.isDir(qtPlatformDir),
        "Invalid Qt platform directory path");
    assert(!qtRootDir.empty && std.file.exists(qtRootDir) && std.file.isDir(qtRootDir),
        "Invalid Qt root directory path");
    assert(splitString(qtVersionStr, ".", true).length == 3,
        "Invalid Qt version");
    assert(!qtPlatformStr.empty,
        "Invalid Qt platform");

    QtVersionInfo result;
    result.qtRootDir = qtRootDir;
    result.qtPlatformDir = qtPlatformDir;
    result.qtMkspecsDir = qtMkspecsDir;
    result.qtBinaryDir = qtBinaryDir;
    result.qtLibraryDir = qtLibraryDir;
    result.qtIncludeDir = qtIncludeDir;
    result.qtLibexecDir = qtLibexecDir;
    result.qtTestsDir = qtTestsDir;
    result.qtPluginsDir = qtPluginsDir;
    result.qtImportsDir = qtImportsDir;
    result.qtQmlDir = qtQmlDir;
    result.qtTranslationsDir = qtTranslationsDir;

    result.qtDocsDir = qtDocsDir;
    result.qtExamplesDir = qtExamplesDir;

    result.qtVersionStr = qtVersionStr;
    result.qtPlatformStr = qtPlatformStr;

    return result;
}

QtVersionInfo chooseQtVersion()
{
    string qtBinaryDir, qtLibraryDir, qtPlatformDir, qtRootDir;
    string qtPlatformStr;
    string qtVersionStr;

    auto qtInstallationDirs = detectQmakeInstallations();
    if (qtInstallationDirs.length == 1)
    {
        writefln("Auto-detected Qt binary directory path: %s", std.path.dirName(qtInstallationDirs[0].fullPath));
        writefln("Enter 'yes' to accept detected dir or your custom path instead:");
        qtBinaryDir = stdin.readln().strip();
        if (qtBinaryDir == "yes")
            qtBinaryDir = std.path.dirName(qtInstallationDirs[0].fullPath);
        else
            qtBinaryDir = stdin.readln().strip();
    }
    else if (qtInstallationDirs.length > 1)
    {
        writefln("More than one Qt binary directory detected:");
        for (int i = 1; i <= qtInstallationDirs.length; i++)
        {
            writefln("[%d] %s", i, std.path.dirName(qtInstallationDirs[i - 1].fullPath));
        }
        writefln("Enter number of Qt binary directory or your custom path instead:");
        qtBinaryDir = stdin.readln().strip();
        if (isNumeric(qtBinaryDir, 10))
        {
            int index = std.conv.to!int(qtBinaryDir, 10);
            assert(index >= 1 && index <= qtInstallationDirs.length);
            qtBinaryDir = std.path.dirName(qtInstallationDirs[index - 1].fullPath);
        }
    }
    else
    {
        writefln("Enter Qt source root directory path (either git repo or Src directory from binary package):");
        qtBinaryDir = stdin.readln().strip();
    }

    assert(qtBinaryDir !is null && !qtBinaryDir.empty
        && std.file.exists(qtBinaryDir) && std.file.isDir(qtBinaryDir), "Invalid Qt directory path");
    assert(std.file.exists(std.path.buildPath(qtBinaryDir, "qmake")));
    writefln("Qt binary dir = " ~ qtBinaryDir);

    return getQtVersion(qtBinaryDir);
}

QtQmake[] detectQmakeInstallations()
{
    QtQmake[] result;

    version (Windows)
    {
        static assert(0, "FIXME: implement");
    }
    else version (OSX)
    {
        import platform.macos;
        result = macosDetectQmakeInstallations();
    }
    else version (linux)
    {
        static assert(0, "FIXME: implement");
    }
    else
    {
        static assert(0, "Unsupported platform");
    }

    return result;
}


// https://doc.qt.io/qt-5/supported-platforms.html
struct QtMakeSpecification
{
    // host: the platform you're building on or with
    // target: the platforms you are building for
    string hostMakespec;    // $$[QMAKE_SPEC]
    string targetMakeSpec;  // $$[QMAKE_XSPEC]

    // Disables default construction: makespecs must be specified
    @disable this();

    this(in string hostMakespec, in string targetMakeSpec)
    {
        this.hostMakespec = hostMakespec;
        this.targetMakeSpec = targetMakeSpec;
    }

    static string detectHostMakeSpec()
    {
        string result;

        version (Windows)
        {
            result = "win32-msvc";
        }
        version (OSX)
        {
            result = "macx-clang";
        }
        else version (linux)
        {
            result = "linux-g++";
        }
        else
        {
            static assert(0, "Unsupported host platform");
        }

        return result;
    }

    // NOTE: there is no sense to detect target makespec, because there are too many variants:
    //       e.g. macOS host can build program for iOS, Android and other targets
    //
    // Android: "android-g++"
    // iOS:     "macx-ios-clang"
    // UWP:     "winrt-x64-msvc2017"
}

struct QtQmake
{
    string fullPath = "";
    string versionString = "";
    bool isValid = false;
}

struct QtVersionInfo
{
    string qtRootDir;
    string qtPlatformDir;
    string qtMkspecsDir;
    string qtBinaryDir;
    string qtLibraryDir;
    string qtIncludeDir;
    string qtLibexecDir;
    string qtTestsDir;
    string qtPluginsDir;
    string qtImportsDir;
    string qtQmlDir;
    string qtTranslationsDir;

    string qtDocsDir;
    string qtExamplesDir;

    string qtVersionStr;
    string qtPlatformStr;
}

class QtVersion
{
    private immutable(QtVersionInfo) m_qt;

    // Disables default construction: Qt version information must be specified
    @disable this();

    this(immutable(QtVersionInfo) qt) immutable
    {
        m_qt = qt;
    }
    
    string mkspecDirPath() const
    out (result)
    {
        assert(std.file.exists(result) && std.file.isDir(result));
    }
    do
    {
        return std.path.buildPath(m_qt.qtPlatformDir, MKSPECS_DIR);
    }

    string featureDirPath() const
    out (result)
    {
        assert(std.file.exists(result) && std.file.isDir(result));
    }
    do
    {
        return std.path.buildPath(m_qt.qtPlatformDir, MKSPECS_DIR, FEATURES_DIR);
    }

    string specPreFilePath() const
    out (result)
    {
        assert(std.file.exists(result) && std.file.isFile(result));
    }
    do
    {
        return std.path.buildPath(featureDirPath(), QMAKE_SPEC_PRE_FILE);
    }

    string specPostFilePath() const
    out (result)
    {
        assert(std.file.exists(result) && std.file.isFile(result));
    }
    do
    {
        return std.path.buildPath(featureDirPath(), QMAKE_SPEC_POST_FILE);
    }

    bool isValid() const
    {
        if (!std.file.exists(featureDirPath()) || !std.file.isDir(featureDirPath()))
        {
            error("feature directory '", featureDirPath(), "' was not found or not a directory");
            return false;
        }

        if (!std.file.exists(mkspecDirPath()) || !std.file.isDir(mkspecDirPath()))
        {
            error("mkspec directory '", mkspecDirPath(), "' was not found or not a directory");
            return false;
        }

        if (!std.file.exists(specPreFilePath()) || !std.file.isDir(specPreFilePath()))
        {
            error("'", specPreFilePath(), "' was not found or not a directory");
            return false;
        }

        if (!std.file.exists(specPostFilePath()) || !std.file.isDir(specPostFilePath()))
        {
            error("'", specPostFilePath(), "' was not found or not a directory");
            return false;
        }

        return true;
    }
}
