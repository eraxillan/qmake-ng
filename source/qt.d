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
module source.qt;

import std.experimental.logger;

static import std.file;
static import std.path;

import std.stdio;
import std.range;
import std.conv;
import std.string;

import source.common_const;
import source.common_utils;
import source.project_context;
import source.project_variable;

public:

void setupQtEnvironmentVariables(const QtVersionInfo qtInfo)
{
    import std.process: environment;

    // Set Qt-specific environment variables like Qt Creator do
    environment["QTDIR"] = qtInfo.qtPlatformDir;
    environment["PATH"] = qtInfo.qtBinaryDir ~ std.path.pathSeparator ~ environment["PATH"];
    environment["DYLD_FRAMEWORK_PATH"] = qtInfo.qtLibraryDir;
    environment["DYLD_LIBRARY_PATH"] = qtInfo.qtLibraryDir;
}

void setupQtProjectVariables(ref ProExecutionContext context, const QtVersionInfo qtInfo, const string mkSpec)
{
    const(string) qtMkspecsDir = std.path.buildPath(qtInfo.qtPlatformDir, MKSPECS_DIR, mkSpec);
    const(string) qtFeaturesDir = std.path.buildPath(qtInfo.qtPlatformDir, MKSPECS_DIR, FEATURES_DIR);
    assert(isValidDirectoryPath(qtMkspecsDir));
    assert(isValidDirectoryPath(qtFeaturesDir));

    context.assignVariable("QMAKESPEC", [qtMkspecsDir], VariableType.STRING_LIST);
    // QMAKE_NG_EXTENSION
    context.assignVariable("QMAKESPEC_FEATURES", [qtFeaturesDir], VariableType.STRING_LIST);
}

QtVersionInfo getQtVersion(const string qtBinaryDir)
out (result)
{
    assert(isValidDirectoryPath(result.qtRootDir));
    assert(isValidDirectoryPath(result.qtPlatformDir));
    assert(isValidDirectoryPath(result.qtMkspecsDir));
    assert(isValidDirectoryPath(result.qtBinaryDir));
    assert(isValidDirectoryPath(result.qtLibraryDir));
    assert(isValidDirectoryPath(result.qtIncludeDir));
    // NOTE: libexec, tests, imports dirs can be absent
    // assert(isValidDirectoryPath(result.qtLibexecDir));
    // assert(isValidDirectoryPath(result.qtTestsDir));
    assert(isValidDirectoryPath(result.qtPluginsDir));
    // assert(isValidDirectoryPath(result.qtImportsDir));
    assert(isValidDirectoryPath(result.qtQmlDir));
    assert(isValidDirectoryPath(result.qtTranslationsDir));

    assert(isValidDirectoryPath(result.qtDocsDir));
    assert(isValidDirectoryPath(result.qtExamplesDir));
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

    assert(isValidDirectoryPath(qtLibraryDir), "Invalid Qt library directory path");
    assert(isValidDirectoryPath(qtPlatformDir), "Invalid Qt platform directory path");
    assert(isValidDirectoryPath(qtRootDir), "Invalid Qt root directory path");
    assert(splitString(qtVersionStr, ".", true).length == 3, "Invalid Qt version");
    assert(!qtPlatformStr.empty, "Invalid Qt platform");

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

QtVersionInfo chooseQtSourceVersion()
{
    string qtSourceDir;

    auto qtSourceDirs = detectQtSourceRepositories();
    if (qtSourceDirs.length == 1)
    {
        writefln("Auto-detected Qt source directory path: %s", std.path.dirName(qtSourceDirs[0]));
        writefln("Enter 'yes' to accept detected dir or your custom path instead:");
        qtSourceDir = stdin.readln().strip();
        if (qtSourceDir == "yes")
            qtSourceDir = std.path.dirName(qtSourceDirs[0]);
        else
            qtSourceDir = stdin.readln().strip();
    }
    else if (qtSourceDirs.length > 1)
    {
        writefln("More than one Qt source directory detected:");
        for (int i = 1; i <= qtSourceDirs.length; i++)
        {
            writefln("[%d] %s", i, std.path.dirName(qtSourceDirs[i - 1]));
        }
        writefln("Enter number of Qt source directory or your custom path instead:");
        qtSourceDir = stdin.readln().strip();
        if (isNumeric(qtSourceDir, 10))
        {
            int index = std.conv.to!int(qtSourceDir, 10);
            assert(index >= 1 && index <= qtSourceDirs.length);
            qtSourceDir = std.path.dirName(qtSourceDirs[index - 1]);
        }
    }
    else
    {
        writefln("Enter Qt source root directory path (either git repository or Src directory from binary package):");
        qtSourceDir = stdin.readln().strip();
    }

    assert(qtSourceDir !is null);
    assert(isValidDirectoryPath(qtSourceDir), "Invalid Qt binary directory path");
    writefln("Qt source dir = " ~ qtSourceDir);

    QtVersionInfo result;

    result.qtRootDir = qtSourceDir;

    // NOTE: current unit test only parse projects, not eval,
    //       so other fields can be left empty

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
        writefln("Enter Qt binary directory path (containing original qmake):");
        qtBinaryDir = stdin.readln().strip();
    }

    assert(qtBinaryDir !is null);
    assert(isValidDirectoryPath(qtBinaryDir), "Invalid Qt binary directory path");
    assert(isValidFilePath(std.path.buildPath(qtBinaryDir, "qmake")));

    return getQtVersion(qtBinaryDir);
}

string[] detectQtSourceRepositories()
{
    string[] result;

    version (Windows)
    {
        static assert(0, "FIXME: implement");
    }
    else version (OSX)
    {
        import source.platform.macos;
        result = macosDetectQtSourceRepositories();
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

QtQmake[] detectQmakeInstallations()
{
    QtQmake[] result;

    version (Windows)
    {
        static assert(0, "FIXME: implement");
    }
    else version (OSX)
    {
        import source.platform.macos;
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

    this(const string hostMakespec, const string targetMakeSpec)
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
        assert(isValidDirectoryPath(result));
    }
    do
    {
        return std.path.buildPath(m_qt.qtPlatformDir, MKSPECS_DIR);
    }

    string featureDirPath() const
    out (result)
    {
        assert(isValidDirectoryPath(result));
    }
    do
    {
        return std.path.buildPath(m_qt.qtPlatformDir, MKSPECS_DIR, FEATURES_DIR);
    }

    string specPreFilePath() const
    out (result)
    {
        assert(isValidFilePath(result));
    }
    do
    {
        return std.path.buildPath(featureDirPath(), QMAKE_SPEC_PRE_FILE);
    }

    string specPostFilePath() const
    out (result)
    {
        assert(isValidFilePath(result));
    }
    do
    {
        return std.path.buildPath(featureDirPath(), QMAKE_SPEC_POST_FILE);
    }

    bool isValid() const
    {
        if (!isValidDirectoryPath(featureDirPath()))
        {
            error("feature directory '", featureDirPath(), "' was not found or not a directory");
            return false;
        }

        if (!isValidDirectoryPath(mkspecDirPath()))
        {
            error("mkspec directory '", mkspecDirPath(), "' was not found or not a directory");
            return false;
        }

        if (!isValidDirectoryPath(specPreFilePath()))
        {
            error("'", specPreFilePath(), "' was not found or not a directory");
            return false;
        }

        if (!isValidDirectoryPath(specPostFilePath()))
        {
            error("'", specPostFilePath(), "' was not found or not a directory");
            return false;
        }

        return true;
    }
}
