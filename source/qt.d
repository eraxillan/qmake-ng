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
import common_utils;

QtQmake[] detectQmakeInstallations()
{
    import std.process;

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
    //       e.g. macOS host can build for iOS, Android and other targets
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
    string qtVersionStr;
    string qtKit;

    this(string qtRootDir, string qtVersionStr, string qtKit)
    {
        this.qtRootDir = qtRootDir;
        this.qtVersionStr = qtVersionStr;
        this.qtKit = qtKit;
    }
}

class QtVersion
{
    private static const string MKSPECS_DIR = "mkspecs";
    private static const string FEATURES_DIR = "features";
    
    public static const string QMAKE_SPEC_PRE_FILE = "spec_pre.prf";
    public static const string QMAKE_SPEC_POST_FILE = "spec_post.prf";

    public static const string QMAKE_PRE_FILE = "default_pre.prf";
    public static const string QMAKE_POST_FILE = "default_post.prf";

    private QtVersionInfo m_qt;

    this(QtVersionInfo qt)
    {
        m_qt = qt;
    }
    
    string mkspecDirPath() const
    {
        return std.path.buildPath(m_qt.qtRootDir, m_qt.qtVersionStr, m_qt.qtKit, MKSPECS_DIR);
    }

    string featureDirPath() const
    {
        return std.path.buildPath(m_qt.qtRootDir, m_qt.qtVersionStr, m_qt.qtKit, MKSPECS_DIR, FEATURES_DIR);
    }

    string specPreFilePath() const
    {
        return std.path.buildPath(featureDirPath(), QMAKE_SPEC_PRE_FILE);
    }

    string specPostFilePath() const
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
