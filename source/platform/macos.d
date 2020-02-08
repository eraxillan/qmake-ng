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

module platform.macos;

import std.experimental.logger;

static import std.file;
static import std.path;

import std.string;
import std.algorithm;
import std.stdio;
import std.process;
import qmakeexception;
import common_utils;
import qt;


string[] macosDetectQtSourceRepositories()
{
    string[] result;

    // Raw command: mdfind 'kMDItemFSName == "LICENSE.QT-LICENSE-AGREEMENT-4.0"' | grep -v /qtbase/
    auto mdfindOutput = executeShell(
        "mdfind 'kMDItemFSName == \"LICENSE.QT-LICENSE-AGREEMENT-4.0\"' | grep -v /qtbase/"
    );
    if (mdfindOutput.status != 0)
    {
        warning("No Qt source code repository instance found");
        return result;
    }

    // Output example for macOS:
    // /Users/<user>/Projects/qt5/LICENSE.QT-LICENSE-AGREEMENT-4.0
    result = splitString(mdfindOutput.output, "\n", true);

    return result;
}

QtQmake[] macosDetectQmakeInstallations()
{
    QtQmake[] result;

    // Raw command: mdfind 'kMDItemFSName == "qmake" && kMDItemKind == "Terminal Document"' | grep ".*/5\..*/bin/qmake"
    auto mdfindOutput = executeShell(
        "mdfind 'kMDItemFSName == \"qmake\" && kMDItemKind == \"Terminal Document\"' | grep \".*/5\\..*/bin/qmake\""
    );
    if (mdfindOutput.status != 0)
    {
    	warning("No qmake instances found");
    	return result;
    }

    // Output example for macOS:
    // /Users/<user>/Qt/5.12.3/clang_64/bin/qmake
    // /Users/<user>/Qt/5.12.3/ios/bin/qmake
    string[] mdfindOutputLines = splitString(mdfindOutput.output, "\n", true);
    foreach (qmakePath; mdfindOutputLines)
    {
        QtQmake qmakeItem;

        // Our `mdfind` call must return only executables, so there is no need to check for it here
        auto qmakeOutput = executeShell(qmakePath ~ " " ~ "-version");
        
        // Example output:
        // QMake version 3.1
        // Using Qt version 5.12.3 in /Users/<user>/Qt/5.12.3/clang_64/lib
        if (qmakeOutput.status != 0)
        {
            writefln("Failed to execute 'qmake -version' command");
            continue;
        }

        string[] qmakeOutputLines = splitString(qmakeOutput.output, "\n", true);
        assert(qmakeOutputLines.length == 2);
        string[] qmakeVersionTokens = splitString(qmakeOutputLines[0], " ", true);
        assert(qmakeVersionTokens.length == 3);

        // NOTE: currently just ignore second line with Qt version and path

        qmakeItem.fullPath = qmakePath;
        qmakeItem.versionString = qmakeVersionTokens[2];
        qmakeItem.isValid = true;

        result ~= qmakeItem;
    }

    return result;
}

// --------------------------------------------------------------------------------------------------------------------

class XcodeInfo
{
    private const string XCODE_APP_STR = "Xcode.app";
    private enum SdkType { macos, ios, watchos, tvos }

    private string m_path;
    private string m_version;

    private string m_clangPath;
    private string m_clangVersion;

    private string m_lldbPath;
    private string m_lldbVersion;

    this()
    {
        detect();
    }

    private void detect()
    {
        import std.process;

        // Xcode path:
        // $ xcode-select -print-path
        // /Applications/Xcode.app/Contents/Developer
        auto xcodeselectOutput = executeShell("xcode-select -print-path");
        assert(xcodeselectOutput.status == 0);
        string[] xcodeselectOutputLines = splitString(xcodeselectOutput.output, "\n", true);
        assert(xcodeselectOutputLines.length == 1);
        m_path = xcodeselectOutputLines[0];
        long xcodeAppIndex = m_path.indexOf(XCODE_APP_STR);
        assert(xcodeAppIndex > 0);
        m_path = m_path[0 .. (xcodeAppIndex + XCODE_APP_STR.length)];
        writefln("Xcode path: '%s'", m_path);

        // Xcode version:
        // $ xcodebuild -version
        // Xcode 10.1
        // Build version 10B61
        auto xcodebuildOutput = executeShell("xcodebuild -version");
        assert(xcodebuildOutput.status == 0);
        string[] xcodebuildOutputLines = splitString(xcodebuildOutput.output, "\n", true);
        assert(xcodebuildOutputLines.length == 2);
        assert(xcodebuildOutputLines[0].startsWith("Xcode "));
        string[] versionString = splitString(xcodebuildOutputLines[0], " ", true);
        assert(versionString.length == 2);
        m_version = versionString[1];
        writefln("Xcode version: '%s'", m_version);

        // Find specific tool (e.g. clang):
        // $ xcrun -sdk macosx -find clang
        // /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang
        // $ xcrun -sdk macosx -find lldb
        // /Applications/Xcode.app/Contents/Developer/usr/bin/lldb
        auto xcrunClangOutput = executeShell("xcrun -sdk macosx -find clang");
        assert(xcrunClangOutput.status == 0);
        m_clangPath = strip(xcrunClangOutput.output);
        assert(std.file.exists(m_clangPath));
        // FIXME: also check whether returned file is executable (no standard function for this in Phobos though)
        auto clangVersionOutput = executeShell(m_clangPath ~ " " ~ "--version");
        assert(clangVersionOutput.status == 0);
        m_clangVersion = strip(clangVersionOutput.output);
        writefln("clang path: '%s'", m_clangPath);
        writefln("clang version: '%s'", m_clangVersion);
        //

        auto xcrunLldbOutput = executeShell("xcrun -sdk macosx -find lldb");
        assert(xcrunLldbOutput.status == 0);
        writefln("lldb path: '%s'", strip(xcrunLldbOutput.output));

        // $ /Applications/Xcode.app/Contents/Developer/usr/bin/lldb --version
        // lldb-1000.11.38.2
        // Swift-4.2
        // [eraxillan@macmini ~]$ /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang --version
        // Apple LLVM version 10.0.0 (clang-1000.11.45.5)
        // Target: x86_64-apple-darwin17.7.0
        // Thread model: posix
        // InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin

        // Show platform SDKs list
        // $ xcodebuild -showsdks -json
        // ...

        // Show the specified platform SDK path used when building:
        // $ xcrun -sdk macosx --show-sdk-path
        // /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk


    }
}
