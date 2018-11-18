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

import std.experimental.logger;

import std.string;
import std.conv;
import std.algorithm;
import std.stdio;
static import std.file;
static import std.path;
import qmakeexception;
import project;
import project_context;
import project_variable;
import common_utils;
import persistent_property;
import command_line_options;


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
    
    public static const string QMAKE_PRE_FILE = "spec_pre.prf";
    public static const string QMAKE_POST_FILE = "spec_post.prf";

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
        return std.path.buildPath(featureDirPath(), QMAKE_PRE_FILE);
    }

    string specPostFilePath() const
    {
        return std.path.buildPath(featureDirPath(), QMAKE_POST_FILE);
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

private static const string QMAKE_CONF_FILE = "qmake.conf";

struct PlatformData
{
    string directorySeparator;
    string directoryListSeparator;
    string currentDate;

    string applicationPath;
    string[] applicationArguments;
    string qtConfFilePath;

    string hostCpuCount;
    string hostOs;
    string hostArch;
    string hostName;
    string hostVersion;
    string hostVersionString;
    
    string targetArch;

    void initialize(string[] argv, in string qtConfFilePath)
    {
        import std.socket : Socket;
        import core.cpuid : coresPerCPU;

        directorySeparator = std.path.dirSeparator;
        directoryListSeparator = std.path.pathSeparator;
        currentDate = getDateTimeString();

        applicationPath = std.file.thisExePath();
        applicationArguments = argv;
        this.qtConfFilePath = qtConfFilePath;

        hostCpuCount = to!string(coresPerCPU());
        version (Windows)
        {
            hostOs = "Windows";
            hostName = Socket.hostName;

            // FIXME: implement and remove stub
            /+
                DWORD name_length = 1024;
                wchar_t name[1024];
                if (GetComputerName(name, &name_length))
                    vars[ProKey("QMAKE_HOST.name")] << ProString(QString::fromWCharArray(name));

                vars[ProKey("QMAKE_HOST.version")] << ProString(QSysInfo::kernelVersion());
                vars[ProKey("QMAKE_HOST.version_string")] << ProString(QSysInfo::productVersion());
            +/
            hostVersion = "Windows 10";
            hostVersionString = "Windows 10";

            version(X86) hostArch = "x86";
            else version(X86_64) hostArch = "x86_64";
            else version(IA64) hostArch = "IA64";
            else hostArch = "Unknown";

            // FIXME: implement and remove stub
            /+
            # if defined(Q_CC_MSVC) // ### bogus condition, but nobody x-builds for msvc with a different qmake
                // Since VS 2017 we need VCToolsInstallDir instead of VCINSTALLDIR
                QString vcInstallDir = m_option->getEnv(QLatin1String("VCToolsInstallDir"));
                if (vcInstallDir.isEmpty())
                    vcInstallDir = m_option->getEnv(QLatin1String("VCINSTALLDIR"));
                vars[ProKey("QMAKE_TARGET.arch")] = msvcArchitecture(vcInstallDir, m_option->getEnv(QLatin1String("PATH")));
            # endif
            +/
            targetArch = "x86_64";
        }
        else version (linux)
        {
            // FIXME: implement and remove stub
            /+
            struct utsname name;
            if (uname(&name) != -1) {
                vars[ProKey("QMAKE_HOST.os")] << ProString(name.sysname);
                vars[ProKey("QMAKE_HOST.name")] << ProString(QString::fromLocal8Bit(name.nodename));
                vars[ProKey("QMAKE_HOST.version")] << ProString(name.release);
                vars[ProKey("QMAKE_HOST.version_string")] << ProString(name.version);
                vars[ProKey("QMAKE_HOST.arch")] << ProString(name.machine);
            }
            +/
            hostOs = "Linux";
            hostName = Socket.hostName;
            hostVersion = "4.15.0-34-generic";
            hostVersionString = "#37~16.04.1-Ubuntu SMP Tue Aug 28 10:44:06 UTC 2018";
            hostArch = "x86_64";
        }
        else
            // FIXME: implement support of other qmake-supported platforms
            throw new NotImplementedException("Only Windows and Linux platforms supported now!");
    }
}

private static void loadQmakeDefaults(ref ProExecutionContext context, string[] argv, string qtConfFilePath)
{
    PlatformData pd;
    pd.initialize(argv, qtConfFilePath);

    context.assignVariable("DIR_SEPARATOR", [pd.directorySeparator], VariableType.STRING);
    context.assignVariable("DIRLIST_SEPARATOR", [pd.directoryListSeparator], VariableType.STRING);
    context.assignVariable("_DATE_", [getDateTimeString()], VariableType.STRING);
    if (!pd.applicationPath.empty)
        context.assignVariable("QMAKE_QMAKE", [pd.applicationPath], VariableType.STRING);
    if (pd.applicationArguments.length > 0)
        context.assignVariable("QMAKE_ARGS", pd.applicationArguments, VariableType.STRING_LIST);
    if (!pd.qtConfFilePath.empty)
        context.assignVariable("QMAKE_QTCONF", [pd.qtConfFilePath], VariableType.STRING);
    context.assignVariable("QMAKE_HOST.cpu_count", [pd.hostCpuCount], VariableType.STRING);
    context.assignVariable("QMAKE_HOST.os", [pd.hostOs], VariableType.STRING);
    context.assignVariable("QMAKE_HOST.name", [pd.hostName], VariableType.STRING);
    context.assignVariable("QMAKE_HOST.version", [pd.hostVersion], VariableType.STRING);
    context.assignVariable("QMAKE_HOST.version_string", [pd.hostVersionString], VariableType.STRING);
    context.assignVariable("QMAKE_HOST.arch", [pd.hostArch], VariableType.STRING);
    context.assignVariable("QMAKE_TARGET.arch", [pd.targetArch], VariableType.STRING);

    assert(context.getVariableRawValue("DIR_SEPARATOR")[0] == "/");
}

private static bool loadQmakeFeature(ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage,
    in QtVersion qt, in string name)
{
    string featureFileName = std.path.buildPath(qt.featureDirPath(), name);
    if (!std.file.exists(featureFileName) || !std.file.isFile(featureFileName))
    {
        error("feature file '", featureFileName, "' was not found or not a file");
        return false;
    }

    auto featurePro = new Project(context, persistentStorage);
    if (!featurePro.eval(featureFileName))
    {
        error("evaluating feature file '", featureFileName, "' failed");
        return false;
    }

    info("feature file '", featureFileName, "' successfully eval'd");
    return true;
}

private static bool loadQmakeSpec(ref ProExecutionContext context, ref PersistentPropertyStorage persistentStorage,
    in QtVersion qt, in string name)
{
    assert(context.getVariableRawValue("DIR_SEPARATOR")[0] == "/");

    // 1) Eval pre-feature
    if (!loadQmakeFeature(context, persistentStorage, qt, QtVersion.QMAKE_PRE_FILE))
    {
        throw new Exception("Post feature eval failed");
        //return false;
    }

	trace("\n\nqmake built-in variable values:");
    foreach (variableName_1; context.getBuiltinVariableNames())
    {
        string[] variableValue = context.getVariableRawValue(variableName_1);
        if (!variableValue.empty && !variableValue[0].empty)
            trace(variableName_1, " = ", variableValue);
    }

    // 2) Eval mkspec
    string mkspecDirPath = std.path.buildPath(qt.mkspecDirPath(), name);
    if (!std.file.isDir(mkspecDirPath))
    {
        error("mkspec directory '", mkspecDirPath, "' was not found");
        return false;
    }

    string mkspecFilePath = std.path.buildPath(mkspecDirPath, QMAKE_CONF_FILE);
    if (!std.file.isFile(mkspecFilePath))
    {
        error("mkspec file '", mkspecFilePath, "' was not found");
        return false;
    }

    auto pro = new Project(context, persistentStorage);
    if (!pro.eval(mkspecFilePath))
    {
        throw new Exception("spec parse failed");
        //return false;
    }
    info("qmake mkspec file '" ~ mkspecFilePath ~ "' was successfully parsed");

    // 3) Eval post-feature
    if (!loadQmakeFeature(context, persistentStorage, qt, QtVersion.QMAKE_POST_FILE))
    {
        throw new Exception("Post feature eval failed");
        //return false;
    }
    assert(context.getVariableRawValue("DIR_SEPARATOR")[0] == "/");

    return false;
}

private bool execPropertyAction(in QmakePropertyAction action, in string[] properties,
    ref PersistentPropertyStorage storage)
{
    bool ret = true;
    if (action == QmakePropertyAction.query)
    {
        // FIXME: implement
        /*if (properties.empty)
        {
            initSettings();
            const auto keys = settings->childKeys();
            for (const QString &key : keys) {
                QString val = settings->value(key).toString();
                fprintf(stdout, "%s:%s\n", qPrintable(key), qPrintable(val));
            }

            string[] specialProps;
            for (unsigned i = 0; i < propList.length; i++)
                specialProps.append(QString::fromLatin1(propList[i].name));
            specialProps.append("QMAKE_VERSION");
            specialProps.append("QT_VERSION");

            for (const QString &prop : qAsConst(specialProps)) {
                ProString val = value(ProKey(prop));
                ProString pval = value(ProKey(prop + "/raw"));
                ProString gval = value(ProKey(prop + "/get"));
                ProString sval = value(ProKey(prop + "/src"));
                ProString dval = value(ProKey(prop + "/dev"));
                fprintf(stdout, "%s:%s\n", prop.toLatin1().constData(), val.toLatin1().constData());
                if (!pval.isEmpty() && pval != val)
                    fprintf(stdout, "%s/raw:%s\n", prop.toLatin1().constData(), pval.toLatin1().constData());
                if (!gval.isEmpty() && gval != (pval.isEmpty() ? val : pval))
                    fprintf(stdout, "%s/get:%s\n", prop.toLatin1().constData(), gval.toLatin1().constData());
                if (!sval.isEmpty() && sval != gval)
                    fprintf(stdout, "%s/src:%s\n", prop.toLatin1().constData(), sval.toLatin1().constData());
                if (!dval.isEmpty() && dval != pval)
                    fprintf(stdout, "%s/dev:%s\n", prop.toLatin1().constData(), dval.toLatin1().constData());
            }
            return true;
        }*/

        foreach (var; properties)
        {
            if (properties.length > 1)
                stdout.writefln("%s:", var);

            if (!storage.hasValue(var))
            {
                ret = false;
                stdout.writefln("**Unknown**");
            }
            else
            {
                stdout.writefln("%s", storage.value(var));
            }
        }
    }
    else if (action == QmakePropertyAction.set)
    {
        for (int i; i < properties.length; i++)
        {
            string var = properties[i];
            i++;
            if (i == properties.length)
            {
                ret = false;
                break;
            }
            string value = properties[i];
            if (!var.startsWith("."))
                storage.setValue(var, value);
        }
    }
    else if (action == QmakePropertyAction.unset)
    {
        foreach (var; properties)
        {
            if (!var.startsWith("."))
                storage.remove(var);
        }
    }
    return ret;
}

/* This is to work around lame implementation on Darwin. It has been noted that the getpwd(3) function
   is much too slow, and called much too often inside of Qt (every fileFixify). With this we use a locally
   cached copy because I can control all the times it is set (because Qt never sets the pwd under me).
*/
private static string pwd;
private string qmake_getpwd()
{
    if (pwd.empty)
        pwd = std.file.getcwd();
    return pwd;
}
private bool qmake_setpwd(in string p)
{
    /*if (QDir::setCurrent(p)) {
        pwd = QDir::currentPath();
        return true;
    }
    return false;*/
    std.file.chdir(p);
    return true;
}

int main(string[] argv)
{
    version (tracer)
    {
        traceAll();
        //setTraceConditionFunction(function(string ruleName, const ref ParseTree p) {return ruleName.startsWith("QMakeProject.");});
    }

    string oldpwd = qmake_getpwd();

    // Parse command line options
    QmakeOptions options;
    immutable int ret = parseCommandlineOptions(argv, options);
    if (ret != CmdLineFlags.QMAKE_CMDLINE_SUCCESS)
    {
        return ((ret & CmdLineFlags.QMAKE_CMDLINE_ERROR) != 0) ? 1 : 0;
    }

    auto persistentStorage = new PersistentPropertyStorage();
    if (options.propertyAction == QmakePropertyAction.query
     || options.propertyAction == QmakePropertyAction.set
     || options.propertyAction == QmakePropertyAction.unset)
    {
        return execPropertyAction(options.propertyAction, options.properties, persistentStorage) ? 0 : 101;
    }

    /+
    ProFileCache proFileCache;
    Option::proFileCache = &proFileCache;
    QMakeParser parser(&proFileCache, &vfs, &Option::evalHandler);
    Option::parser = &parser;
    +/

    // FIXME: process assignments

    // FIXME: my custom code //////////////////////////////////////////
    immutable(QtVersionInfo) qtInfo = QtVersionInfo("/opt/Qt", "5.11.1", "gcc_64");
    const(QtVersion) qt = new const QtVersion(qtInfo);

    auto context = new ProExecutionContext();
    loadQmakeDefaults(context, argv.remove(0), "" /*FIXME: qtConfFileName*/);
    assert(context.getVariableRawValue("DIR_SEPARATOR")[0] == "/");
    loadQmakeSpec(context, persistentStorage, qt, options.specFileName);
    assert(context.getVariableRawValue("MAKEFILE_GENERATOR")[0] == "UNIX");
    ///////////////////////////////////////////////////////////////////

    int exitVal;
    foreach (projectFileName; options.projectFileNames)
    {
        if (options.mode == QmakeMode.makefile || options.mode == QmakeMode.prl)
        {
            //string fn = Option.normalizePath(projectFileName);
            string fn = projectFileName;

            if (!std.file.exists(fn))
            {
                stderr.writefln("Cannot find file: %s.", fn /*QDir::toNativeSeparators(fn).toLatin1().constData()*/);
                exitVal = 2;
                continue;
            }

            // Setup pwd properly
            //debug_msg(1, "Resetting dir to: %s", QDir::toNativeSeparators(oldpwd).toLatin1().constData());
            qmake_setpwd(oldpwd); //reset the old pwd
            long di = fn.lastIndexOf('/');
            if (di != -1)
            {
                //debug_msg(1, "Changing dir to: %s", QDir::toNativeSeparators(fn.left(di)).toLatin1().constData());
                if (!qmake_setpwd(fn.left(di)))
                    stderr.writefln("Cannot find directory: %s", fn.left(di) /*QDir::toNativeSeparators(fn.left(di)).toLatin1().constData()*/);
                fn = fn.right(fn.length - di - 1);
            }

            //Option::prepareProject(fn);

            auto project = new Project(context, persistentStorage);
            if (!project.eval(fn))
            {
                stderr.writefln("Error processing project file: %s", projectFileName /*QDir::toNativeSeparators(*projectFileName).toLatin1().constData()*/);
                exitVal = 3;
                continue;
            }
            if (options.doPreprocess)
            {
                project.dump();

                // No need to create makefile
                continue;
            }
        }

        /+
        bool success = true;
        MetaMakefileGenerator *mkfile = MetaMakefileGenerator::createMetaGenerator(&project, QString(), false, &success);
        if (!success)
            exit_val = 3;

        if (mkfile && !mkfile->write())
        {
            if(Option::qmake_mode == Option::QMAKE_GENERATE_PROJECT)
                fprintf(stderr, "Unable to generate project file.\n");
            else
                fprintf(stderr, "Unable to generate makefile for: %s\n",
                        QDir::toNativeSeparators(*projectFileName).toLatin1().constData());
            exit_val = 5;
        }
        delete mkfile;
        mkfile = NULL;
        +/
    }

    //qmakeClearCaches();
    return exitVal;
}
