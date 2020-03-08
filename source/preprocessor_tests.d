module source.preprocessor_tests;

import std.stdio;
import source.common_utils;
import source.preprocessor;

//---------------------------------------------------------------------------------------------------------------------

unittest
{
    // NOTE: These examples were extracted from clang_64/mkspecs directory

    // Function name         | Function type | Ambiguous argument indeces
    // EXISTS_FUNCTION_STR   | test          | argument 1
    // CONTAINS_FUNCTION_STR | test          | argument 2
    // QTCONFIG_FUNCTION_STR | test          | argument 1
    // ERROR_FUNCTION_STR    | test          | argument 1
    // SYSTEM_FUNCTION_STR   | test/replace  | argument 1
    // FIND_FUNCTION_STR     | replace       | argument 2
    // REESCAPE_FUNCTION_STR | replace       | argument 1
    // REPLACE_FUNCTION_STR  | replace       | arguments 2 and 3

    writeln("Starting preprocessor unit tests...");
 
    ExtractResult result;
    string[] strLinesArray;

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 1) exists

    // Single-line test snippets
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$NDK_ROOT): error(\"You need to set the ANDROID_NDK_ROOT environment variable to point to your Android NDK.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$NDK_ROOT");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$absolute_path($$bundle_file/AppIcon.appiconset, $$_PRO_FILE_PWD_)): next()", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$absolute_path($$bundle_file/AppIcon.appiconset, $$_PRO_FILE_PWD_)");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$test_out_dir/Makefile):qtRunLoggedCommand(\"$$test_cmd_base $$QMAKE_MAKE distclean\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$test_out_dir/Makefile");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$cmake_extras_file.input)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$cmake_extras_file.input");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$DEVICE_PRI):include($$DEVICE_PRI)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$DEVICE_PRI");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$QMLTYPEFILE): AUX_QML_FILES += $$QMLTYPEFILE", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMLTYPEFILE");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$qmod/qml): importpath.value += $$shell_path($$qmod/qml)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$qmod/qml");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$QMAKE_DOCS): error(\"Cannot find documentation specification file $$QMAKE_DOCS\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_DOCS");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$cmd): QT_TOOL.repc.binary = $$cmd", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$cmd");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$UUID_CACHE): include($$UUID_CACHE)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$UUID_CACHE");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$QMAKE_QT_MODULE)|!include($$QMAKE_QT_MODULE, \"\", true):debug(1, \"Cannot load qmodule.pri!\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_QT_MODULE");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$OUT_PWD/qt$${MODULE}-config.pri): include($$OUT_PWD/qt$${MODULE}-config.pri)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$OUT_PWD/qt$${MODULE}-config.pri");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$pfx/.qmake.cache): logn(\"Once everything is built, Qt is installed.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$pfx/.qmake.cache");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$QMAKE_QT_CONFIG)|!include($$QMAKE_QT_CONFIG, \"\", true): debug(1, \"Cannot load qconfig.pri!\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_QT_CONFIG");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$OUT_PWD/config.opt): qtConfAddError(\"No config.opt present - cannot redo configuration.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$OUT_PWD/config.opt");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$lp/.): qtLog(\"Library path $$val_escape(lp) is invalid.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$lp/.");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$libdir/.): qtLog(\"Library path $$val_escape(libdir) is invalid.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$libdir/.");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$incdir/.): qtLog(\"Include path $$val_escape(incdir) is invalid.\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$incdir/.");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($${cmd}.pl): $${1}_EXE = $${cmd}.pl", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$${cmd}.pl");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$BUNDLENAME): cmd = $$BUNDLENAME", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$BUNDLENAME");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists(\"$$sysroot/usr/lib/pkgconfig\"): return()", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$sysroot/usr/lib/pkgconfig\"");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$OUT_PWD/$${MODULE_CFG_FILE}.pri): include($$OUT_PWD/$${MODULE_CFG_FILE}.pri)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$OUT_PWD/$${MODULE_CFG_FILE}.pri");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$OUT_PWD/$${MODULE_CFG_FILE}.h): fwd_rel = $$relative_path($$OUT_PWD, $$REAL_MODULE_BASE_OUTDIR)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$OUT_PWD/$${MODULE_CFG_FILE}.h");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$_PRO_FILE_PWD_/src/src.pro): sub_src.subdir = src", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$_PRO_FILE_PWD_/src/src.pro");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$_PRO_FILE_PWD_/examples/examples.pro): sub_examples.subdir = examples", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$_PRO_FILE_PWD_/examples/examples.pro");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($${include}/parser.g):msvc:QMAKE_CXXFLAGS += /wd4129", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$${include}/parser.g");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$ANDROID_JAR_FILE): ANDROID_API_VERSION = $$section(API_VERSION_TO_USE, -, 1, 1)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$ANDROID_JAR_FILE");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$dir/sh.exe): QMAKE_SH = $$dir/sh.exe", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$dir/sh.exe");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         1, "!exists($$VXWORKS_MUNCH_TOOL): error(\"Could not find VxWorks Munch tool: '$${VXWORKS_MUNCH_TOOL}'\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$VXWORKS_MUNCH_TOOL");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
         0, "exists($$[QT_INSTALL_PLUGINS]/platforms/wasm_shell.html): WASM_PLUGIN_PATH = $$[QT_INSTALL_PLUGINS]/platforms", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$[QT_INSTALL_PLUGINS]/platforms/wasm_shell.html");
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        18, "watchos:simulator:!exists($$simulator_system_frameworks/CoreText.framework/Headers/CoreText.h): device_system_frameworks = $$xcodeSDKInfo(Path, $${device.sdk})/System/Library/Frameworks", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$simulator_system_frameworks/CoreText.framework/Headers/CoreText.h");

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= "exists($$ANDROID_SOURCES_CXX_STL_LIBDIR/libc++.so): \\";
    strLinesArray ~= "    ANDROID_CXX_STL_LIBS = -lc++";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$ANDROID_SOURCES_CXX_STL_LIBDIR/libc++.so");

    strLinesArray = [];
    strLinesArray ~= "!exists($$CMAKE_MODULE_TESTS): \\";
    strLinesArray ~= "    error(\"Missing CMake tests. Either create tests in tests/auto/cmake,\" \\";
    strLinesArray ~= "          \"or disable cmake config file creation with CONFIG-=create_cmake.\")";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$CMAKE_MODULE_TESTS");

    strLinesArray = [];
    strLinesArray ~= "!exists($$QMAKE_XCODE_DEVELOPER_PATH): \\";
    strLinesArray ~= "    error(\"Xcode is not installed in $${QMAKE_XCODE_DEVELOPER_PATH}. Please use xcode-select to choose Xcode installation path.\")";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_XCODE_DEVELOPER_PATH");

    strLinesArray = [];
    strLinesArray ~= "exists($$QMAKE_XCODE_PREFERENCES_FILE): \\";
    strLinesArray ~= "   QMAKE_TARGET_BUNDLE_PREFIX = $$system(\"/usr/libexec/PlistBuddy -c 'print IDETemplateOptions:bundleIdentifierPrefix' $$QMAKE_XCODE_PREFERENCES_FILE 2>/dev/null\")";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_XCODE_PREFERENCES_FILE");

    strLinesArray = [];
    strLinesArray ~= "exists($$NDK_ROOT/sysroot/usr/include): \\";
    strLinesArray ~= "        QMAKE_CFLAGS += --sysroot=$$NDK_ROOT/sysroot \\";
    strLinesArray ~= "                        -isystem $$NDK_ROOT/sysroot/usr/include/$$NDK_TOOLS_PREFIX";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$NDK_ROOT/sysroot/usr/include");

    strLinesArray = [];
    strLinesArray ~= "isEmpty(QMAKE_PLUGINDUMP_DEPENDENCIES_FILE):exists($$_PRO_FILE_PWD_/dependencies.json): \\";
    strLinesArray ~= "    QMAKE_PLUGINDUMP_DEPENDENCIES_FILE = $$_PRO_FILE_PWD_/dependencies.json";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
       44, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$_PRO_FILE_PWD_/dependencies.json");
    
    strLinesArray = [];
    strLinesArray ~= "exists($$[QT_INSTALL_QML/get]): \\";
    strLinesArray ~= "    QMLPATHS *= $$[QT_INSTALL_QML/get]";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$[QT_INSTALL_QML/get]");

    strLinesArray = [];
    strLinesArray ~= "!exists($$[QT_HOST_DATA]/.qmake.cache): \\";
    strLinesArray ~= "    CONFIG += prefix_build force_independent";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$[QT_HOST_DATA]/.qmake.cache");
    
    strLinesArray = [];
    strLinesArray ~= "exists($$qrep/qml): \\";
    strLinesArray ~= "   QMLPATHS += $$qrep/qml";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$qrep/qml");

    strLinesArray = [];
    strLinesArray ~= "exists($$MODULE_BASE_INDIR/.git): \\";
    strLinesArray ~= "    CONFIG += git_build";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$MODULE_BASE_INDIR/.git");

    strLinesArray = [];
    strLinesArray ~= "exists(\"$$dir/$$file\"): \\";
    strLinesArray ~= "    return(\"$$dir/$$file\")";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$dir/$$file\"");

    strLinesArray = [];
    strLinesArray ~= "exists($$test_out_dir/Makefile): \\";
    strLinesArray ~= "    QMAKE_MAKE = \"$$QMAKE_MAKE clean && $$QMAKE_MAKE\"";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$test_out_dir/Makefile");

    strLinesArray = [];
    strLinesArray ~= "exists($$s/configure.json): \\";
    strLinesArray ~= "    configsToProcess += $$c";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$s/configure.json");
    
    strLinesArray = [];
    strLinesArray ~= "exists($$priFile): \\";
    strLinesArray ~= "    !include($$priFile): error()";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$priFile");

    strLinesArray = [];
    strLinesArray ~= "exists($$extra): \\";
    strLinesArray ~= "    sourcefiles += $$extra";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$extra");

    strLinesArray = [];
    strLinesArray ~= "exists($$BLACKLISTPATH): \\";
    strLinesArray ~= "    testdata.files *= $$BLACKLISTPATH";
    result = extractFunctionArguments(FunctionBaseInfo(EXISTS_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$BLACKLISTPATH");

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 2) contains

    // Single-line test snippets
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         1, `!contains(TARGET, ".so"): TARGET = lib$${TARGET}.so`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TARGET`);
    assert(result.arguments[1] == `".so"`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         1, `!contains(bundle_file, .*\\.xcassets$): next()`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `bundle_file`);
    assert(result.arguments[1] == `.*\\.xcassets$`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
        27, `if(!equals(v, -framework):!contains(v, -L.*)): v ~= s,^-l,,`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `v`);
    assert(result.arguments[1] == `-L.*`);
    
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(TEMPLATE, ".*app"): QMAKE_LFLAGS += $$QMAKE_LFLAGS_EXE`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `".*app"`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(CMAKE_INSTALL_LIBS_DIR, ^(/usr)?/lib(64)?.*): CMAKE_USR_MOVE_WORKAROUND = $$CMAKE_INSTALL_LIBS_DIR`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `CMAKE_INSTALL_LIBS_DIR`);
    assert(result.arguments[1] == `^(/usr)?/lib(64)?.*`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(CMAKE_INCLUDE_DIR, "^\\.\\./.*"): CMAKE_INCLUDE_DIR = $$[QT_INSTALL_HEADERS]/`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `CMAKE_INCLUDE_DIR`);
    assert(result.arguments[1] == `"^\\.\\./.*"`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(CONFIG, plugin): debug("test")`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `CONFIG`);
    assert(result.arguments[1] == `plugin`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(QT.$${mod}.plugin_types, $$PLUGIN_TYPE): debug("test")`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT.$${mod}.plugin_types`);
    assert(result.arguments[1] == `$$PLUGIN_TYPE`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         1, `!contains(QT.$${dep}.module_config, no_link): mod_deps += $$cdep`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT.$${dep}.module_config`);
    assert(result.arguments[1] == `no_link`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         1, `!contains(subent, .*\\w\\.xml$): next()`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `subent`);
    assert(result.arguments[1] == `.*\\w\\.xml$`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(TEMPLATE, .*app): QMAKE_EXTRA_INCLUDES += $$shell_quote($$PWD/sdk.mk)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `.*app`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(VALID_SIMULATOR_ARCHS, $$arch): sdk = $$simulator.sdk`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `VALID_SIMULATOR_ARCHS`);
    assert(result.arguments[1] == `$$arch`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(TEMPLATE, ".*(lib|app)"): CONFIG += have_target`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `".*(lib|app)"`);
    
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         1, `!contains(TEMPLATE, vc.*): hdr = $$basename(tlb)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `vc.*`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains($$dir, .*$${exclusive_affix}.*): $$dir ~= s/$${exclusive_affix}/$${build_affix}/gi`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `$$dir`);
    assert(result.arguments[1] == `.*$${exclusive_affix}.*`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(TEMPLATE, "vc.*"): copycommand = $$QMAKE_QMAKE -install qinstall`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE`);
    assert(result.arguments[1] == `"vc.*"`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(QMAKE_LEX, .*flex): lex.commands = $$QMAKE_LEX`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QMAKE_LEX`);
    assert(result.arguments[1] == `.*flex`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         1, `!contains(DISTRO_OPTS, aarch64): COMPILER_FLAGS += -mfloat-abi=softfp`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `DISTRO_OPTS`);
    assert(result.arguments[1] == `aarch64`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(DISTRO_OPTS, boot2qt): QMAKE_PLATFORM += boot2qt`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `DISTRO_OPTS`);
    assert(result.arguments[1] == `boot2qt`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(QMAKE_TARGET.arch, x86_64): DEFINES += WIN64`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QMAKE_TARGET.arch`);
    assert(result.arguments[1] == `x86_64`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         1, `!contains(WINRT_MANIFEST.CONFIG, "verbatim"):equals(TEMPLATE, "app"): VCLIBS = $${VCLIBS}`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `WINRT_MANIFEST.CONFIG`);
    assert(result.arguments[1] == `"verbatim"`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(MSVC_VER, "15.0"): VCLIBS = $$replace(VCLIBS, 150, 140)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `MSVC_VER`);
    assert(result.arguments[1] == `"15.0"`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(WINRT_MANIFEST.capabilities, defaults): WINRT_MANIFEST.capabilities -= defaults`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `WINRT_MANIFEST.capabilities`);
    assert(result.arguments[1] == `defaults`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(DISTRO, squeeze): QMAKE_LIBS_OPENGL_ES2 = -lGLESv2 -lEGL`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `DISTRO`);
    assert(result.arguments[1] == `squeeze`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(B_REFSW_DEBUG, [Nn]): BRCM_BUILD_TYPE = release`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `B_REFSW_DEBUG`);
    assert(result.arguments[1] == `[Nn]`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         1, `!contains(use, linkonly): CC_USES += $$nu`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `use`);
    assert(result.arguments[1] == `linkonly`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(qmlf,.*\\.js$)|contains(qmlf,.*\\.qml$): CACHEGEN_FILES += $$absolute_path($$qmlf, $$_PRO_FILE_PWD_)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `qmlf`);
    assert(result.arguments[1] == `.*\\.js$`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(qt_depends, gui(-private)?): LIBS *= -L$$[QT_INSTALL_PLUGINS/get]/platforms`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `qt_depends`);
    assert(result.arguments[1] == `gui(-private)?`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(name, ^\\.\\..*): name = $$relative_path($$1, $$OUT_PWD)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `^\\.\\..*`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(rccContents,.*\\.js$)|contains(rccContents,.*\\.qml$)|contains(rccContents,.*\\.mjs$): new_resource = $$qmlCacheResourceFileOutputName($$res)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `rccContents`);
    assert(result.arguments[1] == `.*\\.js$`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(QT, core(-private)?|xml): QT -= core core-private xml`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT`);
    assert(result.arguments[1] == `core(-private)?|xml`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         1, `!contains(QMAKE_INTERNAL_INCLUDED_FILES, .*qmodule\\.pri): QMAKE_QT_MODULE = $$[QT_HOST_DATA/get]/mkspecs/qmodule.pri`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QMAKE_INTERNAL_INCLUDED_FILES`);
    assert(result.arguments[1] == `.*qmodule\\.pri`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(apple_ver, "4\\.[012]|5\\.[01]")|contains(reg_ver, "[345]\\.|6\\.0"): QMAKE_CXXFLAGS_WARN_ON += -Werror`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `apple_ver`);
    assert(result.arguments[1] == `"4\\.[012]|5\\.[01]"`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         6, `linux:contains(ver, "(1[345678]\\.|19\\.0)"): QMAKE_CXXFLAGS_WARN_ON += -Werror`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `ver`);
    assert(result.arguments[1] == `"(1[345678]\\.|19\\.0)"`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(ver, "(4\\.[6789]|[5-9]\\..)"): QMAKE_CXXFLAGS_WARN_ON += -Werror`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `ver`);
    assert(result.arguments[1] == `"(4\\.[6789]|[5-9]\\..)"`);

    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
         0, `contains(MSVC_VER, "1[124].0"): QMAKE_CXXFLAGS_WARN_ON += -WX`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `MSVC_VER`);
    assert(result.arguments[1] == `"1[124].0"`);

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= `!contains(QT.$${dep}.module_config, no_link): \`;
    strLinesArray ~= `    mod_deps += $$cmakeModuleName($$dep)`;
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT.$${dep}.module_config`);
    assert(result.arguments[1] == `no_link`);

    strLinesArray = [];
    strLinesArray ~= `!contains(TARGET, .*phony_target.*): \   # monster hack, you don't really see this here, right? ;)`;
    strLinesArray ~= `    system($$QT_BREAKPAD_ROOT_PATH/qtbreakpadsymbols`;
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
        1, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TARGET`);
    assert(result.arguments[1] == `.*phony_target.*`);

    strLinesArray = [];
    strLinesArray ~= `$$sim_and_dev|contains(QMAKE_MAC_SDK, ^$${device.sdk}.*): \`;
    strLinesArray ~= `    CONFIG += device $${device.sdk}`;
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
       14, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QMAKE_MAC_SDK`);
    assert(result.arguments[1] == `^$${device.sdk}.*`);

    strLinesArray = [];
    strLinesArray ~= `contains(DISTRO_OPTS, deb-multi-arch): \`;
    strLinesArray ~= `   QMAKE_PKG_CONFIG = $${CROSS_COMPILE}pkg-config`;
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `DISTRO_OPTS`);
    assert(result.arguments[1] == `deb-multi-arch`);

    strLinesArray = [];
    strLinesArray ~= `contains(UAP_CAPABILITIES, $$CAPABILITY): \`;
    strLinesArray ~= `   MANIFEST_CAPABILITIES += \"  <uap:Capability Name=\"$$CAPABILITY\" />\"`;
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `UAP_CAPABILITIES`);
    assert(result.arguments[1] == `$$CAPABILITY`);

    strLinesArray = [];
    strLinesArray ~= `contains(flag, ^-.*): \`;
    strLinesArray ~= `   $$1 -= $$replace(flag, ^-, )`;
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `flag`);
    assert(result.arguments[1] == `^-.*`);

    strLinesArray = [];
    strLinesArray ~= `contains(QT.$${module}.enabled_features, $$1): \`;
    strLinesArray ~= `   return(true)`;
    result = extractFunctionArguments(FunctionBaseInfo(CONTAINS_FUNCTION_STR, 2, 0),
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `QT.$${module}.enabled_features`);
    assert(result.arguments[1] == `$$1`);

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 3) qtConfig

    // Single-line test snippets
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        19, `qtHaveModule(gui):!qtConfig(egl): CMAKE_GL_DEFINES += -DNO_EGL=True`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `egl`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         0, `qtConfig(c++11):CONFIG += c++11`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `c++11`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         0, `qtConfig(opengles2): INCLUDEPATH += $$QMAKE_INCDIR_OPENGL_ES2`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `opengles2`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         0, `qtConfig(opengles2):qtConfig(combined-angle-lib):QMAKE_LIBS_OPENGL_ES2 = -l$${LIBQTANGLE_NAME} $$QMAKE_LIBS_OPENGL_ES2`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `opengles2`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         0, `qtConfig(debug_and_release): CONFIG += debug_and_release`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `debug_and_release`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         6, `win32:qtConfig(shared):QMAKE_DLL_PATHS += $$[QT_INSTALL_BINS/get]`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `shared`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         0, `qtConfig(force_asserts): DEFINES += QT_FORCE_ASSERTS`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `force_asserts`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        15, `import_plugins:qtConfig(static):manualplugs = $$QTPLUGIN`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        18, `isEmpty(QTPLUGIN):qtConfig(static):plug_type = $$eval(QT_PLUGIN.$${plug}.TYPE)`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        39, `host_build:force_bootstrap:!build_pass:qtConfig(release_tools): CONFIG += release`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `release_tools`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        12, `!build_pass:qtConfig(debug_and_release): CONFIG += release`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `debug_and_release`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         0, `qtConfig(c++11): CONFIG += c++11 strict_c++`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `c++11`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         0, `qtConfig(stack-protector-strong): CONFIG += stack_protector_strong`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `stack-protector-strong`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        31, `if(!host_build|!cross_compile):qtConfig(reduce_exports): CONFIG += hide_symbols`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `reduce_exports`);    

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         5, `unix:qtConfig(reduce_relocations): CONFIG += bsymbolic_functions`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `reduce_relocations`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         0, `qtConfig(separate_debug_info): CONFIG += separate_debug_info`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `separate_debug_info`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        30, `CONFIG(shared, static|shared):qtConfig(framework):export(QMAKE_FRAMEWORK_BUNDLE_NAME)`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `framework`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        28, `installed|if(!not_installed:qtConfig(static)): load(qt_installs)`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        11, `!if(static:qtConfig(shared)):QMAKE_CFLAGS += $$QMAKE_CFLAGS_SSE2`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `shared`);

    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
         4, `if(!qtConfig(debug_and_release)|CONFIG(release, debug|release)):CLEAN_HEADERS =`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `debug_and_release`);

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= `!isEmpty(QT_VERSION):qtConfig(simulator_and_device): \`;
    strLinesArray ~= `    sim_and_dev = true`;
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
       21, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `simulator_and_device`);

    strLinesArray = [];
    strLinesArray ~= `macx-xcode:qtConfig(static): \`;
    strLinesArray ~= `   QMAKE_XCODE_DEBUG_INFORMATION_FORMAT = dwarf`;
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
       11, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    strLinesArray = [];
    strLinesArray ~= `!no_qt_rpath:!static:qtConfig(rpath):!qtConfig(static):\`;
    strLinesArray ~= `    contains(all_qt_module_deps, core)\`;
    strLinesArray ~= `    QMAKE_RPATHDIR += $$[QT_INSTALL_LIBS/dev]`;
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
       21, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `rpath`);

    strLinesArray = [];
    strLinesArray ~= `!qtConfig(static)|host_build|no_import_scan: \`;
    strLinesArray ~= `    return()`;
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        1, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `static`);

    strLinesArray = [];
    strLinesArray ~= `qtConfig(framework): \`;
    strLinesArray ~= `    deppath.name = DYLD_FRAMEWORK_PATH`;
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `framework`);
    
    strLinesArray = [];
    strLinesArray ~= `qtConfig(rpath): \`;
    strLinesArray ~= `   QMAKE_SONAME_PREFIX = @rpath`;
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `rpath`);
    
    strLinesArray = [];
    strLinesArray ~= `qtConfig(private_tests): \   # -developer-build`;
    strLinesArray ~= `    QMAKE_SYNCQT += -check-includes`;
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        0, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `private_tests`);
    
    strLinesArray = [];
    strLinesArray ~= `isEmpty(hr)|qtConfig($$hr): \`;
    strLinesArray ~= `    CLEAN_HEADERS += $$member(hh, 0)`;
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
       12, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `$$hr`);
    
    strLinesArray = [];
    strLinesArray ~= `!qtConfig(simulator_and_device):contains(QMAKE_MAC_SDK, ^$${simulator.sdk}.*): \`;
    strLinesArray ~= `    addExclusiveBuildsProper(simulator_and_device, simulator device)`;
    result = extractFunctionArguments(FunctionBaseInfo(QTCONFIG_FUNCTION_STR, 1, 0),
        1, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `simulator_and_device`);

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 4) error

    // Single-line test snippets
    result = extractFunctionArguments(FunctionBaseInfo(ERROR_FUNCTION_STR, 1, 0),
        61, `write_file($$ANDROID_DEPLOYMENT_SETTINGS_FILE, FILE_CONTENT)|error()`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(ERROR_FUNCTION_STR, 1, 0),
         0, `error("QMAKE_ASSET_CATALOGS_BUILD_PATH must be set when using QMAKE_ASSET_CATALOGS.")`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `"QMAKE_ASSET_CATALOGS_BUILD_PATH must be set when using QMAKE_ASSET_CATALOGS."`);

    result = extractFunctionArguments(FunctionBaseInfo(ERROR_FUNCTION_STR, 1, 0),
         0, `error("Multiple modules claim plugin type '$$PLUGIN_TYPE' ($$mod, in addition to $$PLUGIN_MODULE_NAME)")`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `"Multiple modules claim plugin type '$$PLUGIN_TYPE' ($$mod, in addition to $$PLUGIN_MODULE_NAME)"`);

    result = extractFunctionArguments(FunctionBaseInfo(ERROR_FUNCTION_STR, 1, 0),
        21, `!load(device_config):error(Could not successfully load device configuration)`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `Could not successfully load device configuration`);

    result = extractFunctionArguments(FunctionBaseInfo(ERROR_FUNCTION_STR, 1, 0),
         0, `error($$join(msg, $$escape_expand(\\n)))`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `$$join(msg, $$escape_expand(\\n))`);

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= `!exists($$CMAKE_MODULE_TESTS): \`;
    strLinesArray ~= `    error("Missing CMake tests. Either create tests in tests/auto/cmake," \`;
    strLinesArray ~= `          "or disable cmake config file creation with CONFIG-=create_cmake.")`;
    result = extractFunctionArguments(FunctionBaseInfo(ERROR_FUNCTION_STR, 1, 0),
       31, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `"Missing CMake tests. Either create tests in tests/auto/cmake," "or disable cmake config file creation with CONFIG-=create_cmake."`);

    strLinesArray = [];
    strLinesArray ~= `isEmpty(VALID_ARCHS): \`;
    strLinesArray ~= `    error("QMAKE_APPLE_DEVICE_ARCHS or QMAKE_APPLE_SIMULATOR_ARCHS must contain at least one architecture")`;
    result = extractFunctionArguments(FunctionBaseInfo(ERROR_FUNCTION_STR, 1, 0),
       22, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `"QMAKE_APPLE_DEVICE_ARCHS or QMAKE_APPLE_SIMULATOR_ARCHS must contain at least one architecture"`);

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 5) system test/replace function tests

    // Corner case 1: empty argument
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        0, "system(uname -a, , ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "uname -a");
    assert(result.arguments[1] == "");
    assert(result.arguments[2] == "ec");
    
    // Single-line test snippets
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
         1, "!system($$pkg_config --exists $$package):return(false)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$pkg_config --exists $$package");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
         8, "ret = $$system(\"$$1 -E $$system_quote($$PWD/data/macros.cpp) 2>$$QMAKE_SYSTEM_NULL_DEVICE\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$1 -E $$system_quote($$PWD/data/macros.cpp) 2>$$QMAKE_SYSTEM_NULL_DEVICE\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
         2, "$$system(\"$$QMAKE_CXX -print-libgcc-file-name\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$QMAKE_CXX -print-libgcc-file-name\"");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        14, "sysrooted = $$system(\"/usr/bin/xcrun -sdk $$QMAKE_MAC_SDK -find $$first(value) 2>/dev/null\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"/usr/bin/xcrun -sdk $$QMAKE_MAC_SDK -find $$first(value) 2>/dev/null\"");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        11, "output = $$system(\"$$1\", lines, result)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$1\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "result");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        25, "cmake_version_output = $$system(cmake --version 2>$$QMAKE_SYSTEM_NULL_DEVICE, lines)", __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == "cmake --version 2>$$QMAKE_SYSTEM_NULL_DEVICE");
    assert(result.arguments[1] == "lines");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        18, "CTEST_VERSION = $$system(ctest --version 2>$$QMAKE_SYSTEM_NULL_DEVICE)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "ctest --version 2>$$QMAKE_SYSTEM_NULL_DEVICE");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
         0, "system($$QT_BREAKPAD_ROOT_PATH/qtbreakpadsymbols --breakpad-exists)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QT_BREAKPAD_ROOT_PATH/qtbreakpadsymbols --breakpad-exists");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        21, "PKGCONFIG_CFLAGS = $$system($$PKG_CONFIG --cflags $$PKGCONFIG_LIB)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$PKG_CONFIG --cflags $$PKGCONFIG_LIB");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
         9, "JSON = $$system($$QMLIMPORTSCANNER $$system_quote($$_PRO_FILE_PWD_) $$IMPORTPATHS)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMLIMPORTSCANNER $$system_quote($$_PRO_FILE_PWD_) $$IMPORTPATHS");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
         0, "system($$QMAKE_SYNCQT)|error(\"Failed to run: $$QMAKE_SYNCQT\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "$$QMAKE_SYNCQT");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        16, "rccContents = $$system($$QMAKE_RCC_DEP -list $$system_quote($$absRes),lines)", __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == "$$QMAKE_RCC_DEP -list $$system_quote($$absRes)");
    assert(result.arguments[1] == "lines");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        20, "remaining_files = $$system($$QML_CACHEGEN_FILTER -filter-resource-file -o $$system_quote($$new_resource) $$system_quote($$absRes),lines)", __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == "$$QML_CACHEGEN_FILTER -filter-resource-file -o $$system_quote($$new_resource) $$system_quote($$absRes)");
    assert(result.arguments[1] == "lines");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
         8, "ret = $$system(\"$$1 -nologo -E $$2 $$system_quote($$PWD/data/macros.cpp) 2>NUL\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$1 -nologo -E $$2 $$system_quote($$PWD/data/macros.cpp) 2>NUL\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        11, "output = $$system(\"$$cmd_prefix $$QMAKE_CXX $$qtMakeExpand($$cxx_flags) -xc++ - 2>&1 $$cmd_suffix\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$cmd_prefix $$QMAKE_CXX $$qtMakeExpand($$cxx_flags) -xc++ - 2>&1 $$cmd_suffix\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        11, "output = $$system(\"$$cmd_prefix $$QMAKE_LINK $$QMAKE_LFLAGS -print-search-dirs\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$cmd_prefix $$QMAKE_LINK $$QMAKE_LFLAGS -print-search-dirs\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        11, "output = $$system(\"$$cmd\", blob, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$cmd\"");
    assert(result.arguments[1] == "blob");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        11, "output = $$system(\"$$cmd 2>&1\", lines, ec)", __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$cmd 2>&1\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
         0, "system(\"$$QMAKE_CD $$system_quote($$OUT_PWD) && $$cmd\")", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$QMAKE_CD $$system_quote($$OUT_PWD) && $$cmd\"");
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        15, "WINRT_UUID = $$system(uuidgen)", __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "uuidgen");

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= "system(\"/usr/libexec/PlistBuddy -c 'Print NSPhotoLibraryUsageDescription' $$system_quote($$plist_path) &>/dev/null\"): \\";
    strLinesArray ~= "    QTPLUGIN += qiosnsphotolibrarysupport";
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"/usr/libexec/PlistBuddy -c 'Print NSPhotoLibraryUsageDescription' $$system_quote($$plist_path) &>/dev/null\"");

    strLinesArray = [];
    strLinesArray ~= "ret = $$system(\"$$1 -E $$system_quote($$PWD/data/macros.cpp) \\";
    strLinesArray ~= "     2>$$QMAKE_SYSTEM_NULL_DEVICE\", lines, ec)";
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == "\"$$1 -E $$system_quote($$PWD/data/macros.cpp) 2>$$QMAKE_SYSTEM_NULL_DEVICE\"");
    assert(result.arguments[1] == "lines");
    assert(result.arguments[2] == "ec");

    strLinesArray = [];
    strLinesArray ~= "!system(\"$$system_quote($$system_path($$[QT_HOST_BINS/src]/$$QT_LICHECK)) check\" \\";
    strLinesArray ~= "        \"$$QT_RELEASE_DATE $$[QMAKE_SPEC] $$[QMAKE_XSPEC]\"): \\";
    strLinesArray ~= "     error(\"License check failed! Giving up ...\")";
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == "\"$$system_quote($$system_path($$[QT_HOST_BINS/src]/$$QT_LICHECK)) check\" \"$$QT_RELEASE_DATE $$[QMAKE_SPEC] $$[QMAKE_XSPEC]\"");

    strLinesArray = [];
    strLinesArray ~= "actool_output_files = $$system(\\";
    strLinesArray ~= "        mkdir -p $$system_quote($$QMAKE_ASSET_CATALOGS_BUILD_PATH) && \\";
    strLinesArray ~= "        /usr/libexec/PlistBuddy -c \\'Print :com.apple.actool.compilation-results:output-files\\' \\";
    strLinesArray ~= "            /dev/stdin <<< $($${asset_catalog_compiler.commands} 2>/dev/null) | sed -Ene \\'s/^ +//p\\', lines)";
    result = extractFunctionArguments(FunctionBaseInfo(SYSTEM_FUNCTION_STR, 1, 2),
        0, mergeMultilineHelper(strLinesArray), __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == "mkdir -p $$system_quote($$QMAKE_ASSET_CATALOGS_BUILD_PATH) && /usr/libexec/PlistBuddy -c \\'Print :com.apple.actool.compilation-results:output-files\\' /dev/stdin <<< $($${asset_catalog_compiler.commands} 2>/dev/null) | sed -Ene \\'s/^ +//p\\'");
    assert(result.arguments[1] == "lines");

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 6) find

    // Single-line test snippets
    result = extractFunctionArguments(FunctionBaseInfo(FIND_FUNCTION_STR, 2, 0),
        26, `PKGCONFIG_INCLUDEPATH = $$find(PKGCONFIG_CFLAGS, ^-I.*)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `PKGCONFIG_CFLAGS`);
    assert(result.arguments[1] == `^-I.*`);

    result = extractFunctionArguments(FunctionBaseInfo(FIND_FUNCTION_STR, 2, 0),
        16, `ICONS_FOUND = $$find(TEMPLATE_CONTENTS, \\\$\\\$\\{WINRT_MANIFEST\\.(logo|tile)_)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `TEMPLATE_CONTENTS`);
    assert(result.arguments[1] == `\\\$\\\$\\{WINRT_MANIFEST\\.(logo|tile)_`);

    result = extractFunctionArguments(FunctionBaseInfo(FIND_FUNCTION_STR, 2, 0),
        35, `parser = $$lower($$member($$list($$find(sfl, "^%parser\\s")), 1))`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `sfl`);
    assert(result.arguments[1] == `"^%parser\\s"`);

    result = extractFunctionArguments(FunctionBaseInfo(FIND_FUNCTION_STR, 2, 0),
        25, `decl = $$member($$list($$find(sfl, "^%decl\\s")), 1)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `sfl`);
    assert(result.arguments[1] == `"^%decl\\s"`);

    result = extractFunctionArguments(FunctionBaseInfo(FIND_FUNCTION_STR, 2, 0),
        25, `impl = $$member($$list($$find(sfl, "^%impl\\s")), 1)`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `sfl`);
    assert(result.arguments[1] == `"^%impl\\s"`);

    result = extractFunctionArguments(FunctionBaseInfo(FIND_FUNCTION_STR, 2, 0),
        19, `$$csources_var = $$find($$sources_var, ".*\\.c$")`, __LINE__);
    assert(result.arguments.length == 2);
    assert(result.arguments[0] == `$$sources_var`);
    assert(result.arguments[1] == `".*\\.c$"`);

    // Multi-line test snippets
    // TODO:

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 7) re_escape

    // Single-line test snippets
    result = extractFunctionArguments(FunctionBaseInfo(REESCAPE_FUNCTION_STR, 1, 0),
        60, `!isEmpty(QMAKE_LINK_SHLIB_CMD):QMAKE_LINK_SHLIB_CMD ~= s/^$$re_escape($$QMAKE_LINK_SHLIB)$/$$QMAKE_LINK_C_SHLIB/`, __LINE__);
    assert(result.arguments.length == 1);
    assert(result.arguments[0] == `$$QMAKE_LINK_SHLIB`);

    // Multi-line test snippets
    // TODO:

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // 8) replace

    // Single-line test snippets
    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        68, `ANDROID_CXX_STL_LIBS = $$ANDROID_SOURCES_CXX_STL_LIBDIR/libc++.so.$$replace(ANDROID_PLATFORM, "android-", "")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `ANDROID_PLATFORM`);
    assert(result.arguments[1] == `"android-"`);
    assert(result.arguments[2] == `""`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        39, `defineReplace(emitString): return("\"$$replace(1, \\\\, \\\\)\"")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `1`);
    assert(result.arguments[1] == `\\\\`);
    assert(result.arguments[2] == `\\\\`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        22, `cmake_module_name = $$replace(_name, ^Qt, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `_name`);
    assert(result.arguments[1] == `^Qt`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         9, `return($$replace(path, ([^/])$, \\1/))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `path`);
    assert(result.arguments[1] == `([^/])$`);
    assert(result.arguments[2] == `\\1/`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        14, `BUILD_DIR = $$replace($$list($$OUT_PWD/build), /, $$QMAKE_DIR_SEP)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `$$list($$OUT_PWD/build)`);
    assert(result.arguments[1] == `/`);
    assert(result.arguments[2] == `$$QMAKE_DIR_SEP`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `grp = $$replace(group, _?dbus_$${dbus_type}\$, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `group`);
    assert(result.arguments[1] == `_?dbus_$${dbus_type}\$`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        28, `QT_COMPILER_STDCXX_no_L = $$replace(QT_COMPILER_STDCXX, "L$", "")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT_COMPILER_STDCXX`);
    assert(result.arguments[1] == `"L$"`);
    assert(result.arguments[2] == `""`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         0, `QMAKE_MAC_SDK_MAJOR_MINOR_VERSION = $$replace(QMAKE_MAC_SDK_VERSION, "(\\d+)(\\.\\d+)(\\.\\d+)?", \\1\\2)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_MAC_SDK_VERSION`);
    assert(result.arguments[1] == `"(\\d+)(\\.\\d+)(\\.\\d+)?"`);
    assert(result.arguments[2] == `\\1\\2`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        39, `QM_FILES += $$OUT_PWD/$$LRELEASE_DIR/$$replace(translation, \\..*$, .qm)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `translation`);
    assert(result.arguments[1] == `\\..*$`);
    assert(result.arguments[2] == `.qm`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        28, `VCLIBS = Microsoft.VCLibs.$$replace(MSVC_VER, \\., ).00`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `MSVC_VER`);
    assert(result.arguments[1] == `\\.`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        39, `contains(MSVC_VER, "15.0"): VCLIBS = $$replace(VCLIBS, 150, 140)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `VCLIBS`);
    assert(result.arguments[1] == `150`);
    assert(result.arguments[2] == `140`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        15, `base = qlalr_$$replace(sf, ^.*/([^./]+)[^/]*$, \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `sf`);
    assert(result.arguments[1] == `^.*/([^./]+)[^/]*$`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        15, `nu = $$upper($$replace(name, -, _))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        14, `URITARGET = $$replace(URI, "\\.", "_")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `URI`);
    assert(result.arguments[1] == `"\\."`);
    assert(result.arguments[2] == `"_"`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        26, `error("Library '$$lower($$replace(nu, _, -))' is not defined.")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `nu`);
    assert(result.arguments[1] == `_`);
    assert(result.arguments[2] == `-`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `URI = $$replace(TARGETPATH, "/", ".")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGETPATH`);
    assert(result.arguments[1] == `"/"`);
    assert(result.arguments[2] == `"."`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        19, `TARGETPATHBASE = $$replace(TARGETPATH, \\.\\d+\$, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGETPATH`);
    assert(result.arguments[1] == `\\.\\d+\$`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        22, `qmltypes.commands = $$replace(TARGETPATHBASE, /, .)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGETPATHBASE`);
    assert(result.arguments[1] == `/`);
    assert(result.arguments[2] == `.`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        13, `CLEAN_QT = $$replace(QT, -private$, _private)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT`);
    assert(result.arguments[1] == `-private$`);
    assert(result.arguments[2] == `_private`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        11, `nptype = $$replace(ptype, [-/], _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `ptype`);
    assert(result.arguments[1] == `[-/]`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         9, `$$1 -= $$replace(flag, ^-, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `flag`);
    assert(result.arguments[1] == `^-`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        43, `error("Unknown module(s) in QT$$var_sfx: $$replace(BAD_QT, _private$, -private)")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `BAD_QT`);
    assert(result.arguments[1] == `_private$`);
    assert(result.arguments[2] == `-private`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         9, `name = $$replace(name,/,_)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `/`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         9, `name = $$replace(name, \\.qrc$, _qmlcache.qrc)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `\\.qrc$`);
    assert(result.arguments[2] == `_qmlcache.qrc`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         9, `name = $$replace(name,\.\.,)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `\.\.`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         0, `name = $$replace(name,-,_)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         9, `deps = $$replace(QT, -private$, _private)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT`);
    assert(result.arguments[1] == `-private$`);
    assert(result.arguments[2] == `_private`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        23, `QMAKE_MODULE_PATH += $$replace(dirs, \$, /modules)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `dirs`);
    assert(result.arguments[1] == `\$`);
    assert(result.arguments[2] == `/modules`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `opt = $$replace(c, "^([A-Z0-9_]+)=(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^([A-Z0-9_]+)=(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `opt = $$replace(c, "^--?enable-(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^--?enable-(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `opt = $$replace(c, "^--?(disable|no)-(.*)", "\\2")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^--?(disable|no)-(.*)"`);
    assert(result.arguments[2] == `"\\2"`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `opt = $$replace(c, "^--([^=]+)=(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^--([^=]+)=(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `opt = $$replace(c, "^-(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `"^-(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `val = $$replace(opt, "(qt|system)-(.*)", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `opt`);
    assert(result.arguments[1] == `"(qt|system)-(.*)"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `val = $$replace(c, $$e, "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `c`);
    assert(result.arguments[1] == `$$e`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        20, `$${lpfx}.export = $$replace(l, -, _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `l`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        26, `isEmpty(alias): alias = $$replace(l, -, _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `l`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         7, `lp = $$replace(l, "^-L", )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `l`);
    assert(result.arguments[1] == `"^-L"`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        31, `USES = $$eval($$list($$upper($$replace(QMAKE_USE, -, _))))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_USE`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        17, `NAME = $$upper($$replace($${1}.library, -, _))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `$${1}.library`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        21, `depends += $$upper($$replace(use_lib, -, _))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `use_lib`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        11, `result = $$replace(e, "^'(.*)'$", "\\1")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `e`);
    assert(result.arguments[1] == `"^'(.*)'$"`);
    assert(result.arguments[2] == `"\\1"`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `var = $$replace(e, "^config\\.", "")`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `e`);
    assert(result.arguments[1] == `"^config\\."`);
    assert(result.arguments[2] == `""`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        48, `rhs = $$qtConfEvaluateSingleExpression($${1}, $$replace(e, ".*==", ""))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `e`);
    assert(result.arguments[1] == `".*=="`);
    assert(result.arguments[2] == `""`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        12, `feature = $$replace(name, [-+.], _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `name`);
    assert(result.arguments[1] == `[-+.]`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         8, `dep = $$replace($${currentConfig}.depends.$$d, -private$, _private)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `$${currentConfig}.depends.$$d`);
    assert(result.arguments[1] == `-private$`);
    assert(result.arguments[2] == `_private`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        17, `qtmver.value = $$replace(qtver.value, ^(\\d+\\.\\d+).*$, \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `qtver.value`);
    assert(result.arguments[1] == `^(\\d+\\.\\d+).*$`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        19, `qtvertag.value = $$replace(qtver.value, \\.,)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `qtver.value`);
    assert(result.arguments[1] == `\\.`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        22, `QMAKE_DOCS_TARGET = $$replace(QMAKE_DOCS, ^(.*/)?(.*)\\.qdocconf$, \\2)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_DOCS`);
    assert(result.arguments[1] == `^(.*/)?(.*)\\.qdocconf$`);
    assert(result.arguments[2] == `\\2`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        9, `deps = $$replace(QT, -private$, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT`);
    assert(result.arguments[1] == `-private$`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        10, `resrc = $$replace(rline, ^[ \\t]*<file[^>]*>([^<]+)</file>[ \\t]*$, \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `rline`);
    assert(result.arguments[1] == `^[ \\t]*<file[^>]*>([^<]+)</file>[ \\t]*$`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        10, `resrc = $$replace(rline, "^\\d+\\s+ICON\\s+[^\"]*\"([^\"]+)\"\$", \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `rline`);
    assert(result.arguments[1] == `"^\\d+\\s+ICON\\s+[^\"]*\"([^\"]+)\"\$"`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         2, `$$replace(_PRO_FILE_, \\.pro$, .qmlproject)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `_PRO_FILE_`);
    assert(result.arguments[1] == `\\.pro$`);
    assert(result.arguments[2] == `.qmlproject`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        11, `MODULE = $$replace(TARGET, ^qt, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGET`);
    assert(result.arguments[1] == `^qt`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        25, `QMAKE_PKGCONFIG_FILE = $$replace(TARGET, ^Qt, Qt$$QT_MAJOR_VERSION)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `TARGET`);
    assert(result.arguments[1] == `^Qt`);
    assert(result.arguments[2] == `Qt$$QT_MAJOR_VERSION`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        30, `QMAKE_PKGCONFIG_REQUIRES += $$replace(QT.$${i}.name, ^Qt, Qt$$section(QT.$${i}.VERSION, ., 0, 0))`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QT.$${i}.name`);
    assert(result.arguments[1] == `^Qt`);
    assert(result.arguments[2] == `Qt$$section(QT.$${i}.VERSION, ., 0, 0)`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        12, `fwd_hdr = $$replace(ofwd_hdr, ^\\^, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `ofwd_hdr`);
    assert(result.arguments[1] == `^\\^`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         0, `resource_name = $$replace(resource_name, [^a-zA-Z0-9_], _)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `resource_name`);
    assert(result.arguments[1] == `[^a-zA-Z0-9_]`);
    assert(result.arguments[2] == `_`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        21, `BUNDLEIDENTIFIER = $$replace(QMAKE_FRAMEWORK_BUNDLE_NAME, \\.framework$, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_FRAMEWORK_BUNDLE_NAME`);
    assert(result.arguments[1] == `\\.framework$`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         9, `file = $$replace(file, ^(\\.\\./)+, )`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `file`);
    assert(result.arguments[1] == `^(\\.\\./)+`);
    assert(result.arguments[2] == ``);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         6, `m = $$replace(out, ".*\\$\\(EXPORT_([^)]+)\\).*", \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `out`);
    assert(result.arguments[1] == `".*\\$\\(EXPORT_([^)]+)\\).*"`);
    assert(result.arguments[2] == `\\1`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         2, `$$replace(path, ^=, $$[SYSROOT])`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `path`);
    assert(result.arguments[1] == `^=`);
    assert(result.arguments[2] == `$$[SYSROOT]`);

    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
         6, `m = $$replace(out, ".*\\$\\(EXPORT_([^)]+)\\).*", \\1)`, __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `out`);
    assert(result.arguments[1] == `".*\\$\\(EXPORT_([^)]+)\\).*"`);
    assert(result.arguments[2] == `\\1`);

    // Multi-line test snippets
    strLinesArray = [];
    strLinesArray ~= `qmake_args += \`;
    strLinesArray ~= `    "QMAKE_DEPENDS_$${NAME}_CC = $$upper($$replace(dep_uses, -, _))" \`;
    strLinesArray ~= `    "QMAKE_DEPENDS_$${NAME}_LD = $$upper($$replace(dep_uses, -, _))"`;
    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        58, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `dep_uses`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    strLinesArray = [];
    strLinesArray ~= `for (ent, $$qtConfScalarOrList($${test}.test.qmake)): \`;
    strLinesArray ~= `   contents += $$replace(ent, "@PWD@", $$pwd)`;
    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        68, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `ent`);
    assert(result.arguments[1] == `"@PWD@"`);
    assert(result.arguments[2] == `$$pwd`);
    
    strLinesArray = [];
    strLinesArray ~= `!isEmpty(QT.$$replace(1, -, _).name): \`;
    strLinesArray ~= `    return(true)`;
    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
       14, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `1`);
    assert(result.arguments[1] == `-`);
    assert(result.arguments[2] == `_`);

    strLinesArray = [];
    strLinesArray ~= `!isEmpty(MODULE_PLUGIN_TYPES): \`;
    strLinesArray ~= `    module_plugtypes = "QT.$${MODULE_ID}.plugin_types = $$replace(MODULE_PLUGIN_TYPES, /[^.]+\\.[^.]+$, )"`;
    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
        85, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `MODULE_PLUGIN_TYPES`);
    assert(result.arguments[1] == `/[^.]+\\.[^.]+$`);
    assert(result.arguments[2] == ``);

    strLinesArray = [];
    strLinesArray ~= `QMAKE_CFLAGS_MSVC_COMPAT = $$replace(QMAKE_MSC_FULL_VER, "(..)(..)(.*)", \`;
    strLinesArray ~= `                                     "-fms-compatibility-version=\\1.\\2.\\3")`;
    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
       29, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `QMAKE_MSC_FULL_VER`);
    assert(result.arguments[1] == `"(..)(..)(.*)"`);
    assert(result.arguments[2] == `"-fms-compatibility-version=\\1.\\2.\\3"`);

    strLinesArray = [];
    strLinesArray ~= `!equals(WINSDK_VER, $$replace(winsdk_ver, ^(\\d+\\.\\d+).*$, \\1)): \`;
    strLinesArray ~= `    winsdk_ver =`;
    result = extractFunctionArguments(FunctionBaseInfo(REPLACE_FUNCTION_STR, 3, 0),
       22, mergeMultilineHelper(strLinesArray),  __LINE__);
    assert(result.arguments.length == 3);
    assert(result.arguments[0] == `winsdk_ver`);
    assert(result.arguments[1] == `^(\\d+\\.\\d+).*$`);
    assert(result.arguments[2] == `\\1`);

    writeln("All 227 preprocessor tests were successfully finished!");
}
