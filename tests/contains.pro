contains(DISTRO, squeeze) {
    message(test1)
}
contains(DISTRO, arch) {
    message(test2)
}
contains(DISTRO_OPTS, hard-float) {
    message(test3)
}
!contains(DISTRO_OPTS, aarch64) {
    message(test4)
}
contains(DISTRO_OPTS, deb-multi-arch): \
    message(test5)
contains(DISTRO_OPTS, deb-multi-arch) {
    message(test6)
}
contains(DISTRO_OPTS, boot2qt) {
    message(test7)
}
contains(B_REFSW_DEBUG, [Nn]) {
    message(test8)
}
contains(QMAKE_TARGET.arch, x86_64) {
    message(test9)
}

isEmpty(QMAKE_REL_RPATH_BASE)|!contains(INSTALLS, target): \
    message(test10)
contains(TEMPLATE, "vc.*") {
    message(test11)
}
contains(CMAKE_INSTALL_LIBS_DIR, ^(/usr)?/lib(64)?.*): CMAKE_USR_MOVE_WORKAROUND = $$CMAKE_INSTALL_LIBS_DIR
contains(CMAKE_INCLUDE_DIR, "^\\.\\./.*") {
    message(test13)
}
contains(CMAKE_LIB_DIR,"^\\.\\./.*") {
    message(test14)
}
contains(CMAKE_BIN_DIR, "^\\.\\./.*") {
    message(test15)
}
contains(CMAKE_PLUGIN_DIR, "^\\.\\./.*") {
    message(test16)
}
contains(CMAKE_DLL_DIR, "^\\.\\./.*") {
    message(test17)
}
contains(CONFIG, plugin) {
    message(test18)
}
contains(QT.$${mod}.plugin_types, $$PLUGIN_TYPE) {
    message(test19)
}
!contains(QT.$${dep}.module_config, no_link) {
    message(test20)
}
contains(SUBDIRS, sub_src): sub_examples.depends = sub_src
!contains(QT_BUILD_PARTS, examples): sub_examples.CONFIG = no_default_target no_default_install
contains(SUBDIRS, sub_src): sub_demos.depends = sub_src
!contains(QT_BUILD_PARTS, examples): sub_demos.CONFIG = no_default_target no_default_install
contains(SUBDIRS, sub_src): sub_tests.depends = sub_src   # The tests may have a run-time only dependency on other parts
!contains(QT_BUILD_PARTS, tests) {
    message(test26)
}
contains(QMAKE_HOST.os, Windows) {
    message(test27)
}
contains(TEMPLATE, .*lib) {
    message(test28)
}
contains(TEMPLATE, .*lib):!if(plugin:no_plugin_name_prefix): LIBPREFIX = lib
contains(TEMPLATE, .*subdirs): error("COPIES does not work with TEMPLATE=subdirs")
contains(TEMPLATE, ".*(lib|app)"):CONFIG += have_target
!contains(TARGET, .*phony_target.*): \   # monster hack, you don't really see this here, right? ;)
    message(test32)
!contains(QMAKE_LINK, "@:"):QMAKE_LINK = @echo linking $@ && $$QMAKE_LINK
!contains(QMAKE_INTERNAL_INCLUDED_FILES, $$MODULE_FWD_PRI): \ # before the actual include()!
    message(test34)
contains(TEMPLATE, subdirs) {
    message(test35)
}
contains(TEMPLATE, .*lib) {
    message(test36)
}
contains(QMAKE_DEFAULT_LIBDIRS, $$qt_libdir) {
    message(test37)
}
contains(apple_ver, "4\\.[012]|5\\.[01]|6\\.[01234]|7\\.[0123]")|contains(reg_ver, "[34]\\.|5\\.0") {
    message(test38)
}
linux:contains(ver, "(1[3456]\\.|17\\.0)") {
    message(test39)
}
contains(ver, "(4\\.[6789]|[5-9]\\..)") {
    message(test40)
}
contains(MSVC_VER, "1[124].0"): QMAKE_CXXFLAGS_WARN_ON += -WX
contains(TEMPLATE, ".*app"):!build_pass: {
    message(test42)
}
contains(TEMPLATE, ".*app") {
    message(test43)
}
!contains(TARGET, ".so"): TARGET = lib$${TARGET}.so
contains(TEMPLATE, "lib"):!static:!QTDIR_build:android_install {
    message(test45)
}
contains(QT_CPU_FEATURES, $$name) {
    message(test46)
}
!contains(QT_CPU_FEATURES, $${name}): $${name}_assembler.commands += $$cflags
!contains(QT_CPU_FEATURES, $$uppart): cpu_features_missing += $$uppart
!contains(QMAKE_INTERNAL_INCLUDED_FILES, $$MODULE_PRI): \ # before the actual include()!
    message(test49)
contains(QMAKE_HOST.os, Windows) {
    message(test50)
}
contains(QMAKE_HOST.os, Darwin) {
    message(test51)
}
contains(config, prepend): infix = \${$$name:+:\$$$name}
contains(config, always_prepend): infix = :\$$$name
contains(config, prepend) {
    message(test54)
}
contains(config, always_prepend) {
    message(test55)
}
contains(QMAKE_HOST.os, Linux|FreeBSD|OpenBSD|NetBSD|DragonFly|SunOS|HP-UX|QNX|GNU) {
    message(test56)
}
contains(QMAKE_HOST.os, Haiku) {
    message(test57)
}
contains(ptypes, $$eval(QT_PLUGIN.$${qplug}.TYPE)): \
    message(test58)
contains(subdir_config, no_default_target): next()
contains(subdir_config, no_$${target}_target): next()
contains(QT.$${module}.enabled_features, $$1): \
    message(test61)
contains(QT.$${module}.disabled_features, $$1): \
    message(test62)
!contains(QMAKE_RESOURCE_FLAGS, -root):!isEmpty(QMAKE_RESOURCE_ROOT):QMAKE_RESOURCE_FLAGS += -root $$QMAKE_RESOURCE_ROOT
!contains(QMAKE_RESOURCE_FLAGS, -name): QMAKE_RESOURCE_FLAGS += -name ${QMAKE_FILE_BASE}
contains(resource, ".*\\.qrc$"): \
    message(test65)
!resources_big|ltcg|macx-xcode|contains(TEMPLATE, "vc.*") {
    message(test66)
}
contains(TEMPLATE, "vc.*") {
    message(test67)
}
contains(TEMPLATE, "vc.*"): \
    message(test68)
!contains(WINRT_MANIFEST.CONFIG, "verbatim") {
    message(test69)
}
contains(MSVC_VER, "15.0"): VCLIBS = $$replace(VCLIBS, 150, 140)
contains(WINRT_MANIFEST.capabilities, defaults) {
    message(test71)
}
contains(WINRT_MANIFEST.capabilities_device, defaults) {
    message(test72)
}
contains(UAP_CAPABILITIES, $$CAPABILITY): \
    message(test73)
contains(UAP3_CAPABILITIES, $$CAPABILITY): \
    message(test74)
!contains(TEMPLATE, "vc.*") {
    message(test75)
}
contains(TEMPLATE, .*lib):dll: QMAKE_UIC_FLAGS += -no-stringliteral
!contains(QMAKE_EXTRA_TARGETS, check) {
    message(test77)
}
contains(TEMPLATE, subdirs): \
    message(test78)
!contains(QMAKE_EXTRA_TARGETS, benchmark) {
    message(test79)
}
contains(TEMPLATE, subdirs): \
    message(test80)
if(!equals(v, -framework):!contains(v, -L.*)) {
    message(test81)
}
contains(TEMPLATE, subdirs) {
    message(test82)
}
contains(QMAKE_DIR_REPLACE_SANE, $$dir) {
    message(test83)
}
contains($$dir, .*$${exclusive_affix}.*) {
    message(test84)
}
!contains($$dir, .*$${dir_affix}.*) {
    message(test85)
}
contains($$dir, .*/$) {
    message(test86)
}
contains(INSTALLS, target): asset_catalogs_files.depends += install_target
!contains(bundle_file, .*\.xcassets$): next()
contains(VALID_SIMULATOR_ARCHS, $$arch) {
    message(test89)
}
contains(QMAKE_MAC_SDK, .*/.*): \
    message(test90)
!contains(QT.$${dep}.module_config, no_link): \
    message(test91)
contains(sf, \\..*) {
    message(test92)
}
isEmpty(probase)|contains(probase, ^\\..*): \
    message(test93)
contains(TEMPLATE, "vc.*"): \
    message(test94)
contains(TEMPLATE, .*app): \
    message(test95)
!equals(inst, target):!contains($${inst}.CONFIG, no_check_exist): \
    message(test96)
!contains(INSTALLS, target) {
    message(test97)
}
contains(QMAKE_HOST.os, Windows) {
    message(test98)
}
contains(line, "^[^ ]*cc1plus .*") {
    message(test99)
}
contains(line, "^[^ ]*-ld .*") {
    message(test100)
}
contains(parameter, "^-L.*") {
    message(test101)
}
contains(line, "LIBRARY_PATH=.*") {
    message(test102)
}
contains(line, "Library search paths:") {
    message(test103)
}
contains(line, "$${LITERAL_HASH}include <.*") {  # #include <...> search starts here:
    message(test104)
}
contains(line, "End of search.*") {
    message(test105)
}
!contains(line, "^/.*") {
    message(test106)
}
!contains(line, ".* \\(framework directory\\)"): \
    message(test107)
contains(line, "^libraries: .*") {
    message(test108)
}
isEmpty(v)|contains(v, $${LITERAL_HASH}.*): next()
contains(v, $${LITERAL_HASH}.*)|contains(v, " *"): next()
shared:contains(TEMPLATE, lib) {
    message(test111)
}
!contains(QMAKE_INTERNAL_INCLUDED_FILES, $$TOOL_PRI) { # before the actual include()!
    message(test112)
}
!contains(use, nolink) {
    message(test113)
}
!contains(use, linkonly) {
    message(test114)
}
!qtConfig(simulator_and_device):contains(QMAKE_MAC_SDK, ^$${simulator.sdk}.*): \
    message(test115)
!watchos:equals(TEMPLATE, app):contains(qt_depends, gui(-private)?) {
    message(test116)
}
$$sim_and_dev|contains(QMAKE_MAC_SDK, ^$${device.sdk}.*): \
    message(test117)
$$sim_and_dev|contains(QMAKE_MAC_SDK, ^$${simulator.sdk}.*): \
    message(test118)
!isEmpty(DESTDIR):contains(TEMPLATE, lib) {
    message(test119)
}
!contains(QT_ARCH, arm):!contains(QT_ARCH, mips): \
    message(test120)
c++11:contains(QMAKE_CXXFLAGS_CXX11, -std=gnu++11) {
    message(test121)
}
contains(QMAKE_CXXFLAGS, -std=gnu++11) {
    message(test122)
}
c++11:contains(QMAKE_CXXFLAGS_CXX11, -std=gnu++0x) {
    message(test123)
}
contains(QMAKE_CXXFLAGS, -std=gnu++0x) {
    message(test124)
}
!c++11:!contains(QMAKE_CXXFLAGS, -std=c++0x):!contains(QMAKE_CXXFLAGS, -std=c++11) {
    message(test125)
}
contains(TEMPLATE, ".*lib"): CONFIG += staticlib
contains(TEMPLATE, ".*lib"): CONFIG += dll
contains(QT_CONFIG, debug): \
    message(test128)
!contains(QMAKE_INTERNAL_INCLUDED_FILES, .*qmodule\\.pri) {
    message(test129)
}
!contains(QTREPOS, $$MODULE_BASE_OUTDIR): \
    message(test130)
!contains(QMAKEMODULES, $$modpath): \
    message(test131)
!contains(deps, qml): \
    message(test132)
contains(bp, $$1): return(true)
contains(QT.$${module}.enabled_features, $$1): \
    message(test134)
contains(QT.$${module}.disabled_features, $$1): \
    message(test135)
contains(val, "^-.*")|isEmpty(val) {
    message(test136)
}
contains(val, "^-.*|[A-Z_]+=.*")|isEmpty(val): \
    message(test137)
contains(val, "^-.*")|isEmpty(val) {
    message(test138)
}
contains(c, "([A-Z_]+)=(.*)") {
    message(test139)
}
contains(c, "^--?enable-(.*)") {
    message(test140)
}
contains(c, "^--?(disable|no)-(.*)") {
    message(test141)
}
contains(c, "^--([^=]+)=(.*)") {
    message(test142)
}
contains(c, "^--(.*)") {
    message(test143)
}
contains(c, "^-(.*)") {
    message(test144)
}
isEmpty(type):contains(opt, "(qt|system)-.*") {
    message(test145)
}
contains(c, $$e) {
    message(test146)
}
isEmpty(type):contains(opt, "feature-(.*)") {
    message(test147)
}
contains($${cc}.features._KEYS_, $$opt) {
    message(test148)
}
contains(i, "-I.*") {
    message(test149)
}
contains(i, "-D.*") {
    message(test150)
}
contains($${currentConfig}.libraries._KEYS_, $$u) {
    message(test151)
}
contains(f, ".*\.h") {
    message(test152)
}
contains(f, ".*\.(lib|so|a)") {
    message(test153)
}
contains(e, "^[0-9]+$") {
    message(test154)
}
contains(e, "^'.*'$") {
    message(test155)
}
contains(e, "^tests\..*") {
    message(test156)
}
!contains($${currentConfig}.tests._KEYS_, $$test): \
    message(test157)
contains(e, "^libs\..*") {
    message(test158)
}
!contains($${currentConfig}.libraries._KEYS_, $$lib): \
    message(test159)
contains(e, "^features\..*") {
    message(test160)
}
!contains($${currentConfig}.features._KEYS_, $$feature) {
    message(test161)
}
contains(QT.$${module}.enabled_features, $$feature): \
    message(test162)
contains(QT.$${module}.disabled_features, $$feature): \
    message(test163)
contains(e, "^config\..*") {
    message(test164)
}
contains(CONFIG, $$var): result = true
contains(e, "^module\..*") {
    message(test165)
}
contains(e, "^arch\..*") {
    message(test166)
}
contains(QT_ARCH, $$var): result = true
contains(e, "^input\..*") {
    message(test168)
}
contains(e, "^var\..*") {
    message(test169)
}
contains(e, "^call\..*") {
    message(test170)
}
contains(e, ".*==.*") {
    message(test171)
}
contains(e, ".*!=.*") {
    message(test172)
}
!contains($${currentConfig}.features._KEYS_, $$2): \
    message(test173)
contains(subKeys, condition) {
    message(test174)
}
contains(subKeys, "section") {
    message(test175)
}
contains($${currentConfig}.output.$${output}.remove.$${name}, $$value): \
    message(test176)
!contains($${currentConfig}._KEYS_, "features"): \
    message(test177)
contains(type, ".*Pro") {
    message(test178)
}
contains(type, ".*Header") {
    message(test179)
}
contains($${currentConfig}._KEYS_, libraries) {
    message(test180)
}
!force_import_plugins:!contains(TEMPLATE, ".*app"):!if(contains(TEMPLATE, ".*lib"):dll): \
    message(test181)
!contains(all_qt_module_deps, $$dep) {
    message(test182)
}
contains(flag, ^-.*): \
    message(test183)
contains(MODULE_CONFIG, internal_module): \
    message(test184)
contains(MODULE_CONFIG, ltcg): \
    message(test185)
!contains(MODULE_CONFIG, v2) {
    message(test186)
}
contains(MODULE_CONFIG, lib_bundle) {
    messae(test187)
}
contains(MODULE_CONFIG, internal_module): \
    message(test188)
!isEmpty(MODULE_LIBS):!contains(MODULE_CONFIG, no_link): \
    message(test189)
!if(contains(MODULE_CONFIG, lib_bundle):qt_no_framework_direct_includes): \
    message(test190)
contains(MODULE_CONFIG, internal_module): \
    message(test191)
contains(MODULE_CONFIG, lib_bundle) {
    message(test192)
}
contains(MODULE_CONFIG, staticlib): \
    message(test193)
contains(all_qt_module_deps, core) {
    message(test194)
}
relative_qt_rpath:!isEmpty(QMAKE_REL_RPATH_BASE):contains(INSTALLS, target):\
    message(test195)
contains(all_qt_module_deps, qml): \
    message(test196)
!contains(ADDED_IMPORTS, $$PLUGIN)  {
    message(test197)
}
!isEmpty(TESTRUN_CWD):!contains(TESTRUN_CWD, ^\\./?): \
    message(test198)
!no_testcase_installs:!contains(INSTALLS, target) {
    message(test199)
}
!builtin_testdata:contains(INSTALLS, target) {
    message(test200)
}
contains(QT, core(-private)?|xml) {
    message(test201)
}
contains(MODULE_DEPENDS, $$MODULE): \
    message(test202)
contains(TARGET, QtAddOn.*): \
    message(test203)
sse2:!contains(QT_CPU_FEATURES.$$QT_ARCH, sse2):!host_build:!if(static:qtConfig(shared)) {
    message(test204)
}
equals(QT_ARCH, i386):contains(QT_CPU_FEATURES.$$QT_ARCH, sse2):compiler_supports_fpmath {
    message(test205)
}
contains(QT_PRODUCT, OpenSource.*):DEFINES *= QT_OPENSOURCE
contains(QT, core(-private)?|xml) {
    message(test207)
}
contains(INSTALLS, target):isEmpty(target.files):isEmpty(target.commands):isEmpty(target.extra) {
    message(test208)
}
!contains(QMAKE_HOST.os, Windows): \
    message(test209)
!contains(subent, .*\\w\\.xml$) {
    message(test210)
}
contains(QMAKE_LEX, .*flex) {
    message(test211)
}
contains(QMAKE_PLATFORM, macx) {
    message(test212)
}
!contains(QMAKE_PLATFORM, osx) {
    message(test213)
}
!contains(QMAKE_PLATFORM, macos) {
    message(test214)
}
contains(qmlf,.*\\.js$)|contains(qmlf,.*\\.qml$) {
    message(test215)
}
contains(TEMPLATE, vc.*): DEFINES += QT_TESTCASE_BUILDDIR=\"$$OUT_PWD\"
contains(TEMPLATE, ".*app") {
    message(test217)
}
!contains(QMAKE_DEFAULT_LIBDIRS, $$QT.core.libs): \
    message(test218)
contains(TEMPLATE, "vc.*") {
    message(test219)
}
if(isEmpty(BUILDS)|build_pass):have_target:!contains(TEMPLATE, vc.*) {
    message(test220)
}
contains(TEMPLATE, ".*app") {
    message(test221)
}

