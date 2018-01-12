
requires(if(embedded|x11):qtHaveModule(gui))

#################################################################################################

# features/incredibuild_xge.prf
{
    $${xge}.commands = Rem IncrediBuild_AllowRemote $$EOC Rem IncrediBuild_OutputFile $$system_path($${xge}.output) $$EOC $$eval($${xge}.commands)
}

# features/create_cmake.prf
CMAKE_SOURCE_PRIVATE_INCLUDES = \
    $$cmakeTargetPaths($$QT_MODULE_INCLUDE_BASE/Qt$${CMAKE_MODULE_NAME}/$$eval(QT.$${MODULE}.VERSION) \
    $$QT_MODULE_INCLUDE_BASE/Qt$${CMAKE_MODULE_NAME}/$$eval(QT.$${MODULE}.VERSION)/Qt$${CMAKE_MODULE_NAME})

cmake_extra_source_includes.input = $$PWD/data/cmake/ExtraSourceIncludes.cmake.in

{
    CMAKE_LIB_FILE_LOCATION_DEBUG = lib$${CMAKE_QT_STEM}_debug.$$eval(QT.$${MODULE}.VERSION).dylib
    CMAKE_LIB_FILE_LOCATION_RELEASE = lib$${CMAKE_QT_STEM}.$$eval(QT.$${MODULE}.VERSION).dylib
}

{
    CMAKE_LIB_FILE_LOCATION_DEBUG = lib$${CMAKE_QT_STEM}.so.$$eval(QT.$${MODULE}.VERSION)
    CMAKE_LIB_FILE_LOCATION_RELEASE = lib$${CMAKE_QT_STEM}.so.$$eval(QT.$${MODULE}.VERSION)
    CMAKE_LIB_SONAME = lib$${CMAKE_QT_STEM}.so.$$section(QT.$${MODULE}.VERSION, ., 0, 0)
}

CMAKE_PACKAGE_VERSION = $$eval(QT.$${MODULE}.VERSION)

# features/qt_parts.prf
bp = $$eval($$upper($$TARGET)_BUILD_PARTS)

# features/repccommon.pri
files = $$eval($${entry}.files)

# features/file_copies.prf

### FIXME: important case
eval(defineReplace(qtStripSrcDir_$$cp) { \
    return(\$\$relative_path(\$\$1, $$val_escape(base))) \
})
###

$$dir:$$notdir: \
    error("COPIES entry $$cp lists both files and directories.")
path = $$eval($${cp}.path)
isEmpty(path): error("COPIES entry $$cp defines no .path")
base = $$eval($${cp}.base)
isEmpty(base) {
    $${pfx}.output = $$path/${QMAKE_FILE_IN_NAME}
} else: isEqual(base, $$_PRO_FILE_PWD_) {
    $${pfx}.output = $$path/${QMAKE_FUNC_FILE_IN_qtStripProPwd}
} else {
    eval(defineReplace(qtStripSrcDir_$$cp) { \
        return(\$\$relative_path(\$\$1, $$val_escape(base))) \
    })
    $${pfx}.output = $$path/${QMAKE_FUNC_FILE_IN_qtStripSrcDir_$$cp}
}

# features/default_post.prf
QMAKE_CXXFLAGS += $$eval(QMAKE_CXXFLAGS_$$cxxstd)
QMAKE_LFLAGS += $$eval(QMAKE_LFLAGS_$$cxxstd)

# features/qml_plugin.prf
isEmpty(TARGETPATH): TARGETPATH = $$eval(QT.$${CXX_MODULE}.name)

# features/simd.prf
QT_CPU_FEATURES = $$eval(QT_CPU_FEATURES.$$QT_ARCH)
cflags = $$eval(QMAKE_CFLAGS_$${upname})
SOURCES += $$eval($$sources_var)
silent: $${name}_compiler.commands = @echo compiling[$${name}] ${QMAKE_FILE_IN} && $$eval($${name}_compiler.commands)
SOURCES += $$eval($$asm_var)
silent: $${name}_assembler.commands = @echo assembling[$${name}] ${QMAKE_FILE_IN} && $$eval($${name}_assembler.commands)
HEADERS += $$eval($$headers_var)
cflags *= $$eval(QMAKE_CFLAGS_$${uppart})

# features/qt_functions.prf
cmd = $$eval(QT_TOOL.$${2}.binary)
QT_TOOL_ENV += $$eval(QT_TOOL.$${2}.envvars)
value = $$eval($${env}.value)
name = $$eval($${env}.name)
config = $$eval($${env}.CONFIG)
cmd = $$eval($$1)

isEmpty(3): \
    deppath += $$shell_path($$eval(QT.$${dep}.$$libs))
else: \
    deppath += $$system_path($$eval(QT.$${dep}.$$libs))

ptypes += $$eval(QT.$${dep}.plugin_types)

for(qplug, QT_PLUGINS): \
    contains(ptypes, $$eval(QT_PLUGIN.$${qplug}.TYPE)): \
        ppaths += $$eval(QT_PLUGIN.$${qplug}.PATH)

subdir_config = $$eval($${subdir}.CONFIG)

# mkspecs/resources.prf
prefix = $$eval($${resource}.prefix)
abs_base = $$absolute_path($$eval($${resource}.base), $$_PRO_FILE_PWD_)

# mkspecs/winrt/package_manifest.prf
eval($$WINRT_UUID)

ICON_FILE = $$eval(WINRT_MANIFEST.$$ICON_NAME)
isEmpty(ICON_FILE) {
    equals(ICON_NAME, "logo_310x150"): ICON_FILE = $$eval(WINRT_MANIFEST.logo_wide)
    else: equals(ICON_NAME, "logo_150x150"): ICON_FILE = $$eval(WINRT_MANIFEST.logo_large)
    # Windows Phone specifics
    else: equals(ICON_NAME, "logo_480x800"): ICON_FILE = $$eval(WINRT_MANIFEST.logo_splash)
    else: equals(ICON_NAME, "logo_71x71"): ICON_FILE = $$eval(WINRT_MANIFEST.logo_medium)
    else: equals(ICON_NAME, "logo_44x44"): ICON_FILE = $$eval(WINRT_MANIFEST.logo_small)
    # Windows RT specifics
    else: equals(ICON_NAME, "logo_620x300"): ICON_FILE = $$eval(WINRT_MANIFEST.logo_splash)
    else: equals(ICON_NAME, "logo_70x70"): ICON_FILE = $$eval(WINRT_MANIFEST.logo_medium)
    else: equals(ICON_NAME, "logo_30x30"): ICON_FILE = $$eval(WINRT_MANIFEST.logo_small)
}

# features/cmake_functions.prf
_name = $$eval(QT.$${_module}.name)

# features/exclusive_builds_post.prf
newExistingBuilds = $$eval($$first(permutationBuilds).exclusive)
key = $${key}$$eval($${build}.name)
config *= $$eval($${build}.CONFIG) $${build} $$eval($${build}.name)Build
target += $$eval($${build}.target)
$${build}.depends += $$eval($${key}.target)
BUILDS = $$eval(BUILDS.$$size(priority)) $$BUILDS
for(build, builds): \
    affixes += $$eval($${build}.dir_affix)
$$dir = $$clean_path($$eval($$dir)/$$full_dir_affix)
build_affix = $$eval($${build}.dir_affix)
exclusive_affix = $$eval($${exclusive}.dir_affix)
dir_affix = $$eval($${build}.dir_affix)
$$dir = $$eval($$dir)$$dir_affix
$$dir = $$eval($$dir)-$${dir_affix}

# features/mac/sdk.prf
QMAKE_MAC_SDK.$${sdk}.$${info}
QMAKE_MAC_SDK.$${sdk}.$${info} = test_1
eval(QMAKE_MAC_SDK.$${sdk}.$${info})
return($$eval(QMAKE_MAC_SDK.$${sdk}.$${info}))
$$tool = $$eval($$tool_variable)
value = $$eval($$tool)

# features/ctest_testcase_common.prf
ver = $$eval(QT.$${MODULE_UNDER_TEST}.VERSION)

# features/qt_example_installs.prf
sd = $$eval($${i}.file)
sd = $$eval($${i}.subdir)

# features/toolchain.prf
out = $$replace(out, "\\$\\(EXPORT_$$m\\)", $$eval($$m))
{
    QMAKE_DEFAULT_INCDIRS = $$eval($${target_prefix}.INCDIRS)
    QMAKE_DEFAULT_LIBDIRS = $$eval($${target_prefix}.LIBDIRS)
}
eval($$v)
$$i = $$eval($${target_prefix}.$$i)

# features/qmake_use.prf
!contains(use, nolink) {
    QMAKE_LIBDIR += $$eval(QMAKE_LIBDIR_$$nu)
    debug: \
        LIBS$${suffix} += $$eval(QMAKE_LIBS_$${nu}_DEBUG) $$eval(QMAKE_LIBS_$$nu)
    else: \
        LIBS$${suffix} += $$eval(QMAKE_LIBS_$${nu}_RELEASE) $$eval(QMAKE_LIBS_$$nu)
}
!contains(use, linkonly) {
    DEFINES += $$eval(QMAKE_DEFINES_$${nu})
    INCLUDEPATH += $$eval(QMAKE_INCDIR_$${nu})
}

# features/uikit/xcodebuild.prf
SUBTARGETS += $$eval($${build}.target)
distfiles += $$title($$eval($${build}.target))

# features/qt_module_headers.prf
depname = $$eval(QT.$${dep}.master_header)
isEmpty(depname): \
    depname = $$eval(QT.$${dep}.name)

# features/qt_build_config.prf
bp = $$eval($$upper($$section(_QMAKE_CONF_, /, -2, -2))_BUILD_PARTS)

# features/qt_configure.prf
for (i, $${1}._KEYS_): \
    $$vals += $$eval($${1}.$$i)
!isEmpty($${currentConfig}.commandline.options.$${arg}.name): \
    arg = $$eval($${currentConfig}.commandline.options.$${arg}.name)
val = $$eval($${currentConfig}.commandline.options.$${opt}.value)
mapped = $$eval($${currentConfig}.commandline.options.$${opt}.values.$${val})
validValues = $$eval($${currentConfig}.commandline.options.$${opt}.values._KEYS_)
!isEmpty($${currentConfig}.commandline.options.$${opt}.name): \
    opt = $$eval($${currentConfig}.commandline.options.$${opt}.name)
custom = $$eval($${cc}.commandline.custom)
var = $$eval($${cc}.commandline.assignments.$${opt})
type = $$eval($${cc}.commandline.options.$${opt})
type = $$eval($${cc}.commandline.options.$${opt}.type)

type = $$eval($${cc}.commandline.options.$${opt})
isEmpty(type): \
    type = $$eval($${cc}.commandline.options.$${opt}.type)
contains(c, $$e) {
    opt = $$eval($${cc}.commandline.prefix.$${p})
}
flag = $$eval($${1}.flag)
alias = $$eval($${lpfx}.export)
$${spfx}.libs = $$eval($${spfx})
ra = config.commandline.rev_assignments.$$eval($$apfx)
lib = $$eval($${1}.library)
input = $$eval($${2}.alias)
$${1}.libs = $$eval(config.input.$${input}.libs)
vars += $$eval(config.commandline.rev_assignments.$${iv})
defined(config.input.$${iv}, var) {
    $${1}.builds.$${b}.libs = $$eval(config.input.$${iv})
}
$${1}.libs = "-L$$prefix/lib $$eval($${1}.libs)"
$${1}.libs = "-L$$libdir $$eval($${1}.libs)"
spec = $$eval($${1}.spec)
isEmpty(spec): \
    error("makeSpec source in library '$$eval($${1}.library)' does not specify 'spec'.")
libs += $$eval(QMAKE_LIBS_$$spec)
pkg_config = $$qtConfPkgConfig($$eval($${1}.host))
args = $$qtConfPrepareArgs($$eval($${1}.args))
pkg_config = $$qtConfPkgConfig($$eval($${1}.host))
args = $$qtConfPrepareArgs($$eval($${1}.pkg-config-args))
variable = $$eval($${1}.pkg-config-variable)
libs = $$eval($${1}.libs)
for (b, $${1}.builds._KEYS_): \
    qmake_args += $$system_quote(LIBS_$$upper($$b) += $$eval($${1}.builds.$${b}))
includedir = $$eval($${1}.includedir)
cflags = $$eval($${1}.cflags)
alias = $$eval($${lpfx}.alias)
name = $$eval($${lpfx}.export)
spfx = $${lpfx}.sources.$$eval($${lpfx}.source)

###
$${spfx}.export
eval($${spfx}.export)
qtConfEvaluate($$eval($${spfx}.export))
$$qtConfEvaluate($$eval($${spfx}.export))
!$$qtConfEvaluate($$eval($${spfx}.export)): return()
###

###
$${spfx}.libs
eval($${spfx}.libs)
$$eval($${spfx}.libs)
eval($$eval($${spfx}.libs))
libs = $$eval($${spfx}.libs)
eval(libs)
eval(libs = test)
#
eval(libs = $$eval($${spfx}.libs))
###

eval(cflags = $$eval($${spfx}.cflags))
eval(includes = $$eval($${spfx}.includedir))
qtConfOutputVar(assign, $$output, QMAKE_LIBS_$${NAME}_$$upper($$b), \
    $$eval($${spfx}.builds.$${b}))
alias = $$eval($${lpfx}.alias)
use_args = $$eval($${lpfx}.literal_args)
$$eval($${lpfx}.result): \
    qtConfExportLibrary($$1)
t = $$eval($${spfx}.type)
cond = $$eval($${spfx}.condition)
$${lpfx}.host = $$eval($${spfx}.host)
use = $$eval($${1}.use.$${k}.lib)
!$$qtConfEvaluate($$eval($${1}.use.$${k}.condition)): \
    next()
exports = $$eval($${currentConfig}.exports.$$u)
ru = $$eval($${currentConfig}.found.$$u)
exports = $$eval($${d}.exports.$$u)
ru = $$eval($${d}.found.$$u)
$${1}.literal_args += $$qtConfLibraryArgs($${lpfx}.sources.$$eval($${lpfx}.source))
test = $$eval($${1}.test)
host = $$eval($${1}.host)
!isEmpty($${1}.pro): \
    test_dir = $$test_dir/$$eval($${1}.pro)
qmake_args += $$qtConfPrepareArgs($$eval($${1}.args)) $$eval($${1}.literal_args)
f = $$eval($${1}.files.$${i})
label = $$eval($${1}.label)
field = $$eval($${1}.log)
log_msg = $$eval($${1}.$$field)
keys = result $$eval($${1}.cache)
equals(QMAKE_CONFIG_CACHE_USE, positive):!$$eval(cache.$${2}.result): \
    return(false)
for (k, cache.$${2}._KEYS_) {
    $${1}.$${k} = $$eval(cache.$${2}.$${k})
}
feature = $$eval($${currentConfig}.testTypeDependencies.$${tt}.$${f})
type = $$eval($${currentConfig}.testTypeAliases.$${tt}.$${tta})
feature = $$eval($${currentConfig}.testTypeDependencies.$${1}.$${dep})
type = $$eval($${tpfx}.type)
!isEmpty(QT.$${module}.skip):$$eval(QT.$${module}.skip): \
    return(false)
expr_list = $$eval($$list($$expr))
result = $$eval($${currentConfig}.tests.$${test}.$${var})
!defined($${currentConfig}.libraries.$${lib}.$${var}, var): \
    var = sources.$$eval($${currentConfig}.libraries.$${lib}.source).$$var
result = $$eval($${currentConfig}.libraries.$${lib}.$${var})
result = $$eval($${currentConfig}.features.$${feature}.$${var})    
result = $$eval(config.$$e)
result = $$eval($$var)

### FIXME: important case #############
$$call()
"$$call"()
$$"$$call"()
\$\$"$$call"()
result = \$\$"$$call"()
eval(\$\$"$$call"())
eval(result = true)
eval(result = \$\$"$$call"())
#######################################

enable = $$eval($${currentConfig}.features.$${1}.enable)
disable = $$eval($${currentConfig}.features.$${1}.disable)
available = $$eval($${fpfx}.available)
emitIf = $$qtConfEvaluate($$eval($${fpfx}.emitIf))

!$$enabled:!$$qtConfEvaluate($$eval($${fpfx}.autoDetect)) {
    result = false
}

!$$qtConfEvaluate($$eval($${currentConfig}.condition)): \
    QT.$${currentModule}.skip = true

$$eval($${currentConfig}.features.$${feature}.available): \
    l += $$eval($${currentConfig}.features.$${feature}.label)

$$eval($${currentConfig}.features.$${feature}.available): \
    return($$eval($${currentConfig}.features.$${feature}.label))

$$eval($${currentConfig}.features.$${2}.available) {
    result = "yes"
    !isEmpty(3): result = "$${3}"
}
text = $$eval($${currentConfig}.features.$${2}.label)
keys = $$eval($${1}._KEYS_)
subKeys = $$eval($${entry}._KEYS_)
condition = $$eval($${entry}.condition)
section = $$eval($${entry}.section)
feature = $$eval($${entry})
text = $$eval($${entry}.message)
r = "qtConfReport_$$eval($${entry}.type)"
!defined($$r, test): \
    error("Undefined report type $$eval($${entry}.type) used in report entry $${entry}.")
args = $$eval($${entry}.args)

!isEmpty($${2}.public):$$eval($${2}.public) {
    output = "publicPro"
} else {
    output = "privatePro"
}
negative = $$eval($${2}.negative)

name = $$eval($${2}.name)
isEmpty(name): \
    error("Output type 'var$$title($$1)' used in feature '$$eval($${2}.feature)' without a 'name' entry.")

value = $$qtConfEvaluate($$eval($${2}.value))
    !isEmpty($${2}.eval):$$qtConfEvaluate($$eval($${2}.eval)): \
        eval(value = $$value)

negative = $$eval($${1}.negative)
val = $$eval($${1}.name)
val = $$eval($${1}.feature)

!isEmpty($${currentConfig}.module): \
    error("Cannot use output type 'publicConfig' in module-local feature '$$eval($${1}.feature)'.")

define = $$eval($${1}.name)
value = $$qtConfEvaluate($$eval($${1}.value))
isEmpty(define): \
    error("Output type 'define' used in feature '$$eval($${1}.feature)' without a 'name' entry.")
negative = $$eval($${1}.negative)

name = "$$eval($${1}.name)"
isEmpty(name): \
    name = $$eval($${1}.feature)

currentModule = $$eval($${currentConfig}.module)

name = "$$eval($${1}.name)"
isEmpty(name): \
    name = $$eval($${1}.feature)

name = "$$eval($${1}.name)"
isEmpty(name): \
    name = $$eval($${1}.feature)

call = $$eval($${opfx}.type)
isEmpty(call) {
    call = $$eval($$opfx)
}

!$$qtConfEvaluate($$eval($${opfx}.condition)): \
    return()

qtConfOutput_$${call}($$opfx, $$eval($${fpfx}.available))

basedir = $$shadowed($$eval($${currentConfig}.dir))
module = $$eval($${currentConfig}.module)

$${currentConfig}.output.$$type += "$$k = $$eval($${currentConfig}.output.$${type}.assign.$$k)"
value = $$eval($${currentConfig}.output.$${type}.$${define})
content = $$eval($${currentConfig}.output.$${type})

### FIXME: important case
eval(content = \$\$"$$call"(\$\$content))

file = $$eval($${currentConfig}.files.$${type})
s = $$eval(config.$${c}.dir)
thisDir = $$eval($${currentConfig}.dir)
ex = $$eval(config.modules.$${currentModule})
subconfig = $$eval($${currentConfig}.subconfigs.$${n})
ex = $$eval(config.$${name}.dir)
pp = $$eval($${currentConfig}.features.$${k}.purpose)
pfx = $$eval($${currentConfig}.features.$${k}.section)

!isEmpty($${currentConfig}.module): \
    logn($$eval($${currentConfig}.module):)

libs = $$eval($${currentConfig}.exports.$$xport)

QMAKE_CONFIG_VERBOSE = $$eval(config.input.verbose)
QMAKE_CONFIG_CACHE_USE = $$eval(config.input.cache_use)
tdir = $$eval($${currentConfig}.testDir)
QMAKE_CONFIG_TESTS_DIR = $$absolute_path($$tdir, $$eval($${currentConfig}.dir))
QMAKE_LIBRARY_DEPS = $$eval(config.modules.global)
QMAKE_LIBRARY_DEPS += $$eval(config.modules.$$gdep) 

for (p, QMAKE_POST_CONFIGURE): \
    eval($$p)

# features/qlalr.prf
silent: $${base}.commands = @echo qlalr ${QMAKE_FILE_IN} && $$eval($${base}.commands)

# features/qt.prf
plug = $$eval(QTPLUGIN.$$nptype)
for (plug, QTPLUGIN): \
    plugin_deps += $$eval(QT_PLUGIN.$${plug}.DEPENDS)
plug_class = $$eval(QT_PLUGIN.$${plug}.CLASS_NAME)
plug_type = $$eval(QT_PLUGIN.$${plug}.TYPE)
plug_path = $$eval(QT_PLUGIN.$${plug}.PATH)

MODULE_NAME = $$eval(QT.$${QTLIB}.name)
MODULE_MODULE = $$eval(QT.$${QTLIB}.module)
MODULE_INCLUDES = $$eval(QT.$${QTLIB}.includes)
MODULE_LIBS = $$eval(QT.$${QTLIB}.libs)
MODULE_FRAMEWORKS = $$eval(QT.$${QTLIB}.frameworks)
MODULE_USES = $$eval(QT.$${QTLIB}.uses)
MODULE_CONFIG = $$eval(QT.$${QTLIB}.module_config)

MODULE_INCLUDES += \
    $$inc/$$eval(QT.$${QTLIB}.VERSION) \
    $$inc/$$eval(QT.$${QTLIB}.VERSION)/$$MODULE_NAME

MODULE_WINRT_CAPABILITIES = $$eval(QT.$${QTLIB}.winrt_capabilities)
MODULE_WINRT_CAPABILITIES_DEVICE = $$eval(QT.$${QTLIB}.winrt_capabilities_device)

for(dep, privdep): \
    rpaths += $$eval(QT.$${dep}.libs)

PATH = $$eval(IMPORTS.$${key}.path)
PLUGIN = $$eval(IMPORTS.$${key}.plugin)

PLUGIN = $$eval(IMPORTS.$${key}.plugin)
CLASSNAME = $$eval(IMPORTS.$${key}.classname)

# features/testcase.prf
!isEmpty(TESTRUN_CWD):!contains(TESTRUN_CWD, ^\\./?): \
    $${type}.commands = $$QMAKE_CD $$shell_path($$TESTRUN_CWD) && $$eval($${type}.commands)
insignificant_test: $${type}.commands = -$$eval($${type}.commands)
$${type}_first.depends = $$eval($$first(BUILDS).target)-$${type}

# features/qt_module.prf
skip = $$eval(QT.$${MODULE}.skip)
INCLUDEPATH *= $$eval(QT.$${MODULE}.includes) $$eval(QT.$${MODULE}_private.includes)

# features/qt_app.prf
launch_first.depends = $$eval($$first(BUILDS).target)-launch

# features/unix/ccache.prf
$$tool = $$ccache_prefix $$eval($$tool)

# features/dbuscommon.pri
files = $$eval($${entry}.files)
isEmpty(grp) {
    hdr_flags = $$eval(QDBUSXML2CPP_$${dbus_TYPE}_HEADER_FLAGS)
    src_flags = $$eval(QDBUSXML2CPP_$${dbus_TYPE}_SOURCE_FLAGS)
} else {
    hdr_flags = $$eval($${grp}.header_flags)
    src_flags = $$eval($${grp}.source_flags)
}
$${group}_source.depends += $$eval($${group}_header.output)

# features/data/cmake/Qt5BasicConfig.cmake.in
# NOTE: rewritten using qmake syntax
VERSION_STRING = $$eval(QT.$${MODULE}.VERSION)

