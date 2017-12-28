error(One)
message(One two)
equals(ANDROID_TARGET_ARCH)
equals(TEST, X86)
for(PKGCONFIG_LIB, $$list($$unique($$pkgvar)))
write_file($$DEPENDENCY_FILE, FILE_CONTENT)|error()
cache(QMAKE_INTERNAL_INCLUDED_FILES, add transient, MODULE_PRI_FILES)
return("\"$$replace(1, \\\\, \\\\)\"")
addAvx512Profile(avx512core, avx512cd avx512bw avx512dq avx512vl)

##########################################################
# FIXME: move to separate scope_N.pro test
contains(TEMPLATE, ".*app"):!build_pass: {
    message("test")
}

*-msvc2015|*-msvc2017 {
    message("test")
    isEmpty(WINRT_MANIFEST.minVersion): error("No UCRTVersion found in environment.")
}
##########################################################

return($$LIBRARY_NAME$$qtPlatformTargetSuffix())
isEmpty(QT.$$replace(1, -, _).name)
qtAddTargetEnv($$1, QT_TOOL.$${2}.depends, )

contains(config, prepend): infix = \${$$name:+:\$$$name}
contains(config, always_prepend): infix = :\$$$name
!contains(bundle_file, .*\.xcassets$): next()
contains(QMAKE_MAC_SDK, $${simulator.sdk})
#сontains(CMAKE_INSTALL_LIBS_DIR, ^(/usr)?/lib(64)?.*): CMAKE_USR_MOVE_WORKAROUND = $$CMAKE_INSTALL_LIBS_DIR
#contains(qt_depends, gui(-private)?): message("test")

UAP_CAPABILITIES += \
            appointments \
            blockedChatMessages \
            chat \
            contacts \
            enterpriseAuthentication \
            # internetClient is special, as it needs to be written without namespace
            #internetClient \
            musicLibrary \
            objects3D \
            phoneCall \
            picturesLibrary \
            removableStorage \
            sharedUserCertificates \
            userAccountInformation \
            videosLibrary \
            voipCall

!$$join($${build}.exclusive, _and_)_target: \
    next()

BUILDS.$$size(priority) = $$key

#!watchos:equals(TEMPLATE, app):contains(qt_depends, gui(-private)?) {
#    message("test")
#}

#*
defineTest(qtAddTargetEnv) {
    deps = $$replace($$2, -private$, _private)
    deps = $$resolve_depends(deps, "QT.", ".depends" ".run_depends")
    !isEmpty(deps) {
        libs = libs
        deppath.CONFIG = prepend
        equals(QMAKE_HOST.os, Windows) {
            libs = bins
            deppath.CONFIG = always_prepend
            deppath.name = PATH
        } else:contains(QMAKE_HOST.os, Linux|FreeBSD|OpenBSD|NetBSD|DragonFly|SunOS|HP-UX|QNX|GNU) {
            deppath.name = LD_LIBRARY_PATH
        } else:contains(QMAKE_HOST.os, Haiku) {
            deppath.name = LIBRARY_PATH
        } else:equals(QMAKE_HOST.os, Darwin) {
            qtConfig(framework): \
                deppath.name = DYLD_FRAMEWORK_PATH
            else: \
                deppath.name = DYLD_LIBRARY_PATH
        } else:equals(QMAKE_HOST.os, AIX) {
            deppath.name = LIBPATH
        } else {
            error("Operating system not supported.")
        }
        ptypes =
        for(dep, deps) {
            isEmpty(3): \
                deppath += $$shell_path($$eval(QT.$${dep}.$$libs))
            else: \
                deppath += $$system_path($$eval(QT.$${dep}.$$libs))
            ptypes += $$eval(QT.$${dep}.plugin_types)
        }
        deppath.value = $$unique(deppath)

        pluginpath.value =
        ppaths = $$[QT_INSTALL_PLUGINS/get]
        for(qplug, QT_PLUGINS): \
            contains(ptypes, $$eval(QT_PLUGIN.$${qplug}.TYPE)): \
                ppaths += $$eval(QT_PLUGIN.$${qplug}.PATH)
        ppaths = $$unique(ppaths)
        for(qplug, ppaths) {
            isEmpty(3): \
                pluginpath.value += $$shell_path($$qplug)
            else: \
                pluginpath.value += $$system_path($$qplug)
        }
        pluginpath.name = QT_PLUGIN_PATH
        pluginpath.CONFIG = prepend

        QT_TOOL_ENV += deppath pluginpath
    }
    qtAddToolEnv($$1, $$QT_TOOL_ENV, $$3)
}
*#

