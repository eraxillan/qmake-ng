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

# FIXME: failed
#BUILDS.$$size(priority) = $$key
