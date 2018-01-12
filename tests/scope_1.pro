NDK_TOOLCHAIN_PREFIX = $$(ANDROID_NDK_TOOLCHAIN_PREFIX)

isEmpty(NDK_TOOLCHAIN_PREFIX) {
    message("hello")
    equals(ANDROID_TARGET_ARCH, x86): NDK_TOOLCHAIN_PREFIX = x86
    else: NDK_TOOLCHAIN_PREFIX = arm-linux-androideabi
    else: NDK_TOOLCHAIN_PREFIX = arm
}

contains(TEMPLATE, ".*app"):!build_pass: {
    message("test")
}

*-msvc2015|*-msvc2017 {
    message("test")
    isEmpty(WINRT_MANIFEST.minVersion): error("No UCRTVersion found in environment.")
}

!watchos:equals(TEMPLATE, app):contains(qt_depends, gui(-private)?) {
    message("test")
}

!$$join($${build}.exclusive, _and_)_target: \
    next()

