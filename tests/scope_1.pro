NDK_TOOLCHAIN_PREFIX = $$(ANDROID_NDK_TOOLCHAIN_PREFIX)
isEmpty(NDK_TOOLCHAIN_PREFIX) {
    message("hello")
    equals(ANDROID_TARGET_ARCH, x86): NDK_TOOLCHAIN_PREFIX = x86
#    else: NDK_TOOLCHAIN_PREFIX = arm-linux-androideabi
#    else: NDK_TOOLCHAIN_PREFIX = arm
}

