NDK_TOOLCHAIN_PREFIX = $$(ANDROID_NDK_TOOLCHAIN_PREFIX)
isEmpty(NDK_TOOLCHAIN_PREFIX) {
    message("hello")
    equals(ANDROID_TARGET_ARCH, x86): NDK_TOOLCHAIN_PREFIX = x86
#    else: equals(ANDROID_TARGET_ARCH, x86_64): NDK_TOOLCHAIN_PREFIX = x86_64
#    else: equals(ANDROID_TARGET_ARCH, mips): NDK_TOOLCHAIN_PREFIX = mipsel-linux-android
#    else: equals(ANDROID_TARGET_ARCH, mips64): NDK_TOOLCHAIN_PREFIX = mips64el-linux-android
#    else: equals(ANDROID_TARGET_ARCH, arm64-v8a): NDK_TOOLCHAIN_PREFIX = aarch64-linux-android
    else: NDK_TOOLCHAIN_PREFIX = arm-linux-androideabi
    else: NDK_TOOLCHAIN_PREFIX = arm
}

