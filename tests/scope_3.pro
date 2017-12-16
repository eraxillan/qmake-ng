for(ever) {
    for(PKGCONFIG_LIB, $$list($$unique($$pkgvar))) {
        # don't proceed if the .pro asks for a package we don't have!
        !packagesExist($$PKGCONFIG_LIB): error("$$PKGCONFIG_LIB development package not found")

        PKGCONFIG_CFLAGS = $$system($$PKG_CONFIG --cflags $$PKGCONFIG_LIB)
        PKGCONFIG_INCLUDEPATH = $$find(PKGCONFIG_CFLAGS, ^-I.*)
        PKGCONFIG_INCLUDEPATH ~= s/^-I(.*)/\\1/g

        PKGCONFIG_DEFINES = $$find(PKGCONFIG_CFLAGS, ^-D.*)
        PKGCONFIG_DEFINES ~= s/^-D(.*)/\\1/g
        PKGCONFIG_CFLAGS ~= s/^-[ID].*//g

        $$libvar += $$system($$PKG_CONFIG --libs $$PKGCONFIG_LIB)
    }
}

