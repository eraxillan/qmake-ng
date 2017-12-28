CONFIG(static, static|shared)|prefix_build {
    isEmpty(MODULE): MODULE = $$basename(TARGET)

    mod_work_pfx = $$MODULE_QMAKE_OUTDIR/mkspecs/modules
    force_independent: \
        mod_inst_pfx = $$MODULE_QMAKE_OUTDIR/mkspecs/modules-inst
    else: \
        mod_inst_pfx = $$mod_work_pfx
    MODULE_PRI = $$mod_inst_pfx/qt_plugin_$${MODULE}.pri
    MODULE_FWD_PRI = $$mod_work_pfx/qt_plugin_$${MODULE}.pri

    !build_pass {
        qt_plugin_deps = $$QT $$QT_PRIVATE
        qt_plugin_deps = s,-private$,_private,g

        MODULE_PRI_CONT = \
            "QT_PLUGIN.$${MODULE}.TYPE = $$PLUGIN_TYPE" \
            "QT_PLUGIN.$${MODULE}.EXTENDS =$$join(PLUGIN_EXTENDS, " ", " ")" \
            "QT_PLUGIN.$${MODULE}.DEPENDS = $$qt_plugin_deps" \
            "QT_PLUGIN.$${MODULE}.CLASS_NAME = $$PLUGIN_CLASS_NAME" \
            "QT_PLUGINS += $$MODULE"
        write_file($$MODULE_PRI, MODULE_PRI_CONT)|error()
        MODULE_PRI_FILES = $$MODULE_PRI

        force_independent {

            # Create a forwarding module .pri file
            MODULE_FWD_PRI_CONT = \
                "QT_PLUGIN.$${MODULE}.PATH = $$MODULE_BASE_OUTDIR/plugins" \
                "include($$MODULE_PRI)"
            write_file($$MODULE_FWD_PRI, MODULE_FWD_PRI_CONT)|error()
            touch($$MODULE_FWD_PRI, $$MODULE_PRI)
            MODULE_PRI_FILES += $$MODULE_FWD_PRI

        }

        # Then, inject the new module into the current cache state
#        !contains(QMAKE_INTERNAL_INCLUDED_FILES, $$MODULE_FWD_PRI): \ # before the actual include()!
#            cache(QMAKE_INTERNAL_INCLUDED_FILES, add transient, MODULE_PRI_FILES)
        include($$MODULE_FWD_PRI)
        for(var, $$list(TYPE EXTENDS CLASS_NAME PATH)): \
            defined(QT_PLUGIN.$${MODULE}.$$var, var): \
                cache(QT_PLUGIN.$${MODULE}.$$var, transient)
        cache(QT_PLUGINS, transient)
    }

    CONFIG(static, static|shared) {
        pritarget.path = $$[QT_HOST_DATA]/mkspecs/modules
        pritarget.files = $$MODULE_PRI
        INSTALLS += pritarget
    }
}

