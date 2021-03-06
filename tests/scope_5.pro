for (cp, COPIES) {
    isEmpty($${cp}.files): next()
    pfx = copy_$${cp}
    notdir = false
    dir = false
    for (f, $${cp}.files) {
        fil = $$absolute_path($$f, $$_PRO_FILE_PWD_)
        tfiles = $$files($$fil/*)
        isEmpty(tfiles): \
            notdir = true
        else: \
            dir = true
        $${pfx}.files += $$fil
    }
    $$dir:$$notdir: \
        error("COPIES entry $$cp lists both files and directories.")
    path = $$eval($${cp}.path)
    isEmpty(path): error("COPIES entry $$cp defines no .path")
    base = $$eval($${cp}.base)
#
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
#
    $${pfx}.input = $${pfx}.files
    $${pfx}.commands = $(QINSTALL) ${QMAKE_FILE_IN} ${QMAKE_FILE_OUT}
    $${pfx}.name = COPY ${QMAKE_FILE_IN}
    $${pfx}.CONFIG = no_link no_clean target_predeps
    QMAKE_EXTRA_COMPILERS += $${pfx}
}
