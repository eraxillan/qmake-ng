$${group}_header.output  = $$QMAKE_MOD_REPC${QMAKE_FILE_BASE}_$${repc_type}.h

{
    GROUP = $$upper($$group)
    input_list = $${GROUP}_LIST

    $${group}_header.output  = $$QMAKE_MOD_REPC${QMAKE_FILE_BASE}_$${repc_type}.h
#    $${group}_header.commands = $$QMAKE_REPC $$repc_option $$REPC_INCLUDEPATH ${QMAKE_FILE_NAME} ${QMAKE_FILE_OUT}
#    $${group}_header.depends = ${QMAKE_FILE_NAME} $$QT_TOOL.repc.binary
#    $${group}_header.variable_out = $${GROUP}_HEADERS
#    $${group}_header.input = $$input_list

#    $${group}_moc.commands = $$moc_header.commands $$REPC_INCLUDEPATH
#    $${group}_moc.output = $$moc_header.output
#    $${group}_moc.input = $${GROUP}_HEADERS
#    $${group}_moc.variable_out = GENERATED_SOURCES
#    $${group}_moc.name = $$moc_header.name

#    QMAKE_EXTRA_COMPILERS += $${group}_header $${group}_moc
}
