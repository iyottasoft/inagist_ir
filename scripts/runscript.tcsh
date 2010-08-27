#!/bin/tcsh


setenv ROOT_DIR /home/balaji/inagist/ir_cpp
setenv SCRIPT_DIR $ROOT_DIR/scripts
setenv CONFIG_DIR $ROOT_DIR/configs
setenv BIN_DIR $ROOT_DIR/bin
setenv DATA_DIR $ROOT_DIR/data
setenv STATIC_DATA_DIR $DATA_DIR/static_data
setenv LOG_DIR $ROOT_DIR/log

set script_file=$1
shift
tcsh $script_file $argv:q
