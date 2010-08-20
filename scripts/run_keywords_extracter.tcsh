#!/usr/bin/tcsh

set ROOT_DIR = "/home/balaji/inagist/ir_cpp"
set BIN_DIR = "$ROOT_DIR/bin"
set STATIC_DATA_DIR = "$ROOT_DIR/data/static_data"

if ($#argv != 1) then
  set user_name = "worldnewsgist"
else
  set user_name = $argv[1]
endif

set DATA_DIR = "$ROOT_DIR/data/$user_name"
mkdir -p $DATA_DIR

set epoch_time = `date +%s`
echo "$BIN_DIR/x_keywords_extract $STATIC_DATA_DIR/stopwords.txt $STATIC_DATA_DIR/dictionary.txt $user_name $DATA_DIR/keywords.$epoch_time"
$BIN_DIR/x_keywords_extract $STATIC_DATA_DIR/stopwords.txt $STATIC_DATA_DIR/dictionary.txt $user_name $DATA_DIR/keywords.$epoch_time

