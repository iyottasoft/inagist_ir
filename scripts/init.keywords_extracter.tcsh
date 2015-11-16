#!/usr/bin/tcsh

if ($#argv != 1) then
  set user_name = "worldnewsgist"
else
  set user_name = $argv[1]
endif

mkdir -p $DATA_DIR/$user_name

set epoch_time = `date +%s`
echo "$BIN_DIR/x_keywords_extract $STATIC_DATA_DIR/stopwords.txt $STATIC_DATA_DIR/dictionary.txt $user_name $DATA_DIR/$user_name/keywords.$epoch_time"
$BIN_DIR/x_keywords_extract $STATIC_DATA_DIR/stopwords.txt $STATIC_DATA_DIR/dictionary.txt $user_name $DATA_DIR/$user_name/keywords.$epoch_time

