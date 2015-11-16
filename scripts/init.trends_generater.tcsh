#!/bin/tcsh

echo "$BIN_DIR/trends_generater.py $DATA_DIR $argv[1]"
$BIN_DIR/trends_generater.py $DATA_DIR $argv[1]
if ($status != 0) then
  echo "ERROR: $BIN_DIR/trends_generater.py failed"
  exit(-1)
endif

exit(0)
