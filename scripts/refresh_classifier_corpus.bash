#!/bin/bash

if [ $# -lt 1 ]
then
  echo "Usage: $0 <dir>"
  exit
fi

HOME_DIR="/home/balaji/ir_cpp/data"
DATE=`date '+%Y_%m_%d.%s'`

echo "mkdir -p ${HOME_DIR}/$1/backup/${DATE}"
echo "cp ${HOME_DIR}/$1/training/* ${HOME_DIR}/$1/backup/${DATE}/"
echo "cp ${HOME_DIR}/$1/tweetsource/corpus/* ${HOME_DIR}/$1/training/"
