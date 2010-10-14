#!/bin/bash

home="/home/balaji/inagist/ir_cpp"

if [ $# -lt 1 -o $# -gt 2 ]
then
  echo "Usage: $0 <dir> [client]"
  exit
fi

cd ${home}

bin_dir=${home}/bin
data_dir=${1}
time_stamp=`date '+%s'`

if [ $# -eq 2 ]
then
  mkdir -p ${data_dir}/$2
  ${bin_dir}/x_dashboard_populater ${data_dir}/$2 $2 ${time_stamp}
  ${bin_dir}/trends_generater.py ${data_dir}/$2 $2
  echo "$2" >> ${data_dir}/clients.txt
fi

if [ $# -eq 1 ]
then
  while read line
  do
    mkdir -p ${data_dir}/$line
    ${bin_dir}/x_dashboard_populater ${data_dir}/$line $line ${mydate}
    ${bin_dir}/trends_generater.py ${data_dir}/$line $line
  done < "${data_dir}/clients.txt"
fi
