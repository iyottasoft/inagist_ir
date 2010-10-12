#!/bin/bash

home="/home/balaji/code"

if [ $# -lt 1 -o $# -gt 2 ] 
then
  echo "Usage: $0 <dir> [client]"
  exit
fi

cd ${home}

dir=${1}
mydate=`date '+%s'`

if [ $# -eq 2 ]
then
  mkdir -p ${dir}/$2/${mydate}
  ${home}/bin/x_ping_twitter_search ${dir}/$2/${mydate} $2
  ${home}/test/dashboard/dashboard_generator.py ${dir}/$2/${mydate} $2
  echo "$2" >> ${dir}/clients.txt
fi

if [ $# -eq 1 ]
then
  while read line
  do
    mkdir -p ${dir}/$line/${mydate}
    ${home}/bin/x_ping_twitter_search ${dir}/$line/${mydate} $line
    ${home}/bin/dashboard_generator.py ${dir}/$line/${mydate} $line
  done < "${dir}/clients.txt"
fi
