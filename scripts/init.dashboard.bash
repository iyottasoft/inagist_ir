#!/bin/bash

home="/home/balaji/prod/"

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
  ${bin_dir}/trends_generater.py ${data_dir} $2
  echo "$2" >> ${data_dir}/clients.txt
fi

if [ $# -eq 1 ]
then
  intervals=(1 2 8 16 48 144 336)
  interval_tags=(now hour 4hrs 8hrs today 3days week)
  while read line
  do
    echo $line
    mkdir -p ${data_dir}/${line}
    echo "${bin_dir}/x_dashboard_populater ${data_dir}/${line} ${line} ${time_stamp}"
    ${bin_dir}/x_dashboard_populater ${data_dir}/${line} ${line} ${time_stamp}
    echo "${bin_dir}/trends_generater.py ${data_dir} ${line}"
    ${bin_dir}/trends_generater.py ${data_dir} ${line}

    for ((i=0;i<${#intervals[@]};i++))
    do
      if [ -f ${data_dir}/${line}/trends.${time_stamp}.txt.${intervals[${i}]}.json ]
      then
        echo "cp ${data_dir}/${line}/trends.${time_stamp}.txt.${intervals[${i}]}.json /tmp/testjsons/dashboard/${line}.${interval_tags[${i}]}.json"
        cp ${data_dir}/${line}/trends.${time_stamp}.txt.${intervals[${i}]}.json /tmp/testjsons/dashboard/${line}.${interval_tags[${i}]}.json
      else
        echo "File not found: ${data_dir}/${line}/trends.${time_stamp}.txt.${intervals[${i}]}.json"
      fi
    done
    sleep 120
  done < "${data_dir}/clients.txt"
fi
