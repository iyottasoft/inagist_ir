#!/bin/bash

home="/home/balaji/prod"

if [ $# -lt 1 -o $# -gt 2 ]
then
  echo "Usage: $0 <dir> [client]"
  exit
fi

cd ${home}

bin_dir=${home}/bin
data_dir=${1}
log_dir=${home}/log
time_stamp=`date '+%s'`

if [ $# -eq 2 ]
then
  user_name=${2}
  mkdir -p ${data_dir}/$user_name
  #rm ${data_dir}/$user_name/trends*
  echo "${bin_dir}/dashboard_trends_generator.py ${data_dir} $user_name > ${log_dir}/dashboard_trends_generator.log"
  ${bin_dir}/dashboard_trends_generator.py ${data_dir} $user_name > ${log_dir}/dashboard_trends_generator.log
fi

if [ $# -eq 1 ]
then
  while read user_name
  do
    echo `date`
    echo $user_name
    mkdir -p ${data_dir}/${user_name}
    #rm ${data_dir}/${user_name}/trends*
    echo "${bin_dir}/dashboard_trends_generator.py ${data_dir} ${user_name} > ${log_dir}/dashboard_trends_generator.log"
    ${bin_dir}/dashboard_trends_generator.py ${data_dir} ${user_name} > ${log_dir}/dasboard_trends_generator.log
  done < "${data_dir}/clients.txt"
fi
