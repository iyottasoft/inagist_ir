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

intervals=(2 5 13 25 73 168)
interval_tags=(now 4hrs 12hrs today 3days week)

if [ $# -eq 2 ]
then
  user_name=${2}
  mkdir -p ${data_dir}/$user_name
  rm ${data_dir}/$user_name/trends*
  ${bin_dir}/x_dashboard_populater ${data_dir}/$user_name $user_name ${time_stamp}
  ${bin_dir}/dashboard_trends_generator.py ${data_dir} $user_name
  #echo "$user_name" >> ${data_dir}/clients.txt
  #echo "${bin_dir}/dasboard_generator.py ${data_dir} $user_name"
  #${bin_dir}/dashboard_generator.py ${data_dir} $user_name

  for ((i=0;i<${#intervals[@]};i++))
  do
    if [ -f ${data_dir}/${user_name}/trends.${time_stamp}.txt.${intervals[${i}]}.json ]
    then
      echo "mv ${data_dir}/${user_name}/trends.${time_stamp}.txt.${intervals[${i}]}.json /tmp/testjsons/dashboard/${user_name}.${interval_tags[${i}]}.json"
      mv ${data_dir}/${user_name}/trends.${time_stamp}.txt.${intervals[${i}]}.json /tmp/testjsons/dashboard/${user_name}.${interval_tags[${i}]}.json
    else
      echo "File not found: ${data_dir}/${user_name}/trends.${time_stamp}.txt.${intervals[${i}]}.json"
    fi
  done
fi

if [ $# -eq 1 ]
then
  while read user_name
  do
    echo `date`
    echo $user_name
    mkdir -p ${data_dir}/${user_name}
    rm ${data_dir}/${user_name}/trends*
    echo "${bin_dir}/x_dashboard_populater ${data_dir}/${user_name} ${user_name} ${time_stamp} > ${log_dir}/x_dashboard_populater.log"
    ${bin_dir}/x_dashboard_populater ${data_dir}/${user_name} ${user_name} ${time_stamp} > ${log_dir}/x_dashboard_populater.log
    echo "${bin_dir}/dasboard_trends_generator.py ${data_dir} ${user_name} > ${log_dir}/dasboard_trends_generator.log"
    ${bin_dir}/dashboard_trends_generator.py ${data_dir} ${user_name} > ${log_dir}/dasboard_trends_generator.log

    #for ((i=0;i<${#intervals[@]};i++))
    #do
    #  if [ -f ${data_dir}/${user_name}/trends.${time_stamp}.txt.${intervals[${i}]}.json ]
    #  then
    #    echo "mv ${data_dir}/${user_name}/trends.${time_stamp}.txt.${intervals[${i}]}.json /tmp/testjsons/dashboard/${user_name}.${interval_tags[${i}]}.json"
    #    mv ${data_dir}/${user_name}/trends.${time_stamp}.txt.${intervals[${i}]}.json /tmp/testjsons/dashboard/${user_name}.${interval_tags[${i}]}.json
    #  else
    #    echo "File not found: ${data_dir}/${user_name}/trends.${time_stamp}.txt.${intervals[${i}]}.json"
    #  fi
    #done
    ##echo "${bin_dir}/dasboard_generator.py ${data_dir} ${user_name}"
    #${bin_dir}/dashboard_generator.py ${data_dir} ${user_name}
    sleep 5
  done < "${data_dir}/clients.txt"
fi
