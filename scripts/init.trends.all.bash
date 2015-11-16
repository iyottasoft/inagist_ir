#!/bin/bash

#user_names=(btgist balajinix jebui tantricninja netroy kotharipankaj chjeeves tuxtoti scitechgist cricketgist nygist winegist airportsgist geekgist sporegist fashiongist pharmagist motorgist astrogist healthgist cancergist picgist bayareagist airlinesgist travelgist worldnewsgist soccergist worldbizgist booksgist foodgist londongist indonesiagist indiagist hitechgist designgist musicgist francegist ngogist testa19 malaygist bollygist hollygist envirogist sportsinagist)

user_names=(tantricninja)

epoch_time=`date +%s`

for user_name in ${user_names[@]}
do
  mkdir -p /home/balaji/trends/data/$user_name
  #rm -fr /home/balaji/trends/data/$user_name/trends.*
  /home/balaji/trends/scripts/init.keywords_extracter.bash $user_name $epoch_time &> /home/balaji/trends/log/keywords_extracter.log
done

intervals=(1 2 8 16 48 144 336)
interval_tags=(now hour 4hrs 8hrs today 3days week)
for user_name in ${user_names[@]}
do
  /home/balaji/trends/scripts/init.trends_generater.bash $user_name &> /home/balaji/trends/log/trends_generater.log

  for ((i=0;i<${#intervals[@]};i++))
  do
    if [ -f /home/balaji/trends/data/$user_name/trends.$epoch_time.${intervals[${i}]}.json ]
    then
      cp /home/balaji/trends/data/$user_name/trends.$epoch_time.${intervals[${i}]}.json /tmp/testjsons/$user_name.${interval_tags[${i}]}.json
    else 
      echo "File not found: /home/balaji/trends/data/$user_name/trends.$epoch_time.${intervals[${i}]}.json"
    fi
  done
done
