#!/bin/bash

user_names=(btgist balajinix jebui tantricninja netroy kotharipankaj chjeeves tuxtoti scitechgist cricketgist nygist winegist airportsgist geekgist sporegist fashiongist pharmagist motorgist astrogist healthgist cancergist picgist bayareagist airlinesgist travelgist worldnewsgist soccergist worldbizgist booksgist foodgist londongist indonesiagist indiagist hitechgist designgist musicgist francegist ngogist testa19 malaygist bollygist hollygist envirogist sportsinagist)

for user_name in ${user_names[@]}
do
  echo "/home/balaji/ir_cpp/bin/t_twitter_api 3 $user_name /home/balaji/ir_cpp/data/text_class/tweetsource/handles/$user_name.txt"
  /home/balaji/ir_cpp/bin/t_twitter_api 3 $user_name /home/balaji/ir_cpp/data/text_class/tweetsource/handles/$user_name.txt
done
