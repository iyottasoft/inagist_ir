#!/bin/bash

user_names=(btgist balajinix jebui balajiworld netroy chjeeves tuxtoti scitechgist cricketgist nygist winegist airportsgist geekgist sporegist fashiongist pharmagist motorgist astrogist healthgist cancergist picgist bayareagist airlinesgist travelgist worldnewsgist soccergist worldbizgist booksgist foodgist londongist indonesiagist indiagist hitechgist designgist musicgist francegist ngogist testa19 malaygist bollygist hollygist envirogist sportsinagist)

home_folder="/home/balaji/ir_cpp"

i=0
config_file=$home_folder/configs/text_classifier.config
echo "testdata=$home_folder/data/text_class/test/classes_freq.txt" > $config_file
for user_name in ${user_names[@]}
do
  echo "class.$i=$user_name" >> $config_file 
  echo "trainingdata.$i=$home_folder/data/text_class/training/$user_name.txt" >> $config_file
  echo "handles.$i=$home_folder/data/text_class/tweetsource/handles/$user_name.txt" >> $config_file
  echo "corpus.$i=$home_folder/data/text_class/tweetsource/corpus/$user_name.txt" >> $config_file
  echo "tweets.$i=$home_folder/data/text_class/tweetsource/tweets/$user_name.txt" >> $config_file
  i=`expr $i + 1`
done

for user_name in ${user_names[@]}
do
  echo "$home_folder/bin/t_twitter_api 3 $user_name $home_folder/data/text_class/tweetsource/handles/$user_name.txt"
  /home/balaji/ir_cpp/bin/t_twitter_api 3 $user_name /home/balaji/ir_cpp/data/text_class/tweetsource/handles/$user_name.txt
done
