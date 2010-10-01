#!/bin/bash

cd /home/balaji/trends

mydate=`date '+%s'`

mkdir -p $1/$2/${mydate}
./bin/x_ping_twitter_search $1/$2/${mydate}
./bin/dashboard_generator.py $1/$2/${mydate}
