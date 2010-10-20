#!/bin/bash

echo "/home/balaji/trends/bin/x_keywords_extract /home/balaji/trends/data/static_data/stopwords.txt /home/balaji/trends/data/static_data/dictionary.txt $1 /home/balaji/trends/data/$1/keywords.$2"
/home/balaji/trends/bin/x_keywords_extract /home/balaji/trends/data/static_data/stopwords.txt /home/balaji/trends/data/static_data/dictionary.txt $1 /home/balaji/trends/data/$1/keywords.$2
