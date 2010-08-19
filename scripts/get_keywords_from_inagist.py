#!/usr/bin/env python2.6

import sys
import commands

commands.getoutput("rm ./data/keywords_set.txt")

if (len(sys.argv) == 1):
  commands.getoutput("./bin/t_keywords_from_inagist")
else:
  #commands.getoutput("./bin/t_keywords_from_inagist http://inagist.com/api/v1/get_top_tweets?userid=" + sys.argv[1] + "&limit=5&ham=24")
  commands.getoutput("./bin/t_keywords_from_inagist http://inagist.com/api/v1/get_archived_tweets?userid=" + sys.argv[1] + "&count=0&limit=10000")

keywords_file=open("./data/keywords_set.txt", 'r')

keywords_hash = {}

lines = keywords_file.readlines()
for line in lines:
  if (len(line)) < 6:
    continue
  line = line.strip()
  pair = line.split(" = ")
  count = int(pair[0])
  keywords_hash[pair[1]] = count

keywords_file.close()

freqs = [(v,k) for k,v in keywords_hash.items()]
freqs.sort()
freqs.reverse()

length = len(freqs)
for i in range(length):
  #if freqs[i][0] > 1:
  print freqs[i][0], " ", freqs[i][1] 
