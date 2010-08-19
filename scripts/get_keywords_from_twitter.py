#!/usr/bin/env python2.6

import sys
import commands

commands.getoutput("rm ./data/keywords_set.txt")
commands.getoutput("./bin/t_keywords_from_twitter")
keywords_file=open("./data/keywords_set.txt", 'r')

keywords_hash = {}

lines = keywords_file.readlines()
for line in lines:
  if len(line) == 0:
    break;
  line = line.strip()
  pair = line.split(" = ")
  keywords_hash[pair[1]] = pair[0]

keywords_file.close()

freqs = [(v,k) for k,v in keywords_hash.items()]
freqs.sort()
freqs.reverse()

count = 0
length = len(freqs)
for i in range(length):
  if (count == 10):
    break;
  print freqs[i][0], " ", freqs[i][1] 
  count += 1
