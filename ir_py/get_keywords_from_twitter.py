#!/usr/bin/env python2.6

import sys
import commands

commands.getoutput("rm ./data/named_entities_set.txt")
commands.getoutput("./bin/t_named_entities_from_twitter")
named_entities_file=open("./data/named_entities_set.txt", 'r')

named_entities_hash = {}

lines = named_entities_file.readlines()
for line in lines:
  if len(line) == 0:
    break;
  line = line.strip()
  pair = line.split(" = ")
  named_entities_hash[pair[1]] = pair[0]

named_entities_file.close()

freqs = [(v,k) for k,v in named_entities_hash.items()]
freqs.sort()
freqs.reverse()

count = 0
length = len(freqs)
for i in range(length):
  if (count == 10):
    break;
  print freqs[i][0], " ", freqs[i][1] 
  count += 1
