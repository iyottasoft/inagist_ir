#!/usr/bin/env python2.6

import sys
import commands

commands.getoutput("rm ./data/named_entities_set.txt")

if (len(sys.argv) == 1):
  commands.getoutput("./bin/t_named_entities_from_inagist")
else:
  #commands.getoutput("./bin/t_named_entities_from_inagist http://inagist.com/api/v1/get_top_tweets?userid=" + sys.argv[1] + "&limit=5&ham=24")
  commands.getoutput("./bin/t_named_entities_from_inagist http://inagist.com/api/v1/get_archived_tweets?userid=" + sys.argv[1] + "&count=0&limit=10000")

named_entities_file=open("./data/named_entities_set.txt", 'r')

named_entities_hash = {}

lines = named_entities_file.readlines()
for line in lines:
  if (len(line)) < 6:
    continue
  line = line.strip()
  pair = line.split(" = ")
  count = int(pair[0])
  named_entities_hash[pair[1]] = count

named_entities_file.close()

freqs = [(v,k) for k,v in named_entities_hash.items()]
freqs.sort()
freqs.reverse()

length = len(freqs)
for i in range(length):
  #if freqs[i][0] > 1:
  print freqs[i][0], " ", freqs[i][1] 
