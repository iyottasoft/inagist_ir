#!/usr/bin/env python2.6

import sys
import os, glob
from math import log

num_docs = 0
hash_freqs = {}
latest_hash_freqs = {}
def get_keyword_freqs(kfile,latest):
  fp = open(kfile, 'r')
  lines = fp.readlines()
  for line in lines:
    line = line.strip()
    pair = line.split(" = ")
    word = pair[0]
    freqs = pair[1].split(",")
    if (0 == latest):
      if hash_freqs.has_key(word):
        hash_freqs[word][0] += int(freqs[1])
        hash_freqs[word][1] += 1
      else:
        hash_freqs[word] = []
        hash_freqs[word].append(int(freqs[1]))
        hash_freqs[word].append(1)
    else:
      if latest_hash_freqs.has_key(word):
        latest_hash_freqs[word] += 1
      else:
        latest_hash_freqs[word] = 1
  fp.close()

def calculate_idf():
  for (k,v) in hash_freqs.items():
    hash_freqs[k].append(log(num_docs/v[1]))

freqs = []
def calculate_tf_idf(kfile):
  get_keyword_freqs(kfile, 1)
  for (k, v) in latest_hash_freqs.items():
    freqs.append((v * hash_freqs[k][2], v, k))
  freqs.sort()
  length = len(freqs)
  for i in range(length):
    print freqs[i][0], " ", freqs[i][1], " ", freqs[i][2]

def print_freqs():
  for (k,v) in hash_freqs.items():
    freqs.append((v[2],k))
    #print "%s %d %f" % (k, v[1], v[2])
  freqs.sort()
  length = len(freqs)
  for i in range(length):
    print freqs[i][0], " ", freqs[i][1]

def main():
  global num_docs
  if (len(sys.argv) == 2):
    user_name = sys.argv[1]
  else:
    user_name = "worldnewsgist"
  path = '/home/balaji/inagist/ir_cpp/data/' + user_name + '/'
  kfiles = glob.glob(os.path.join(path, 'keywords.*'))
  kfiles.sort(key=lambda x: os.path.getmtime(x))

  for kfile in kfiles:
    num_docs += 1
    get_keyword_freqs(kfile,0)
  calculate_idf()
  calculate_tf_idf(kfile)
  #print_freqs()

if __name__ == "__main__":
  main()
