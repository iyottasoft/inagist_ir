#!/usr/bin/env python2.6

import sys
import os, glob
from math import log
from time import gmtime

num_docs = 0
hash_freqs = {}
latest_hash_freqs = {}
def get_keyword_freqs(kwfile,latest):
  fp = open(kwfile, 'r')
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
freqs_length = 0
hash_tf_idf = {}
def calculate_tf_idf(kwfile, outfile):
  get_keyword_freqs(kwfile, 1)
  f = open(outfile, 'w')
  for (k, v) in latest_hash_freqs.items():
    freqs.append((v * hash_freqs[k][2], v, k))
  freqs.sort()
  freqs_length = len(freqs)
  for i in range(freqs_length):
    f.write('%f %d %s\n' % (freqs[i][0], freqs[i][1], freqs[i][2]))
    hash_tf_idf[freqs[i][2]] = freqs[i][0]
  f.close()

final_result_bag = []
def generate_result_bag(outfile):
  global hash_tf_idf 
  result_bag = {}
  for k,v in hash_tf_idf.items():
    tokens = k.split(" ")
    # let us consider only trends whose tokens are themselves trends
    if (len(tokens) > 1):
      tf_idf_sum = 0.0
      for token in tokens:
        if token not in hash_tf_idf:
          tf_idf_sum = -1
          break
        tf_idf_sum += hash_tf_idf[token]
      if (tf_idf_sum > -1):
        result_bag[k] = tf_idf_sum / len(tokens)

  for k,v in result_bag.items():
    final_result_bag.append((v,k))
  final_result_bag.sort()
  length = len(final_result_bag)
  if (length > 0):
    f = open(outfile + '.json', 'w')
    f.write('{ "trends": {')
    f.write(' "trend0": {"%s":%.4f}' % (final_result_bag[0][1], final_result_bag[0][0]))
    for i in range(1, length):
      f.write(', "trend%d": {"%s":%.4f}' % (i, final_result_bag[i][1], final_result_bag[i][0]))
    f.write('} }')
    f.close()

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
  if (len(sys.argv) != 3):
    print "Usage: %s data_dir user_name" % (sys.argv[0])
    return -1
  data_dir = sys.argv[1]
  user_name = sys.argv[2]
  path = data_dir + '/' + user_name + '/'
  kfiles = glob.glob(os.path.join(path, 'keywords.*'))
  kfiles.sort(key=lambda x: os.path.getmtime(x))

  for kfile in kfiles:
    num_docs += 1
    get_keyword_freqs(kfile,0)
  calculate_idf()
  if (num_docs != 0):
    kfile = kfiles[num_docs-1]
    filename = kfile.split('keywords.')
    outfile = path + 'trends.' + filename[1]
    calculate_tf_idf(kfile, outfile)
    generate_result_bag(outfile)
  #print_freqs()

if __name__ == "__main__":
  main()
