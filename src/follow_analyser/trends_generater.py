#!/usr/bin/env python2.6

import sys
import os, glob
from math import log
from time import gmtime

def get_keyword_freqs(kwfile, hash_freqs):
  fp = open(kwfile, 'r')
  lines = fp.readlines()
  for line in lines:
    line = line.strip()
    pair = line.split(" = ")
    word = pair[0]
    freqs = pair[1].split(",")
    if hash_freqs.has_key(word):
      hash_freqs[word][0] += int(freqs[1])
      hash_freqs[word][1] += 1
    else:
      hash_freqs[word] = []
      hash_freqs[word].append(int(freqs[1]))
      hash_freqs[word].append(1)
  #print "hash %d" % (len(hash_freqs))
  fp.close()

# TODO - treat idf values with a time decay component
def calculate_idf(num_docs):
  global g_hash_archive_freqs
  for (k,v) in g_hash_archive_freqs.items():
    g_hash_archive_freqs[k].append(log(num_docs/v[1]))

def print_idf():
  for (k,v) in g_hash_archive_freqs.items():
    print "%s %d %d %f" % (k, v[0], v[1], v[2])

freqs = []
freqs_length = 0
g_hash_tf_idf = {}
def calculate_tf_idf(hash_freqs, num_words, outfile):
  f = open(outfile, 'w')
  for (k, v) in hash_freqs.items():
    freqs.append(((float(v[0])/num_words) * g_hash_archive_freqs[k][2], v[0], k))
  freqs.sort()
  freqs_length = len(freqs)
  for i in range(freqs_length):
    f.write('%f %d %s\n' % (freqs[i][0], freqs[i][1], freqs[i][2]))
    g_hash_tf_idf[freqs[i][2]] = freqs[i][0]
  f.close()

def generate_result_bag(outfile):
  global g_hash_tf_idf 
  #result_bag = {}
  #for k,v in g_hash_tf_idf.items():
  #  tokens = k.split(" ")
  #  # let us consider only trends whose tokens are themselves trends
  #  if (len(tokens) > 1):
  #    tf_idf_sum = 0.0
  #    for token in tokens:
  #      if token not in g_hash_tf_idf:
  #        tf_idf_sum = -1
  #        break
  #      tf_idf_sum += g_hash_tf_idf[token]
  #    if (tf_idf_sum > -1):
  #      result_bag[k] = tf_idf_sum / len(tokens)

  final_result_bag = []
  #for k,v in result_bag.items():
  for k,v in g_hash_tf_idf.items():
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

def print_freqs(hash_freqs):
  for (k,v) in hash_freqs.items():
    freqs.append((v[2],k))
    #print "%s %d %f" % (k, v[1], v[2])
  freqs.sort()
  length = len(freqs)
  for i in range(length):
    print freqs[i][0], " ", freqs[i][1]

g_hash_archive_freqs = {}
def main():
  if (len(sys.argv) != 3):
    print "Usage: %s data_dir user_name" % (sys.argv[0])
    return -1

  data_dir = sys.argv[1]
  user_name = sys.argv[2]
  path = data_dir + '/' + user_name + '/'
  file_name_pattern = 'keywords_by_' + user_name + '_followers.*'
  print path
  print file_name_pattern
  kfiles = glob.glob(os.path.join(path, file_name_pattern))
  print kfiles
  kfiles.sort(key=lambda x: os.path.getmtime(x))

  num_docs = 0
  for kfile in kfiles:
    num_docs += 1
    get_keyword_freqs(kfile, g_hash_archive_freqs)
  calculate_idf(num_docs)

  if (num_docs == 0):
    return 0

  current_hash_freqs = {}
  flag = 0
  for i in [2, 3, 9, 17, 49, 145, 337]: # trend intervals + 1
    for j in range(1, i):
      if (j > num_docs):
        break;
      kfile = kfiles[num_docs-j]
      if (flag == 0):
        split_word = 'keywords_by_' + user_name + '_followers.'
        filename = kfile.split(split_word)
        flag = 1
      get_keyword_freqs(kfile, current_hash_freqs)
    num_words = len(current_hash_freqs)
    outfile = path + 'trends.' + filename[1] + '.' + str(i-1)
    calculate_tf_idf(current_hash_freqs, float(num_words), outfile)
    generate_result_bag(outfile)
    #print "current_hash: %d" % len(current_hash_freqs)
    current_hash_freqs.clear()
    g_hash_tf_idf.clear()

  #print_freqs()
  return 0

if __name__ == "__main__":
  main()
