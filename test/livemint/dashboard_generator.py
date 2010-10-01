#!/usr/bin/env python2.6

import sys
from string import lower
import os, glob
from math import log
from time import gmtime

def get_keyword_freqs(kwfile, hash_freqs):
  fp = open(kwfile, 'r')
  lines = fp.readlines()
  for line in lines:
    line = line.strip()
    pair = line.split(" = ")
    if (len(pair) != 2):
      break
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
  global pf
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
    #f = open(outfile + '.json', 'w')
    #f.write('{ "trends": {')
    #f.write(' "trend0": {"%s":%.4f}' % (final_result_bag[0][1], final_result_bag[0][0]))
    #for i in range(1, length):
    #  f.write(', "trend%d": {"%s":%.4f}' % (i, final_result_bag[i][1], final_result_bag[i][0]))
    #f.write('} }')
    #f.close()

    for i in range(0, length):
      pf.write('<a href="http://inagist.com/trends/%s">' % (lower(final_result_bag[i][1].replace(' ', '_'))))
      #pf.write('%s : %.4f' % (final_result_bag[i][1], final_result_bag[i][0]))
      pf.write('%s' % (final_result_bag[i][1]))
      pf.write('</a><br/>')

def print_freqs(hash_freqs):
  for (k,v) in hash_freqs.items():
    freqs.append((v[2],k))
    #print "%s %d %f" % (k, v[1], v[2])
  freqs.sort()
  length = len(freqs)
  for i in range(length):
    print freqs[i][0], " ", freqs[i][1]

g_hash_archive_freqs = {}
pf = open(sys.argv[1] + '/../dashboard.html', 'w')
count = 0
def main():
  global count
  if (len(sys.argv) != 2):
    print "Usage: %s data_dir user_name" % (sys.argv[0])
    return -1

  data_dir = sys.argv[1]
  path = data_dir + '/'
  kfiles = glob.glob(os.path.join(path, 'keywords_*'))
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
  pf.write('<html>\n')
  pf.write('<head>\n')
  pf.write('<h1 align="center">LiveMint Dashboard</h1>\n')
  pf.write('</head>\n')
  pf.write('<body>\n')
  pf.write('<table>\n')
  pf.write('<tr>\n')
  for kfile in kfiles:
    count += 1
    filename = kfile.split('keywords_')
    get_keyword_freqs(kfile, current_hash_freqs)
    num_words = len(current_hash_freqs)
    outfile = path + 'trends_' + filename[1]
    calculate_tf_idf(current_hash_freqs, float(num_words), outfile)
    if (count == 1 or count > 2):
      pf.write('<td width="20%" valign="top">\n')
    pf.write('<hr/>\n<h3> trends: %s </h3>\n' % (filename[1]))
    pf.write('see <a href="%s">tweets</a><br/><br/>' % (path + 'tweets_' + filename[1]))
    generate_result_bag(outfile)
    if (count > 1):
      pf.write('</td>')
    #print "current_hash: %d" % len(current_hash_freqs)
    current_hash_freqs.clear()
    g_hash_tf_idf.clear()
  pf.write('</tr>\n')
  pf.write('</table>\n')
  pf.write('</html>\n')
  pf.write('</body>\n')
  pf.close()

  #print_freqs()
  return 0

if __name__ == "__main__":
  main()
