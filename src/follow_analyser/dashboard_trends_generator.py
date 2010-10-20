#!/usr/bin/env python2.6

import sys
import os, glob
from string import lower
from math import log
from time import gmtime
from datetime import datetime

def get_keyword_freqs(kwfile, hash_freqs):
  fp = open(kwfile, 'r')
  lines = fp.readlines()
  for line in lines:
    line = line.strip()
    pair = line.split(" = ")
    word = pair[0]
    freqs = pair[1].split(",")
    if hash_freqs.has_key(word):
      hash_freqs[word][0] += int(freqs[1]) # number of times word appeared in cyle
      hash_freqs[word][1] += 1 # number of cylces in which this word appeared
    else:
      hash_freqs[word] = []
      hash_freqs[word].append(int(freqs[1]))
      hash_freqs[word].append(1)
  fp.close()
  return len(lines)

# g_hash_archieve_freqs has
# word,
# number of times word appears in file (which is hourly keywords file)
# number of documents (hours) in which this document occurs
# idf value of the word
# tf * idf value of the word

# TODO - treat idf values with a time decay component
def calculate_idf(num_docs):
  global g_hash_idf
  for (k,v) in g_hash_idf.items():
    g_hash_idf[k].append(log(num_docs/v[1])) # idf

def calculate_tf_idf(hash_freqs, num_words, outfile):
  global g_hash_tf_idf
  global g_dashboard_fp
  freq_list_for_sorting = []

  for (k, v) in hash_freqs.items():
    tf = float(v[0])/num_words
    tf_idf = tf * g_hash_idf[k][2] 
    freq_list_for_sorting.append((tf_idf, k))
    g_hash_tf_idf[k] = (v[0], tf_idf) # store in hash - tf_idf can be read directly from list too
  freq_list_for_sorting.sort()
  freq_list_for_sorting.reverse()
  
  #trends_fp = open(outfile, 'w')
  #trends_json_fp = open(outfile + '.json', 'w')

  #trends_json_fp.write('{ "trends": {')
  g_dashboard_fp.write('<table border=1 valign="top">\n')
  g_dashboard_fp.write('<tr>')
  g_dashboard_fp.write('<td align="left">trends n = %.0f </td>' % num_words)
  g_dashboard_fp.write('<td align="center">t</td>')
  g_dashboard_fp.write('<td align="center">f</td> <td align="center">F</td> <td align="center">m</td> <td align="center"> idf </td> <td align="center"> f/n * idf </td>\n')
  g_dashboard_fp.write('</tr>')
  for i in range(len(freq_list_for_sorting)):

    # trends
    #trends_fp.write('%f | %d | %s\n' % (freqs[i][0], freqs[i][1], freqs[i][2]))

    # trends json
    #trends_json_fp.write(', "trend%d": {"%s":%.4f}' % (i, final_result_bag[i][1], final_result_bag[i][0]))

    # dashboard
    g_dashboard_fp.write('<tr>\n')
    word = freq_list_for_sorting[i][1]
    g_dashboard_fp.write('<td><a href="http://inagist.com/trends/%s">%s</a></td>' % (lower(word.replace(' ', '_')), word))
    g_dashboard_fp.write('<td>&nbsp;<a href="http://search.twitter.com/search?q=%s">t</a>&nbsp;</td>\n' % (lower(word.replace(' ', '+'))))
    numbers = g_hash_idf[word]
    g_dashboard_fp.write('<td align="right"> %d </td> <td align="right"> %d </td> <td align="right"> %d </td> <td align="right"> %.4f </td> <td align="right"> %.4f </td>\n' % (g_hash_tf_idf[word][0], numbers[0], numbers[1], numbers[2], g_hash_tf_idf[word][1]))
    g_dashboard_fp.write('</tr>\n')

  g_dashboard_fp.write('</table>\n')
  #trends_json_fp.write('} }')

  #trends_fp.close()
  #trends_json_fp.close()

g_hash_idf = {}
g_hash_tf_idf = {}
g_dashboard_fp = open('/tmp/testjsons/dashboard/' + sys.argv[2] + '.html', 'w')
def main():
  if (len(sys.argv) != 3):
    print "Usage: %s data_dir user_name" % (sys.argv[0])
    return -1

  data_dir = sys.argv[1]
  user_name = sys.argv[2]

  interval_list = []
  interval_hash = {}
  interval_file = data_dir+ '/' + 'intervals.txt'
  fp = open(interval_file, 'r')
  lines = fp.readlines()
  for line in lines:
    line = line.strip()
    pair = line.split(" = ")
    interval_list.append((int(pair[0]),pair[1]))
    interval_hash[int(pair[0])] = pair[1]
  fp.close()

  interval_len = len(interval_list)
  if (interval_len <= 0):
    print 'no interval data in the interval file %s' % (interval_file)
    return -1

  max_interval = interval_list[interval_len - 1][0]

  path = data_dir + '/' + user_name + '/'
  file_name_pattern = 'keywords_by_' + user_name + '_followers.*'
  kfiles = glob.glob(os.path.join(path, file_name_pattern))
  kfiles.sort(key=lambda x: os.path.getmtime(x))
  kfiles.reverse()

  num_docs = 0
  for kfile in kfiles:
    num_docs += 1
    get_keyword_freqs(kfile, g_hash_idf)
  calculate_idf(num_docs)

  if (num_docs == 0):
    print 'no documents found in this cylce'
    return 0

  g_dashboard_fp.write('<html>\n')
  g_dashboard_fp.write('<head>\n')
  g_dashboard_fp.write('<h1 align="center">%s Dashboard</h1>\n' % (sys.argv[2]))
  g_dashboard_fp.write('</head>\n')
  g_dashboard_fp.write('<body>\n')
  g_dashboard_fp.write('<p>Number of cycles: %d</p>' % (num_docs))
  g_dashboard_fp.write('<table border="1">\n')

  g_dashboard_fp.write('<tr>\n')
  for interval in interval_list: # trend intervals + 1
    g_dashboard_fp.write('<td align="center">\n')
    g_dashboard_fp.write(interval[1])
    g_dashboard_fp.write('</td>\n')
  g_dashboard_fp.write('</tr>\n')

  g_dashboard_fp.write('<tr>\n')
  cycle_hash_freqs = {}
  flag = 0
  outfile = ''
  filename = []
  trends_per_cycle = {}
  for i in range(0, num_docs):
    if (i+1 > max_interval):
      break
    kfile = kfiles[i]
    if (flag == 0):
      split_word = 'keywords_by_' + user_name + '_followers.'
      filename = kfile.split(split_word)
      flag = 1
    num_words_file = get_keyword_freqs(kfile, cycle_hash_freqs)
    trends_per_cycle[i] = num_words_file
    if (num_words_file <= 0):
      print '%s had no trends. this is in %d cycle' % (kfile, i+1)
    if (interval_hash.has_key(i+1) or (i+1 == num_docs)):
      num_words = len(cycle_hash_freqs) # num words in current cycle
      if (flag == 1):
        outfile = path + 'trends.' + filename[1] + '.' + str(i+1)
      g_dashboard_fp.write('<td valign="top" width="%d">\n' % (100/interval_len))
      calculate_tf_idf(cycle_hash_freqs, float(num_words), outfile)
      g_dashboard_fp.write('</td>\n')
      g_hash_tf_idf.clear()

  g_dashboard_fp.write('</tr>\n')

  g_dashboard_fp.write('<tr>')
  g_dashboard_fp.write('<td valign="top">')
  for interval in interval_list:
    i = interval[0]
    for j in range(0,i):
      if (j >= num_docs):
        break
      file_time = os.path.getmtime(kfiles[j])
      g_dashboard_fp.write('%s (%d trends)' % (datetime.fromtimestamp(file_time), trends_per_cycle[j]))
      if (i > 1):
        g_dashboard_fp.write('<br/>')
    if (j != num_docs):
      g_dashboard_fp.write('</td><td valign="top">')
    else:
      g_dashboard_fp.write('</td>')
  g_dashboard_fp.write('</tr>')

  g_dashboard_fp.write('</table>\n')
  g_dashboard_fp.write('</html>\n')
  g_dashboard_fp.write('</body>\n')
  g_dashboard_fp.close()

  cycle_hash_freqs.clear()
  g_hash_idf.clear()
  return 0

if __name__ == "__main__":
  main()
