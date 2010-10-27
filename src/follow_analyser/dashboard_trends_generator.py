#!/usr/bin/env python2.6

import sys
import os, glob
from string import lower
from math import log
from time import gmtime
from datetime import datetime

def get_keyword_freqs(kwfile, hash_freqs):
  global interval_hash
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

def calculate_tf_idf_and_print(hash_freqs, num_distinct_words, num_words, keywords_tweeters_map, outfile):
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
  g_dashboard_fp.write('<td align="left">trends n = %.0f </td>' % num_distinct_words)
  g_dashboard_fp.write('<td align="center">t</td><td align="center">trended by</td>')
  g_dashboard_fp.write('<td align="center">f</td> <td align="center">F</td> <td align="center">m</td> <td align="center">M</td><td align="center"> idf = log(N/M)</td> <td align="center"> f/&#931;n * idf (&#931;n = %d)</td>\n' % num_words)
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
    g_dashboard_fp.write('<td>')
    if (keywords_tweeters_map.has_key(word)):
      for (k,v) in keywords_tweeters_map[word].items():
        g_dashboard_fp.write('<a href="http://twitter.com/%s">%s</a> ' % (k,k))
        if (v > 1):
          g_dashboard_fp.write('(%d) ' % (v))
    g_dashboard_fp.write('</td>')
    numbers = g_hash_idf[word]
    g_dashboard_fp.write('<td align="right"> %d </td> <td align="right"> %d </td> <td align="right"> %d </td> <td align="right"> %d </td> <td align="right"> %.4f </td> <td align="right"> %.4f </td>\n' % (g_hash_tf_idf[word][0], numbers[0], hash_freqs[word][1], numbers[1], numbers[2], g_hash_tf_idf[word][1]))
    g_dashboard_fp.write('</tr>\n')

  g_dashboard_fp.write('</table>\n')
  #trends_json_fp.write('} }')

  #trends_fp.close()
  #trends_json_fp.close()

def generate_map(map_file_name, map):
  if (os.path.isfile(map_file_name)):
    fp = open(map_file_name, 'r')
    lines = fp.readlines()
    for line in lines:
      line = line.strip()
      pair = line.split(" = ")
      key = pair[0]
      value = pair[1].split(", ")
      if (map.has_key(key)):
        for v in value:
          if (map[key].has_key(v)):
            map[key][v] += 1
          else:
            map[key][v] = 1
      else: 
        map[key] = {}
        for v in value:
          map[key][v] = 1

g_hash_idf = {}
g_hash_tf_idf = {}
interval_hash = {}
g_num_words = 0
def main():
  global g_dashboard_fp
  global g_num_words
  if (len(sys.argv) != 3):
    print "Usage: %s data_dir user_name" % (sys.argv[0])
    return -1

  data_dir = sys.argv[1]
  user_name = sys.argv[2]

  interval_list = []
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
    g_num_words += get_keyword_freqs(kfile, g_hash_idf)
  calculate_idf(num_docs)

  if (num_docs == 0):
    print 'no documents found in this cylce'
    return 0

  g_dashboard_fp = open('/tmp/testjsons/dashboard/' + user_name + '.html', 'w')
  g_dashboard_fp.write('<html>\n')
  g_dashboard_fp.write('<head>\n')
  g_dashboard_fp.write('<h1 align="center">%s Dashboard</h1>\n' % (user_name))
  g_dashboard_fp.write('</head>\n')
  g_dashboard_fp.write('<body>\n')
  g_dashboard_fp.write('<p>Number of cycles (N): %d</p>' % (num_docs))
  g_dashboard_fp.write('<table border="1">\n')

  g_dashboard_fp.write('<tr>\n')
  for interval in interval_list: # trend intervals + 1
    g_dashboard_fp.write('<td align="center">\n')
    g_dashboard_fp.write(interval[1])
    g_dashboard_fp.write('</td>\n')
  g_dashboard_fp.write('</tr>\n')

  g_dashboard_fp.write('<tr>\n')
  cycle_hash_freqs = {}
  keywords_tweeters_map = {}
  flag = 0
  outfile = ''
  filename = []
  trends_per_cycle = {}
  outfile_time_stamp = ''
  num_words_interval = 0
  num_distinct_words_interval = 0
  num_words_cycle = 0
  for i in range(0, num_docs):
    if (i+1 > max_interval):
      break
    kfile = kfiles[i]
    split_word = 'keywords_by_' + user_name + '_followers.'
    filename = kfile.split(split_word)
    if (flag == 0):
      flag = 1
      outfile_time_stamp = filename[1]
    time_stamp = filename[1]
    num_words_cycle = get_keyword_freqs(kfile, cycle_hash_freqs)
    keywords_tweeters_map_file_name = data_dir + '/' + user_name + '/' + 'keywords_tweeters_map_for_' + user_name + '.' + time_stamp
    generate_map(keywords_tweeters_map_file_name, keywords_tweeters_map)
    trends_per_cycle[i] = num_words_cycle
    num_words_interval += num_words_cycle
    if (num_words_cycle <= 0):
      print '%s had no trends. this is in %d cycle' % (kfile, i+1)
    if (interval_hash.has_key(i+1) or (i+1 == num_docs)):
      num_distinct_words_interval = len(cycle_hash_freqs) # num words in current interval
      if (num_distinct_words_interval > 0 and num_words_interval > 0):
        outfile = path + 'trends.' + outfile_time_stamp + '.' + str(i+1)
        g_dashboard_fp.write('<td valign="top" width="%d">\n' % (100/interval_len))
        calculate_tf_idf_and_print(cycle_hash_freqs, float(num_distinct_words_interval), float(num_words_interval), keywords_tweeters_map, outfile)
        g_dashboard_fp.write('</td>\n')
        g_hash_tf_idf.clear()
      num_words_interval = 0

  g_dashboard_fp.write('</tr>\n')

  g_dashboard_fp.write('<tr>')
  g_dashboard_fp.write('<td valign="top">')
  prev = 0
  for interval in interval_list:
    i = interval[0]
    if (prev >= num_docs):
      break
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
    prev = i
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
