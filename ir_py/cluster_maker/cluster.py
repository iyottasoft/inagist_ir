#!/usr/bin/env python2.6

import sys
import string
from math import log
from math import sqrt

tuple_stop_words = ['a','able','about','across','after','all','almost','also','am','among','an','and','any','are','as','at','be','because','been','but','by','can','cannot','could','dear','did','do','does','either','else','ever','every','for','from','get','got','had','has','have','he','her','hers','him','his','how','however','i','if','in','into','is','it','its','just','least','let','like','likely','may','me','might','most','must','my','neither','no','nor','not','of','off','often','on','only','or','other','our','own','rather','said','say','says','she','should','since','so','some','than','that','the','their','them','then','there','these','they','this','tis','to','too','twas','us','wants','was','we','were','what','when','where','which','while','who','whom','why','will','with','would','yet','you','your'] 

# goes over the tweets, populates the following
total_docs = 0
hash_term_freqs = {}
docs_list = []
docs_tokens_list = []
def read_tweets():
  global total_docs
  f = open('tweets.txt', 'r')
  lines = f.readlines()
  for line in lines: # first pass over all docs
    total_docs += 1
    line = line.translate(string.maketrans("",""), string.punctuation)
    line = line.rstrip()
    docs_list.append(line)
    tokens = line.split(' ')
    #docs_tokens_list.append(tokens)
    doc_tokens = []
    for token in tokens:
      # TODO (balaji) stop words can be handled later 
      if token in tuple_stop_words:
        continue
      else:
        doc_tokens.append(token)
      # TODO (balaji) assuming words to be unique in a tweet.
      # if the above assumption is invalid, use a hash and take the unique ones
      if hash_term_freqs.has_key(token):
        hash_term_freqs[token] += 1
      else:
        hash_term_freqs[token] = 1
    docs_tokens_list.append(doc_tokens)
  return total_docs

docs_term_weights_list = []
def compute_term_weights():
  term_weights()
  for tokens in docs_tokens_list:
    weights_list = []
    for token in tokens:
      weights_list.append(hash_inverse_doc_freqs_squared[token])
    docs_term_weights_list.append(weights_list)

# term weights in our case are just idf (for tf = 1)
hash_inverse_doc_freqs_squared = {}
def term_weights():
  # idf = log (# of docs/# of docs in which this word is present)
  hash_inverse_doc_freqs = {}
  # second pass over all words and hence all docs
  for (word, num_docs) in hash_term_freqs.items():
    idf = log(total_docs/num_docs)
    hash_inverse_doc_freqs[word] = idf
    hash_inverse_doc_freqs_squared[word] = idf * idf
  return 0

docs_euclid_list = []
def compute_euclidean_distance():
  for tokens in docs_tokens_list:
    sum_weights_squared = 0.0
    for token in tokens:
      if hash_inverse_doc_freqs_squared.has_key(token):
        sum_weights_squared += hash_inverse_doc_freqs_squared[token]
    docs_euclid_list.append(sqrt(sum_weights_squared))
  return 0

docs_sim_list = []
active_clusters = []
def compute_cosim():
  for i in range(total_docs):
    #for j in range(i):
    #  print "0.00",
    sim_list = []
    #for j in range(i, total_docs):
    for j in range(total_docs):
      val = cosim(docs_term_weights_list[i], docs_euclid_list[i], docs_term_weights_list[j], docs_euclid_list[j])
      sim_list.append(val)
    docs_sim_list.append(sim_list)
    active_clusters.append(1)
  return 0

def cosim(vector_a, a_euclid, vector_b, b_euclid):
  if len(vector_b) < len(vector_a):
    return cosim(vector_b, b_euclid, vector_a, a_euclid)

  sum_dot_products = 0
  i = 0
  for weight in vector_a:
    sum_dot_products += weight * vector_b[i]
    i += 1

  return (sum_dot_products/(a_euclid * b_euclid))

def print_cosim():
  for i in range(total_docs):
    for j in range(total_docs):
      print "%.2f" % docs_sim_list[i][j],
    print ""
  
clusters = []
def simple_hac():
  print_cosim()
  for i in range(total_docs-1):
    max_sim = 0
    max_sim_j = i
    cluster_list = []
    for j in range(i+1, total_docs):
      if (docs_sim_list[i][j] > max_sim):
        max_sim = docs_sim_list[i][j] # TODO (balaji) optimize
        max_sim_j = j
        
    print "max %.2f between %d and %d" % (max_sim, i, max_sim_j)
    print docs_list[i]
    print docs_list[max_sim_j]
    clusters.append(cluster_list)
  
def main():
  read_tweets()
  print "total docs = %s" % total_docs
  compute_term_weights()
  compute_euclidean_distance()
  compute_cosim()
  simple_hac()
  return 0

if __name__ == "__main__":
  main()
