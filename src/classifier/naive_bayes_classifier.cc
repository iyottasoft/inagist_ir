#include "naive_bayes_classifier.h"
#include <iostream>
#include <cmath>

#ifdef DEBUG
#if DEBUG>0
#define NBC_DEBUG DEBUG
#endif
#endif
//#define NBC_DEBUG 5

namespace inagist_classifiers {

NaiveBayesClassifier::NaiveBayesClassifier() {
#ifdef NBC_DEBUG
  m_debug_level = NBC_DEBUG;
  if (m_debug_level > 1) {
    std::cout << "NBC debug level (default): " << m_debug_level << std::endl;
  }
#else
  m_debug_level = 0;
#endif
}

NaiveBayesClassifier::~NaiveBayesClassifier() {
}

int NaiveBayesClassifier::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  if (m_debug_level > 1) {
    std::cout << "NBC debug level: " << m_debug_level << std::endl;
  }
  return 0;
}

// this is pretty naive!
// TODO (balaji) need to build a hash map with (say) ngram as key and
// a list of structs with frequency and corpus as keys
int NaiveBayesClassifier::GuessClass(CorpusMap& corpus_map,
                                     Corpus& classes_freq_map,
                                     Corpus& test_corpus,
                                     std::string& guess_class_output,
                                     unsigned int debug_level) {

  if (test_corpus.size() < 1) {
    guess_class_output = "RR";
    return -1;
  }

  CorpusMapIter corpus_map_iter;
  CorpusIter test_corpus_iter;
  CorpusIter corpus_iter;
  CorpusIter loc;

  double freqs[MAX_CORPUS_NUMBER];
  double temp_freq = 0;
  double temp_total_freq = 0;
  double prior_entry_for_class = 0;
  double prior_total_entries = 0;
  unsigned int i = 0;
  std::string classes[MAX_CORPUS_NUMBER];
 
  if ((corpus_iter = classes_freq_map.find("all_classes")) != classes_freq_map.end()) {
    prior_total_entries = (*corpus_iter).second;
  }
  bool entry_found = false;
  for (corpus_map_iter = corpus_map.begin(); corpus_map_iter != corpus_map.end(); corpus_map_iter++) {
    classes[i] = (*corpus_map_iter).first;
    if ((corpus_iter = classes_freq_map.find(classes[i])) != classes_freq_map.end()) {
      prior_entry_for_class = (*corpus_iter).second;
    }
    freqs[i] = 0;
    temp_freq = 0;
    temp_total_freq = 0;
    for (test_corpus_iter = test_corpus.begin(); test_corpus_iter != test_corpus.end(); test_corpus_iter++) {
      corpus_iter = ((*corpus_map_iter).second).find((*test_corpus_iter).first); 
      if (corpus_iter != ((*corpus_map_iter).second).end()) {
#ifdef NBC_DEBUG
        if (debug_level > 4) {
          std::cout << (*corpus_iter).first << " : " << (*corpus_iter).second << " in " << (*corpus_map_iter).first << std::endl;
        }
#endif
        temp_freq = (double) (*corpus_iter).second;
        if (temp_freq > 8) {
          temp_freq = 8;
        }
        temp_total_freq += temp_freq;
        entry_found = true;
      }
    }
    if (temp_total_freq > 0) {
      freqs[i] += log(temp_total_freq/test_corpus.size());
    }
#ifdef NBC_DEBUG
    if (debug_level > 3) {
      std::cout << classes[i] << ": " << freqs[i] << " where prior record is (" \
                << prior_entry_for_class << " / " << prior_total_entries << ")" << std::endl;
    }
#endif
    if (prior_entry_for_class > 0 && prior_total_entries) {
      freqs[i] += log(prior_entry_for_class/prior_total_entries);
    }
    i++;
  }

  if (!entry_found) {
    guess_class_output = "XX";
    return 0;
  }

  double max_freq = 0;
  unsigned int max_index = 0;
  unsigned int max_duplicate_count = 0;
  freqs[0] = exp(freqs[0]);
  max_freq = freqs[0];
  max_index = 0;
  for (unsigned int j=1; j<i; j++) {
    freqs[j] = exp(freqs[j]);
#ifdef NBC_DEBUG
    if (debug_level > 2) {
      std::cout << classes[j] << " freqs: " << freqs[j] << std::endl;
    }
#endif
    if (max_freq == freqs[j]) {
      max_duplicate_count++;
    } else if (max_freq < freqs[j]) {
      max_freq = freqs[j];
      max_index = j;
      max_duplicate_count = 0;
    }
  }

#ifdef NBC_DEBUG
  if (debug_level > 1) {
    std::cout << "max freq: " << max_freq << std::endl;
    std::cout << "max index: " << max_index << std::endl;
    std::cout << "guess_class_output: " << classes[max_index] << std::endl;
    std::cout << "max duplicate count: " << max_duplicate_count << std::endl;
  }
#endif

  if (0 == max_duplicate_count) {
    guess_class_output = classes[max_index];
    return 1;
  } else {
    guess_class_output = "UU";
  }

  return 0;
}

int NaiveBayesClassifier::GuessClass(std::map<std::string, int> testfile_features_map,
                                 std::map<std::string, int> class1_features_map,
                                 std::map<std::string, int> class2_features_map) {

  double class1_freq = 0;
  double class2_freq = 0;
  double freq = 0;
  std::map<std::string, int>::iterator map_iter;
  std::map<std::string, int>::iterator freq_iter;
  for (map_iter = testfile_features_map.begin(); map_iter != testfile_features_map.end(); map_iter++) {
    //std::cout << (*map_iter).first << " = " << (*map_iter).second << std::endl;
    freq_iter = class1_features_map.find((*map_iter).first); 
    if (freq_iter != class1_features_map.end()) {
       freq = (double) (*freq_iter).second;
       //std::cout << freq << std::endl;
       class1_freq += log(freq); 
    }
    freq_iter = class2_features_map.find((*map_iter).first); 
    if (freq_iter != class2_features_map.end()) {
       freq = (double) (*freq_iter).second;
       //std::cout << freq << std::endl;
       class2_freq += log(freq); 
    }
  }

#ifdef NBC_DEBUG
  if (m_debug_level > 1) {
    std::cout << "Class 1 freq: " << class1_freq << std::endl;
    std::cout << "Class 2 freq: " << class2_freq << std::endl;
  }
#endif

  double score1 = exp(class1_freq);
  double score2 = exp(class2_freq);

#ifdef NBC_DEBUG
  if (m_debug_level > 1) {
    std::cout << "Class 1 score: " << score1 << std::endl;
    std::cout << "Class 2 score: " << score2 << std::endl;
  }
#endif

  if (score1 > score2) {
    return 1;
  } else {
    return 2;
  }

  return 0;
}

}
