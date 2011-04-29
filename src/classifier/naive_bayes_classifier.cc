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
#ifdef NBC_DEBUG
  std::cout << "setting NBC debug level to " << m_debug_level << std::endl;
#endif
  return 0;
}

void inline Initialize(double array[], unsigned int size) {
  for (unsigned int i=0; i<size; i++)
    array[i] = 0;
}

// this is pretty naive!
// TODO (balaji) need to build a hash map with (say) ngram as key and
// a list of structs with frequency and corpus as keys
//
// note: while testing to create the prior frequencies, don't send the classes_freq_map.
//
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
  CorpusIter class_freq_iter;
  CorpusIter loc;
  Corpus* corpus_ptr;

  double prior_freqs[MAX_CORPUS_NUMBER];
  Initialize(prior_freqs, MAX_CORPUS_NUMBER);
  double freqs[MAX_CORPUS_NUMBER];
  Initialize(freqs, MAX_CORPUS_NUMBER);
  double temp_freq = 0;
  double temp_total_freq = 0;
  double prior_entry_for_class = 0;
  double prior_total_entries = 0;
  unsigned int i = 0;
  std::string classes[MAX_CORPUS_NUMBER];
  std::string class_name;
  std::string test_element;

  if (corpus_map.size() > MAX_CORPUS_NUMBER) {
    std::cerr << "ERROR: exceeds max classes that this classifier can handle\n";
    return -1;
  }

  if ((corpus_iter = classes_freq_map.find("all_classes")) != classes_freq_map.end()) {
    prior_total_entries = (*corpus_iter).second;
  }
  bool entry_found = false;
  for (corpus_map_iter = corpus_map.begin(); corpus_map_iter != corpus_map.end(); corpus_map_iter++) {

    // for this iteration, what is the class name and corpus?
    class_name = corpus_map_iter->first;
    if (class_name.empty()) {
      std::cerr << "ERROR: invalid class name. fatal error\n";
      break;
    }
    classes[i] = class_name;
    corpus_ptr = &(corpus_map_iter->second);
    if (corpus_ptr->empty()) {
      std::cout << "WARNING: no entries found in corpus for class: " << class_name << std::endl;
      continue;
    }

    // now that we know the classes, do we have its previous frequency?
    // lets get it from classes_freq_map
    if (!classes_freq_map.empty()) {
      if ((class_freq_iter = classes_freq_map.find(class_name)) != classes_freq_map.end()) {
        prior_entry_for_class = class_freq_iter->second;
      }
      if (prior_entry_for_class > 0 && prior_total_entries) {
        prior_freqs[i] += log(prior_entry_for_class/prior_total_entries);
      }
    }

    // now lets pit this corpus with the test corpus
    freqs[i] = 0;
    prior_freqs[i] = 0;
    temp_freq = 0;
    temp_total_freq = 0;
    for (test_corpus_iter = test_corpus.begin(); test_corpus_iter != test_corpus.end(); test_corpus_iter++) {
      test_element = test_corpus_iter->first; 
      // see if this element in the test corpus is present in the current corpus
      corpus_iter = corpus_ptr->find(test_element); 
      if (corpus_iter != corpus_ptr->end()) {
#ifdef NBC_DEBUG
        if (debug_level > 4 || NBC_DEBUG > 4) {
          std::cout << (*corpus_iter).first << " : " << (*corpus_iter).second << " in " << class_name << std::endl;
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
    i++;
  }

  if (!entry_found) {
    guess_class_output = "XX";
    return 0;
  }

  if (!classes_freq_map.empty()) {
    for (unsigned int i=0; i<classes_freq_map.size(); i++) {
#ifdef NBC_DEBUG
      if (debug_level > 3 || NBC_DEBUG > 3) {
        std::cout << classes[i] << ": " << freqs[i] << " where prior freq is " << prior_freqs[i] << " (" \
                  << prior_entry_for_class << " / " << prior_total_entries << ")" << std::endl;
      }
#endif
      // note this is a crucial step. don't think this is a debug code
      freqs[i] += prior_freqs[i];
    }
  } else {
#ifdef NBC_DEBUG
    if (debug_level > 3 || NBC_DEBUG > 3) {
      std::cout << classes[i] << ": " << freqs[i] << std::endl;
    }
#endif
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
    if (debug_level > 2 || NBC_DEBUG > 2) {
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
  if (debug_level > 1 || NBC_DEBUG > 1) {
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
