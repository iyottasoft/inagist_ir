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
  unsigned int count = 0;
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
    classes[count] = class_name;
    corpus_ptr = &(corpus_map_iter->second);
    if (corpus_ptr->empty()) {
      std::cout << "WARNING: no entries found in corpus for class: " << class_name << std::endl;
      continue;
    }

    freqs[count] = 0;
    prior_freqs[count] = 0;

    // now that we know the classes, do we have its previous frequency?
    // lets get it from classes_freq_map
    if (!classes_freq_map.empty()) {
      if ((class_freq_iter = classes_freq_map.find(class_name)) != classes_freq_map.end()) {
        prior_entry_for_class = class_freq_iter->second;
      }
      if (prior_entry_for_class > 0 && prior_total_entries) {
        prior_freqs[count] += log(prior_entry_for_class/prior_total_entries);
        freqs[count] = prior_freqs[count];
      }
    }

    // now lets pit this corpus with the test corpus
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
      freqs[count] += log(temp_total_freq/test_corpus.size());
    }
    count++;
  }

  if (!entry_found) {
    guess_class_output = "XX";
    return 0;
  }

#ifdef NBC_DEBUG
  if (!classes_freq_map.empty()) {
    for (unsigned int i=0; i<classes_freq_map.size(); i++) {
      if (debug_level > 3 || NBC_DEBUG > 3) {
        std::cout << classes[i] << ": " << freqs[i] << " where prior freq is " << prior_freqs[i] << " (" \
                  << prior_entry_for_class << " / " << prior_total_entries << ")" << std::endl;
      }
    }
  } else {
    for (unsigned int i=0; i<classes_freq_map.size(); i++) {
      if (debug_level > 3 || NBC_DEBUG > 3) {
        std::cout << classes[i] << ": " << freqs[i] << std::endl;
      }
    }
  }
#endif

  double max_freq = 0;
  unsigned int max_index = 0;
  unsigned int max_duplicate_count = 0;
  freqs[0] = exp(freqs[0]);
  max_freq = freqs[0];
  max_index = 0;
  for (unsigned int j=1; j<count; j++) {
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

int NaiveBayesClassifier::GuessClass2(CorpusMap& corpus_map,
                                      Corpus& classes_freq_map,
                                      Corpus& test_corpus,
                                      std::string& guess_class_output,
                                      std::string& top_classes_output,
                                      unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                      , std::map<std::string, std::string>& class_contributors
#endif // CLASS_CONTRIBUTORS_ENABLED
                                      , unsigned int debug_level
                                     ) {

  top_classes_count = 0;

  if (test_corpus.size() < 1) {
    guess_class_output = "RR";
    top_classes_output = "RR";
    top_classes_count = 1;
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
  unsigned int count = 0;
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
    classes[count] = class_name;
    corpus_ptr = &(corpus_map_iter->second);
    if (corpus_ptr->empty()) {
      std::cout << "WARNING: no entries found in corpus for class: " << class_name << std::endl;
      continue;
    }

    freqs[count] = 0;
    prior_freqs[count] = 0;

    // now that we know the classes, do we have its previous frequency?
    // lets get it from classes_freq_map
    if (!classes_freq_map.empty()) {
      if ((class_freq_iter = classes_freq_map.find(class_name)) != classes_freq_map.end()) {
        prior_entry_for_class = class_freq_iter->second;
      }
      if (prior_entry_for_class > 0 && prior_total_entries) {
        prior_freqs[count] += log(prior_entry_for_class/prior_total_entries);
        freqs[count] = prior_freqs[count];
      }
    }

    // now lets pit this corpus with the test corpus
    temp_freq = 0;
    temp_total_freq = 0;
#ifdef CLASS_CONTRIBUTORS_ENABLED
    std::map<std::string, std::string>::iterator cc_iter;
#endif // CLASS_CONTRIBUTORS_ENABLED
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
#ifdef CLASS_CONTRIBUTORS_ENABLED
        if ((cc_iter = class_contributors.find(class_name)) != class_contributors.end()) {
          cc_iter->second += "," + test_element;
        } else {
          class_contributors[class_name].assign(test_element);
        }
#endif // CLASS_CONTRIBUTORS_ENABLED
      }
    }
    if (temp_total_freq > 0) {
      freqs[count] += log(temp_total_freq/test_corpus.size());
    }
    count++;
  }

  if (!entry_found) {
    guess_class_output = "XX";
    return 0;
  }

#ifdef NBC_DEBUG
  if (!classes_freq_map.empty()) {
    for (unsigned int i=0; i<classes_freq_map.size(); i++) {
      if (debug_level > 3 || NBC_DEBUG > 3) {
        std::cout << classes[i] << ": " << freqs[i] << " where prior freq is " << prior_freqs[i] << " (" \
                  << prior_entry_for_class << " / " << prior_total_entries << ")" << std::endl;
      }
    }
  } else {
    for (unsigned int i=0; i<classes_freq_map.size(); i++) {
      if (debug_level > 3 || NBC_DEBUG > 3) {
        std::cout << classes[i] << ": " << freqs[i] << std::endl;
      }
    }
  }
#endif

  double max_freq = 0;
  unsigned int max_index = 0;
  unsigned int max_duplicate_count = 0;
  double sum = 0;
  freqs[0] = exp(freqs[0]);
  max_freq = freqs[0];
  max_index = 0;
  for (unsigned int j=1; j<count; j++) {
    freqs[j] = exp(freqs[j]);
    sum += freqs[count];
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

  double mean = sum/count;
  double top1 = 0;
  unsigned int top1_index = 0;
  double top2 = 0;
  unsigned int top2_index = 0;
  double top3 = 0;
  unsigned int top3_index = 0;
  double freq = 0;
  if (count > 1) {
    if (freqs[0] > freqs[1]) {
      top1 = freqs[0];
      top1_index = 0;
      top2 = freqs[1];
      top2_index = 1;
    } else {
      top1 = freqs[1];
      top1_index = 1;
      top2 = freqs[0];
      top2_index = 0;
    }
  }
  if (count > 2) {
    top3 = freqs[2];
    top3_index = 2;
    Heapify(top1, top1_index, top2, top2_index, top3, top3_index);
  }
  for (unsigned int j=2; j<count; j++) {
    freq = freqs[j];
    if (freq > top3) {
      top3 = freq;
      top3_index = j;
      Heapify(top1, top1_index, top2, top2_index, top3, top3_index);
    }
  }
  if (count > 2) {
    top_classes_output = classes[top1_index];
    top_classes_count++;
    if (top2 > mean) {
      top_classes_output += "|" + classes[top2_index];
      top_classes_count++;
    }
    if (top3 > mean) {
      top_classes_output += "|" + classes[top3_index];
      top_classes_count++;
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

int NaiveBayesClassifier::Heapify(double& top1, unsigned int& top1_index,
                                         double& top2, unsigned int& top2_index,
                                         double& top3, unsigned int& top3_index) {
  double freq = top3;
  unsigned int freq_index = top3_index;
  if (freq > top2) {
    if (freq > top1) {
      top3_index = top2_index;
      top3 = top2;
      top2_index = top1_index;
      top2 = top1;
      top1_index = freq_index;
      top1 = freq;
      return 2;
    } else {
      top3_index = top2_index;
      top3 = top2;
      top2_index = freq_index;
      top2 = freq;
      return 1;
    }
  }
  return 0;
}

}
