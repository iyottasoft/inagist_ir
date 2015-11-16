#include "naive_bayes_classifier.h"
#include <iostream>
#include <fstream>
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
#else
  m_debug_level = 0;
#endif // NBC_DEBUG
}

NaiveBayesClassifier::~NaiveBayesClassifier() {
}

int NaiveBayesClassifier::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
#ifdef NBC_DEBUG
  std::cout << "setting NBC debug level to " << m_debug_level << std::endl;
#endif // NBC_DEBUG
  return 0;
}

void inline Initialize(double array[], unsigned int size) {
  for (unsigned int i=0; i<size; i++)
    array[i] = 0;
}

// 
// TODO (balaji) this is pretty naive! need to build a hash map with (say) ngram as key and
// a list of structs with frequency and corpus as keys
//
// DONE (balaji) - gist_maker reads from a dictionary with the below structure
// <std::string, std::map<std::string, double> (word, class_name, freq)
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
  std::string class_label;
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
#ifdef NBC_DEBUG
      std::cout << "WARNING: no entries found in corpus for class: " << class_name << std::endl;
#endif // NBC_DEBUG
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
#endif // NBC_DEBUG
        temp_total_freq += (double) (*corpus_iter).second;
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
  if (debug_level > 3 || NBC_DEBUG > 3) {
    if (!classes_freq_map.empty()) {
      for (unsigned int i=0; i<count; i++) {
          std::cout << classes[i] << ": " << freqs[i] << " prior: " << prior_freqs[i] << std::endl;
      }
    } else {
      for (unsigned int i=0; i<count; i++) {
          std::cout << classes[i] << ": " << freqs[i] << std::endl;
      }
    }
  }
#endif // NBC_DEBUG

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

int NaiveBayesClassifier::GuessClass3(CorpusMap& corpus_map,
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

  if (test_corpus.size() < 1) {
#ifdef NBC_DEBUG
    guess_class_output = "RR";
    top_classes_output = "RR";
    top_classes_count = 1;
    std::cerr << "WARNING: empty input text corpus. can't classify\n";
#endif // NBC_DEBUG
    return -1;
  }

  top_classes_count = 0;

  std::set<std::string> top_classes_set;
  int ret_val = 0;
  if (GuessClass3(corpus_map,
                  classes_freq_map,
                  test_corpus,
                  guess_class_output,
                  top_classes_set
#ifdef CLASS_CONTRIBUTORS_ENABLED
                  , class_contributors
#endif // CLASS_CONTRIBUTORS_ENABLED
                  , debug_level
                 ) < 0) {
#ifdef NBC_DEBUG
    std::cerr << "ERROR: could not get top classes set\n";
#endif // NBC_DEBUG
    return -1;
  }

  std::set<std::string>::iterator set_iter;
  if (!top_classes_set.empty()) {
    set_iter = top_classes_set.begin();
    top_classes_output.assign(*set_iter); 
    top_classes_count++;
    set_iter++;
    for (; set_iter != top_classes_set.end(); set_iter++) {
      top_classes_output += "|" + *set_iter; 
      top_classes_count++;
    }
    top_classes_output += "|";
  }

  return ret_val;
}

/*
  c = arg max [ log p(c) + sigma 1<=k<=n log p(tk|c) ]
  the probability of document d being in class c is equal to the sum over 1 to n of the probablity of each term being in class c plus the prior probability of the class among all classes.
*/
int NaiveBayesClassifier::GuessClass2(CorpusMap& corpus_map,
                                      Corpus& classes_freq_map,
                                      Corpus& test_corpus,
                                      std::string& guess_class_output,
                                      std::set<std::string>& top_classes_set
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                      , std::map<std::string, std::string>& class_contributors
#endif // CLASS_CONTRIBUTORS_ENABLED
                                      , unsigned int debug_level
                                     ) {

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
  std::string key_str;
  std::string value_str;

  if (corpus_map.size() > MAX_CORPUS_NUMBER) {
    std::cerr << "ERROR: exceeds max classes that this classifier can handle\n";
    return -1;
  }

  if ((corpus_iter = classes_freq_map.find("all_classes")) != classes_freq_map.end()) {
    prior_total_entries = (*corpus_iter).second;
  }
  bool entry_found = false;
  double freq = 0;
  double prior_freq = 0;
  freqs[count] = 0;
  prior_freqs[count] = 0;

  for (corpus_map_iter = corpus_map.begin(); corpus_map_iter != corpus_map.end(); corpus_map_iter++) {
    freq = 0;
    prior_freq = 0;
    // for this iteration, what is the class name and corpus?
    class_name = corpus_map_iter->first;
    if (class_name.empty()) {
      std::cerr << "ERROR: invalid class name. fatal error\n";
      break;
    }
    corpus_ptr = &(corpus_map_iter->second);
    if (corpus_ptr->empty()) {
#ifdef NBC_DEBUG
      std::cout << "WARNING: no entries found in corpus for class: " << class_name << std::endl;
#endif // NBC_DEBUG
      continue;
    }

    // now lets pit this corpus with the test corpus
    temp_freq = 0;
    temp_total_freq = 0;
#ifdef CLASS_CONTRIBUTORS_ENABLED
    std::map<std::string, std::string>::iterator cc_iter;
#endif // CLASS_CONTRIBUTORS_ENABLED
    for (test_corpus_iter = test_corpus.begin();
         test_corpus_iter != test_corpus.end();
         test_corpus_iter++) {
      test_element = test_corpus_iter->first; 
      // see if this element in the test corpus is present in the current corpus
      corpus_iter = corpus_ptr->find(test_element); 
      if (corpus_iter != corpus_ptr->end()) {
#ifdef NBC_DEBUG
        if (debug_level > 4 || NBC_DEBUG > 4) {
          std::cout << (*corpus_iter).first << " : " << (*corpus_iter).second \
                    << " in " << class_name << std::endl;
        }
#endif
        temp_freq = (double) (*corpus_iter).second;
        temp_total_freq += log(temp_freq); // note this is addition of logs - multiplication of probabilities
        entry_found = true;
#ifdef CLASS_CONTRIBUTORS_ENABLED
        key_str.assign(test_element);
        value_str = class_name + ";";
        if ((cc_iter = class_contributors.find(key_str)) != class_contributors.end()) {
          cc_iter->second += value_str; 
        } else {
          class_contributors[key_str].assign(value_str);
        }
#endif // CLASS_CONTRIBUTORS_ENABLED
      }
    }
    if (!entry_found)
      continue;

    // what is this? why divide the product of term frequencies in class C with the number of words in document D?
    /*
    if (temp_total_freq > 0) {
      freq = log(temp_total_freq/test_corpus.size());
    }
    */
    freq = temp_total_freq;
    if (freq <= 0)
      continue;

    // now that we know the classes, do we have its previous frequency?
    // lets get it from classes_freq_map
    if (!classes_freq_map.empty()) {
      if ((class_freq_iter = classes_freq_map.find(class_name)) != classes_freq_map.end()) {
        prior_entry_for_class = class_freq_iter->second;
      }
      if (prior_entry_for_class > 0 && prior_total_entries) {
        prior_freq = log(prior_entry_for_class/prior_total_entries);
      }
    }

    if (freq > 0) {
      classes[count] = class_name;
      prior_freqs[count] = prior_freq;
      freqs[count] = freq; // + prior_freq; MASSIVE DECISION!
      count++;
      freqs[count] = 0;
      prior_freqs[count] = 0;
    }
  }

  if (!entry_found || count == 0) {
    guess_class_output = "XX";
    return 0;
  }

#ifdef NBC_DEBUG
  if (debug_level > 3 || NBC_DEBUG > 3) {
    if (!classes_freq_map.empty()) {
      for (unsigned int i=0; i<count; i++) {
          std::cout << classes[i] << ": " << freqs[i] << " prior: " << prior_freqs[i] << std::endl;
      }
    } else {
      for (unsigned int i=0; i<count; i++) {
          std::cout << classes[i] << ": " << freqs[i] << std::endl;
      }
    }
  }
#endif // NBC_DEBUG

  double max_freq = 0;
  unsigned int max_index = 0;
  unsigned int max_duplicate_count = 0;
  double sum = 0;
  freqs[0] = exp(freqs[0]);
  sum += freqs[0];
  max_freq = freqs[0];
  max_index = 0;
  for (unsigned int j=1; j<count; j++) {
    freqs[j] = exp(freqs[j]);
    //sum += freqs[count];
    sum += freqs[j];
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
  for (unsigned int j=3; j<count; j++) {
    freq = freqs[j];
    if (freq > top3) {
      top3 = freq;
      top3_index = j;
      Heapify(top1, top1_index, top2, top2_index, top3, top3_index);
    }
  }

  top_classes_set.insert(classes[top1_index]);
  if (count > 1) {
    if (top2 > mean) {
      top_classes_set.insert(classes[top2_index]);
    }
  }
  if (count > 2) {
    if (top3 > mean) {
      top_classes_set.insert(classes[top3_index]);
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

/* sorry for the poor naming of function! */
/*
  c = arg max [ log p(c) + sigma 1<=k<=n log p(tk|c) ]
  the probability of document d being in class c is equal to the sum over 1 to n of the probablity of each term being in class c plus the prior probability of the class among all classes.

  this implementation assumes that the values are already log of the probabilities.
*/

int NaiveBayesClassifier::GuessClass3(CorpusMap& corpus_map,
                                      Corpus& classes_prior_prob_map,
                                      Corpus& test_corpus,
                                      std::string& guess_class_output,
                                      std::set<std::string>& top_classes_set
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                      , std::map<std::string, std::string>& class_contributors
#endif // CLASS_CONTRIBUTORS_ENABLED
                                      , unsigned int debug_level
                                     ) {

  if (test_corpus.size() < 1) {
#ifdef NBC_DEBUG
    guess_class_output = "RR";
    std::cerr << "WARNING: empty input text corpus. can't classify\n";
#endif // NBC_DEBUG
    return -1;
  }

  if (corpus_map.size() > MAX_CORPUS_NUMBER) {
    std::cerr << "ERROR: exceeds max classes that this classifier can handle\n";
    return -1;
  }

  double prior_total_entries = 0;
  CorpusIter corpus_iter;
  if ((corpus_iter = classes_prior_prob_map.find("all_classes")) != classes_prior_prob_map.end()) {
    prior_total_entries = (*corpus_iter).second;
  }

  CorpusMapIter corpus_map_iter;
  CorpusIter test_corpus_iter;
  CorpusIter class_freq_iter;
  CorpusIter loc;
  Corpus* corpus_ptr;

  double prior_prob_values[MAX_CORPUS_NUMBER];
  Initialize(prior_prob_values, MAX_CORPUS_NUMBER);
  double prob_values[MAX_CORPUS_NUMBER];
  Initialize(prob_values, MAX_CORPUS_NUMBER);
  double prior_entry_for_class = 0;
  unsigned int count = 0;
  std::string classes[MAX_CORPUS_NUMBER];
  std::string class_name;
  std::string test_element;
  std::string key_str;
  std::string value_str;

  bool entry_found = false;
  double prob_value = 0;
  double prior_prob_value = 0;
  double total_prob_value = 0;
  prob_values[count] = 0;
  prior_prob_values[count] = 0;

  for (corpus_map_iter = corpus_map.begin(); corpus_map_iter != corpus_map.end(); corpus_map_iter++) {
    prob_value = 0;
    prior_prob_value = 0;
    total_prob_value = 0;
    // for this iteration, what is the class name and corpus?
    class_name = corpus_map_iter->first;
    if (class_name.empty()) {
      std::cerr << "ERROR: invalid class name. fatal error\n";
      break;
    }
    corpus_ptr = &(corpus_map_iter->second);
    if (corpus_ptr->empty()) {
#ifdef NBC_DEBUG
      std::cout << "WARNING: no entries found in corpus for class: " << class_name << std::endl;
#endif // NBC_DEBUG
      continue;
    }

    // now lets pit this corpus with the test corpus
#ifdef CLASS_CONTRIBUTORS_ENABLED
    std::map<std::string, std::string>::iterator cc_iter;
#endif // CLASS_CONTRIBUTORS_ENABLED
    for (test_corpus_iter = test_corpus.begin();
         test_corpus_iter != test_corpus.end();
         test_corpus_iter++) {
      test_element = test_corpus_iter->first; 
      // see if this element in the test corpus is present in the current corpus
      corpus_iter = corpus_ptr->find(test_element); 
      if (corpus_iter != corpus_ptr->end()) {
#ifdef NBC_DEBUG
        if (debug_level > 4 || NBC_DEBUG > 4) {
          std::cout << (*corpus_iter).first << " : " << (*corpus_iter).second \
                    << " in " << class_name << std::endl;
        }
#endif
        prob_value = (double) (*corpus_iter).second;
        total_prob_value += prob_value;
        entry_found = true;
#ifdef CLASS_CONTRIBUTORS_ENABLED
        key_str.assign(test_element);
        value_str = class_name + ";";
        if ((cc_iter = class_contributors.find(key_str)) != class_contributors.end()) {
          cc_iter->second += value_str; 
        } else {
          class_contributors[key_str].assign(value_str);
        }
#endif // CLASS_CONTRIBUTORS_ENABLED
      }
    }
    if (!entry_found)
      continue;

    // now that we know the classes, do we have its previous frequency?
    // lets get it from classes_prior_prob_map
    if (!classes_prior_prob_map.empty()) {
      if ((class_freq_iter = classes_prior_prob_map.find(class_name)) != classes_prior_prob_map.end()) {
        prior_prob_value = class_freq_iter->second;
      }
    }

    if (total_prob_value != 0) {
      classes[count] = class_name;
      prior_prob_values[count] = prior_prob_value;
      prob_values[count] = total_prob_value; // + prior_prob_value; MASSIVE DECISION!
      count++;
      prob_values[count] = 0;
      prior_prob_values[count] = 0;
    }
  }

  if (!entry_found || count == 0) {
    guess_class_output = "XX";
    return 0;
  }

#ifdef NBC_DEBUG
  if (debug_level > 3 || NBC_DEBUG > 3) {
    if (!classes_prior_prob_map.empty()) {
      for (unsigned int i=0; i<count; i++) {
          std::cout << classes[i] << ": " << prob_values[i] << " prior: " << prior_prob_values[i] << std::endl;
      }
    } else {
      for (unsigned int i=0; i<count; i++) {
          std::cout << classes[i] << ": " << prob_values[i] << std::endl;
      }
    }
  }
#endif // NBC_DEBUG

  double max_prob_value = 0;
  unsigned int max_index = 0;
  unsigned int max_duplicate_count = 0;
  double sum = 0;
  prob_values[0] = exp(prob_values[0]);
  sum += prob_values[0];
  max_prob_value = prob_values[0];
  max_index = 0;
  for (unsigned int j=1; j<count; j++) {
    prob_values[j] = exp(prob_values[j]);
    //sum += prob_values[count];
    sum += prob_values[j];
#ifdef NBC_DEBUG
    if (debug_level > 2 || NBC_DEBUG > 2) {
      std::cout << classes[j] << " prob_value: " << prob_values[j] << std::endl;
    }
#endif
    if (max_prob_value == prob_values[j]) {
      max_duplicate_count++;
    } else if (max_prob_value < prob_values[j]) {
      max_prob_value = prob_values[j];
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
  if (count > 1) {
    if (prob_values[0] > prob_values[1]) {
      top1 = prob_values[0];
      top1_index = 0;
      top2 = prob_values[1];
      top2_index = 1;
    } else {
      top1 = prob_values[1];
      top1_index = 1;
      top2 = prob_values[0];
      top2_index = 0;
    }
  }
  if (count > 2) {
    top3 = prob_values[2];
    top3_index = 2;
    Heapify(top1, top1_index, top2, top2_index, top3, top3_index);
  }
  for (unsigned int j=3; j<count; j++) {
    prob_value = prob_values[j];
    if (prob_value > top3) {
      top3 = prob_value;
      top3_index = j;
      Heapify(top1, top1_index, top2, top2_index, top3, top3_index);
    }
  }

  top_classes_set.insert(classes[top1_index]);
  if (count > 1) {
    if (top2 > mean) {
      top_classes_set.insert(classes[top2_index]);
    }
  }
  if (count > 2) {
    if (top3 > mean) {
      top_classes_set.insert(classes[top3_index]);
    }
  }

#ifdef NBC_DEBUG
  if (debug_level > 1 || NBC_DEBUG > 1) {
    std::cout << "max prob_value: " << max_prob_value << std::endl;
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
#endif // NBC_DEBUG

  double score1 = exp(class1_freq);
  double score2 = exp(class2_freq);

#ifdef NBC_DEBUG
  if (m_debug_level > 1) {
    std::cout << "Class 1 score: " << score1 << std::endl;
    std::cout << "Class 2 score: " << score2 << std::endl;
  }
#endif // NBC_DEBUG

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

#if defined CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED
int NaiveBayesClassifier::PrepareNaiveBayes(std::string config_file_name,
                                            const bool& train_not_test,
                                            bool ignore_history) {

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified

  if (ClassifierConfig::Read(config_file_name.c_str(), m_config) < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  if (m_config.classes.empty()) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: class structs could not be read from config file: " \
              << config_file_name << std::endl;
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  CorpusMapMeta corpus_map_meta_data;

#ifdef CLASSIFIER_DEBUG
  if (m_debug_level > 4) {
    std::cout << "classifier training meta data\n";
  }
#endif // CLASSIFIER_DEBUG

  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {
    if (train_not_test) {
      corpus_map_meta_data[m_config.iter->name] = m_config.iter->training_corpus_file;
    } else {
      corpus_map_meta_data[m_config.iter->name] = m_config.iter->testing_corpus_file;
    }
#ifdef CLASSIFIER_DEBUG
    if (m_debug_level > 4) {
      std::cout << m_config.iter->name << " = " << corpus_map_meta_data[m_config.iter->name] << std::endl;
    }
#endif // CLASSIFIER_DEBUG
  }

  if (corpus_map_meta_data.empty()) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: corpus_map_meta_data cannot be empty\n";
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  if (!ignore_history) {
    if (CorpusManager::LoadCorpus(m_config.class_freqs_file,
                                  m_classes_freq_map) < 0) {
#ifdef CLASSIFIER_DEBUG
      std::cerr << "ERROR: could not load the text classes freq file (test data)\n";
      std::cout << "WARNING: continuing without the text classes freq data\n";
#endif // CLASSIFIER_DEBUG
    }
  } else {
#ifdef CLASSIFIER_DEBUG
    if (m_debug_level > 1) {
      std::cout << "INFO: ignoring historical data. plain vanilla classification\n";
    }
#endif // CLASSIFIER_DEBUG
  }

#ifdef CLASSIFIER_DEBUG
  if (m_debug_level > 1) {
    std::cout << "loading corpus_map from corpus_map_meta_data (while initializing classifier)\n";
  }
#endif // CLASSIFIER_DEBUG
  if (CorpusManager::LoadCorpusMap(corpus_map_meta_data, m_corpus_map) < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not load Corpus Map\n";
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  if (AddTaxonomy() < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cout << "ERROR: could not add taxonomy to the corpuses\n";
#endif // CLASSIFIER_DEBUG
  }

  return 0;
}

int NaiveBayesClassifier::AddTaxonomy() {

  Corpus taxon;
  CorpusIter taxon_iter;
  std::string taxon_name;

  CorpusMap* corpus_map = &(m_corpus_map);
  CorpusMapIter corpus_map_iter;

  Corpus* corpus_ptr;
  CorpusIter corpus_iter;

  std::string class_name;
  std::string word;
  double freq = 0;

  // pick the manual seed file for every class that has one
  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {

    if (m_config.iter->seed_file.empty()) {
      continue;
    }

    // load the seeds into a corpus
    if (CorpusManager::LoadCorpus(m_config.iter->seed_file,
                                  taxon,
                                  100) < 0) { // TODO (balaji) no hardcoding please
      std::cout << "ERROR: could not load seed file: " << m_config.iter->seed_file << " into corpus\n";
      taxon.clear();
      continue;
    }
    taxon_name = m_config.iter->name;

    if ((corpus_map_iter = corpus_map->find(taxon_name)) != corpus_map->end()) {
      // for this iteration, what is the class name and corpus?
      class_name = corpus_map_iter->first;
      corpus_ptr = &(corpus_map_iter->second);
      if (corpus_ptr->empty()) {
#ifdef NBC_DEBUG
        std::cout << "WARNING: no entries found in corpus for class: " << class_name << std::endl;
#endif // NBC_DEBUG
      }

      for (taxon_iter = taxon.begin();
           taxon_iter != taxon.end();
           taxon_iter++) {
        word = taxon_iter->first;
        if ((corpus_iter = corpus_ptr->find(word)) != corpus_ptr->end()) {
          freq = corpus_iter->second;
          if (freq < 100)
            corpus_iter->second = 100;
        } else {
          freq = 100;
          corpus_ptr->insert(std::pair<std::string, double> (word, freq));
        }
      }
    }

    taxon.clear();
  }

  return 0;
}

int NaiveBayesClassifier::Clear() {

  int ret_val=0;

  try {
    if (CorpusManager::ClearCorpus(m_classes_freq_map) < 0) {
      std::cerr << "ERROR: could not clear classes freq map\n";
      ret_val = -1;
    }
    if (CorpusManager::ClearCorpusMap(m_corpus_map) < 0) {
      std::cerr << "ERROR: could not clear corpus map\n";
      ret_val = -1;
    }
    if (ClassifierConfig::Clear(m_config) < 0) {
      std::cerr << "ERROR: could not clear classifier config\n";
      ret_val = -1;
    }
  } catch (...) {
    std::cerr << "EXCEPTION: thrown while clearing NaiveBayesClassifier\n";
  }

  return ret_val;
}

int NaiveBayesClassifier::GenerateProbabilities(const bool& train_not_test) {

  CorpusMap* corpus_map = &(m_corpus_map);

  std::set<std::string> vocabulary_set;
  Corpus* corpus_ptr;
  CorpusMapIter corpus_map_iter;
  CorpusIter corpus_iter;
  std::string class_name;
  std::string word;

  for (corpus_map_iter = corpus_map->begin(); corpus_map_iter != corpus_map->end(); corpus_map_iter++) {

    // for this iteration, what is the class name and corpus?
    class_name = corpus_map_iter->first;
    corpus_ptr = &(corpus_map_iter->second);
    if (corpus_ptr->empty()) {
#ifdef NBC_DEBUG
      std::cout << "WARNING: no entries found in corpus for class: " << class_name << std::endl;
#endif // NBC_DEBUG
      continue;
    }

    for (corpus_iter = corpus_ptr->begin();
         corpus_iter != corpus_ptr->end();
         corpus_iter++) {
#ifdef CLASSIFIER_DEBUG
      if (CLASSIFIER_DEBUG > 3) {
          std::cout << (*corpus_iter).first << " : " << (*corpus_iter).second \
                    << " in " << class_name << std::endl;
      }
#endif // CLASSIFIER_DEBUG
      word = (*corpus_iter).first;
      // freq = (*corpus_iter).second;
      vocabulary_set.insert(word);
    }
  }

  // this is the vocabulary size. the B in the denominator. see Raghavan et al.
  unsigned int vocabulary_size = vocabulary_set.size();

  // TODO (balaji)
  // the above loop over corpus map is unavoidable becos we need the vocabulary. if needed find this in GetData

  std::string data_file;
  std::string class_number_str;
  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {

    if (train_not_test) {
      data_file = m_config.iter->training_data_file;
    } else {
      data_file = m_config.iter->testing_data_file;
    }

    if ((corpus_map_iter = corpus_map->find(m_config.iter->name)) == corpus_map->end()) {
      std::cerr << "ERROR: corpus map not found for class: " << m_config.iter->name << std::endl;
      break;
    }

    if (GenerateProbabilities(&(corpus_map_iter->second),
                              data_file.c_str(),
                              vocabulary_size) < 0) {
#ifdef CLASSIFIER_DEBUG
      std::cerr << "ERROR - could not normalize frequencies for: " << corpus_file << std::endl;
#endif // CLASSIFIER_DEBUG
      break;
    }
  }

  return 0;
}

int NaiveBayesClassifier::GenerateProbabilities(Corpus* raw_data_corpus,
                                                const char* probabilities_file,
                                                const unsigned int& vocabulary_size) {

  if (!probabilities_file) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: invalid probabilities file\n";
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  if (raw_data_corpus->size() < 2) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "WARNING: empty raw data corpus\n";
#endif // CLASSIFIER_DEBUG
    return 0;
  }

  // refer to naive bayes as mentioned by Raghavan et al.
  // (Tct + 1) / (sum over Tct-dash + B)
  // Tct - term count for a word
  // +1 - add-one or Laplace smoothing
  // B - vocabulary size. count of words in all classes ignoring duplicates.
  // tct-dash - seems to be the count of all words present the in this class including duplicates.
  CorpusIter corpus_iter;
  double corpus_count = 0;
  for (corpus_iter = raw_data_corpus->begin(); corpus_iter != raw_data_corpus->end(); corpus_iter++) {
    corpus_count += corpus_iter->second;
  }

  double normalizing_denominator = corpus_count + (double) vocabulary_size;
  if (normalizing_denominator <= 1) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "WARNING: invalid normalizing denominator\n";
#endif // CLASSIFIER_DEBUG
    return 0;
  }

  Corpus probabilities_corpus;
  for (corpus_iter = raw_data_corpus->begin(); corpus_iter != raw_data_corpus->end(); corpus_iter++) {
    probabilities_corpus[corpus_iter->first] = log((corpus_iter->second + 1)/ normalizing_denominator);
  }
  

/*
  std::set<std::string>::iterator vocabulary_iter;
  std::string word;
  double laplace_smoothing = log(1/normalizing_denominator);
  for (vocabulary_iter = vocabulary_set.begin();
       vocabulary_iter != vocabulary_set.end();
       vocabulary_iter++) {
    word = *vocabulary_iter;
    if (raw_data_corpus->find(word) == raw_data_corpus->end()) { // only when the word is not found
      probabilities_corpus[word] = laplace_smoothing;
    }
  }
*/

  // for each word that is not present in this corpus, there needs to be a probability.
  // thats why we do laplace smoothing.
  // now, we can run over the vocabulary and assign laplace_smoothing value to all the non-class words (see above)
  // however, why not just calculate this and store it once with "laplace_smoothing_<class_number>" as key?
  std::string laplace_smoothing_key = "laplace_smoothing";
  double laplace_smoothing_value = log(1.0/normalizing_denominator);
  probabilities_corpus[laplace_smoothing_key] = laplace_smoothing_value;

  if (CorpusManager::WriteCorpusToFile(probabilities_corpus, probabilities_file) < 0) {
    probabilities_corpus.clear();
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR - could not write probabilites to " \
              << relative_freq_file << std::endl;
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  probabilities_corpus.clear();

  return 0;
}

int NaiveBayesClassifier::MakePriorProbabilitiesFile(const char* classifier_prior_freqs_file) {

  if (!classifier_prior_freqs_file) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  if (m_classes_freq_map.empty()) {
    std::cerr << "ERROR: classes freq map empty\n";
    return -1;
  }

  Corpus* classes_freq_map = &(m_classes_freq_map);

  CorpusIter corpus_iter;
  double prior_total_entries = 0;
  if ((corpus_iter = classes_freq_map->find("all_classes")) != classes_freq_map->end()) {
    prior_total_entries = (*corpus_iter).second;
  } else {
    return -1;
  }

  if (prior_total_entries <= 0) {
    std::cerr << "ERROR: invalid prior total entries\n";
    return -1;
  }

  std::ofstream ofs(classifier_prior_freqs_file);
  if (!ofs.is_open()) {
    std::cerr << "ERROR: could not open classifier prior freqs file: " \
              << classifier_prior_freqs_file << std::endl;
    return -1;
  }
  std::string class_name;
  double class_freq = 0;
  for (corpus_iter = classes_freq_map->begin();
       corpus_iter != classes_freq_map->end();
       corpus_iter++) {
    class_name = corpus_iter->first;
    class_freq = corpus_iter->second;
    if (class_name.compare("all_classes") == 0)
      continue;
    ofs << class_name << "=" << log(class_freq/prior_total_entries) << std::endl;
  }
  ofs.close();

  return 0;
}
#endif // CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED

}
