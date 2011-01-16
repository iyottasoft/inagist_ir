#include "naive_bayes_classifier.h"
#include <iostream>
#include <cmath>

#define NBC_DEBUG 3

namespace inagist_classifiers {

NaiveBayesClassifier::NaiveBayesClassifier() {
}

NaiveBayesClassifier::~NaiveBayesClassifier() {
}

int NaiveBayesClassifier::GuessClass(CorpusMap& corpus_map,
                                     Corpus& test_corpus,
                                     std::string& guess_lang_output) {

  CorpusMapIter corpus_map_iter;
  CorpusIter test_corpus_iter;
  CorpusIter corpus_iter;
  CorpusIter loc;

  double freqs[MAX_CORPUS_NUMBER];
  double temp_freq = 0;
  unsigned int i = 0;
  std::string langs[MAX_CORPUS_NUMBER];
 
  for (corpus_map_iter = corpus_map.begin(); corpus_map_iter != corpus_map.end(); corpus_map_iter++) {
    langs[i] = (*corpus_map_iter).first;
    for (test_corpus_iter = test_corpus.begin(); test_corpus_iter != test_corpus.end(); test_corpus_iter++) {
      corpus_iter = ((*corpus_map_iter).second).find((*test_corpus_iter).first); 
      if (corpus_iter != ((*corpus_map_iter).second).end()) {
        temp_freq = (double) (*corpus_iter).second;
        freqs[i] = log(temp_freq);
      } else {
        freqs[i] = 0;
      }
    }
    i++;
  }

  double max_freq = 0;
  unsigned int max_index = 0;
  unsigned int max_duplicate_count = 0;
  for (unsigned int j=0; j<i; j++) {
    freqs[j] = exp(freqs[j]);
#ifdef NBC_DEBUG
    if (NBC_DEBUG > 2) {
      std::cout << "freqs: " << freqs[j] << std::endl;
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
  if (NBC_DEBUG > 1) {
    std::cout << "max freq: " << max_freq << std::endl;
    std::cout << "max index: " << max_index << std::endl;
    std::cout << "guess_lang_output: " << langs[max_index] << std::endl;
    std::cout << "max duplicate count: " << max_duplicate_count << std::endl;
  }
#endif

  if (0 == max_duplicate_count)
    guess_lang_output = langs[max_index];
  else
    guess_lang_output = "xx";

  return 0;
}

int NaiveBayesClassifier::GuessClass(std::map<std::string, int> testfile_features_map,
                                 std::map<std::string, int> lang1_features_map,
                                 std::map<std::string, int> lang2_features_map) {

  double lang1_freq = 0;
  double lang2_freq = 0;
  double freq = 0;
  std::map<std::string, int>::iterator map_iter;
  std::map<std::string, int>::iterator freq_iter;
  for (map_iter = testfile_features_map.begin(); map_iter != testfile_features_map.end(); map_iter++) {
    //std::cout << (*map_iter).first << " = " << (*map_iter).second << std::endl;
    freq_iter = lang1_features_map.find((*map_iter).first); 
    if (freq_iter != lang1_features_map.end()) {
       freq = (double) (*freq_iter).second;
       //std::cout << freq << std::endl;
       lang1_freq += log(freq); 
    }
    freq_iter = lang2_features_map.find((*map_iter).first); 
    if (freq_iter != lang2_features_map.end()) {
       freq = (double) (*freq_iter).second;
       //std::cout << freq << std::endl;
       lang2_freq += log(freq); 
    }
  }

#ifdef NBC_DEBUG
  if (NBC_DEBUG > 1) {
    std::cout << "Lang 1 freq: " << lang1_freq << std::endl;
    std::cout << "Lang 2 freq: " << lang2_freq << std::endl;
  }
#endif

  double score1 = exp(lang1_freq);
  double score2 = exp(lang2_freq);

#ifdef NBC_DEBUG
  if (NBC_DEBUG > 1) {
    std::cout << "Lang 1 score: " << score1 << std::endl;
    std::cout << "Lang 2 score: " << score2 << std::endl;
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
