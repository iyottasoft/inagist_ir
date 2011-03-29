#include "text_classifier.h"
#include <iostream>
#include <fstream>
#include "twitter_searcher.h"
#include "string_utils.h"
#include "config_reader.h"
#include "naive_bayes_classifier.h"

#ifdef DEBUG
#if DEBUG>0
#define TC_DEBUG DEBUG
#endif
#endif
//#define TC_DEBUG 0

namespace inagist_classifiers {

TextClassifier::TextClassifier() {
#ifdef TC_DEBUG
  m_debug_level = TC_DEBUG;
#else
  m_debug_level = 0;
#endif
}

TextClassifier::~TextClassifier() {
  Clear();
}

int TextClassifier::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  return 0;
}

int TextClassifier::Init(std::string config_file_name) {

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified

  inagist_classifiers::Config config;
  if (inagist_classifiers::ConfigReader::Read(config_file_name.c_str(), config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
    return -1;
  }

  if (config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " << config_file_name << std::endl;
    return -1;
  }

  std::map<std::string, std::string> text_class_map;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    text_class_map[config.iter->name] = config.iter->training_data_file;
  }

  if (m_corpus_manager.LoadCorpus(config.test_data_file,
                                  m_corpus_manager.m_classes_freq_map) < 0) {
    std::cerr << "ERROR: could not load the text classes freq file (test data)\n";
    std::cout << "WARNING: continuing with the text classes freq data\n";
  }

  inagist_classifiers::ConfigReader::Clear(config);

  if (!text_class_map.empty()) {
    if (m_corpus_manager.LoadCorpusMap(text_class_map) < 0) {
      std::cerr << "ERROR: could not load Corpus Map\n";
      return -1;
    }
  }
  text_class_map.clear();

  return 0;
}
 
int TextClassifier::GuessClass(const std::string& text,
                               const unsigned int& text_len,
                               std::string& text_class,
                               bool ignore_case) {
  int num_words = 0;
  Corpus test_corpus;

  if ((num_words = GetCorpus(text, test_corpus)) < 0) {
    std::cerr << "ERROR: no words found" << std::endl;
    return -1;
  }

  if (num_words == 0) {
#ifdef TC_DEBUG
    if (m_debug_level > 0)
      std::cout << "no ngrams found for ... \n" << text << std::endl;
#endif
    text_class.assign("RR");
    return 0;
  }

#ifdef TC_DEBUG
  if (m_debug_level > 1)
    std::cout << "now guessing class for ... \n" << text << std::endl;
#endif

  if (NaiveBayesClassifier::GuessClass(m_corpus_manager.m_corpus_map,
                                       m_corpus_manager.m_classes_freq_map,
                                       test_corpus,
                                       text_class) < 0) {
    std::cout << "ERROR: naive bayes classifier could not guess the text class\n";
    test_corpus.clear();
    return -1;
  }

#ifdef TC_DEBUG
  if (m_debug_level > 1)
    std::cout << "guess_class: " << text_class << std::endl;
#endif

  test_corpus.clear();

  return 1;
}

int TextClassifier::GuessClass(std::set<std::string>& words_set,
                                     std::string& text_class,
                                     bool ignore_case) {

  if (words_set.size() <= 0) {
    text_class.assign("RR");
    return 0;
  }

  Corpus test_corpus;

  std::set<std::string>::iterator set_iter;
  for (set_iter = words_set.begin(); set_iter != words_set.end(); set_iter++) {
    test_corpus[*set_iter] = 1;
  }

  if (NaiveBayesClassifier::GuessClass(m_corpus_manager.m_corpus_map,
                                       m_corpus_manager.m_classes_freq_map,
                                       test_corpus,
                                       text_class) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the text class\n";
    test_corpus.clear();
    return -1;
  }

#ifdef TC_DEBUG
  if (m_debug_level > 1)
    std::cout << "guess_class: " << text_class << std::endl;
#endif

  test_corpus.clear();

  return 1;
}

int TextClassifier::GetWordFrequencies(const std::string& input_file_name,
                                          Corpus& corpus) {

  std::ifstream ifs(input_file_name.c_str());
  if (!ifs) {
    std::cout << "ERROR: could not open file " << input_file_name << std::endl;
    return -1;
  }

  std::string line;
  int num_docs = 0;
  while (getline(ifs, line)) {
    if (GetCorpus(line, corpus) < 0) {
      std::cout << "ERROR: could not get corpus from text: " << line << std::endl;
    } else {
      num_docs++;
    }
  }
  ifs.close();

  return corpus.size();
}

int TextClassifier::GetCorpus(const std::string& text, Corpus& corpus) {
  int words_count = 0;
  std::set<std::string> words_set;
  if ((words_count = inagist_utils::Tokenize(text, words_set)) < 0) {
    std::cerr << "ERROR: could not tokenize: " << text << std::endl;
    return -1;
  }

  std::set<std::string>::iterator set_iter;
  for (set_iter = words_set.begin(); set_iter != words_set.end(); set_iter++) {
    corpus[*set_iter] = 1;
  }

  return words_count;
}

int TextClassifier::Clear() {
  try {
    m_corpus_manager.Clear();
  } catch (...) {
    std::cerr << "ERROR: Corpus Manager throws exception" << std::endl;
  }
  return 0;
}

} // namespace inagist_classifiers

