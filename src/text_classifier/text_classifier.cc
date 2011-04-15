#include "text_classifier.h"
#include <iostream>
#include <fstream>
#include <cstring>
#include "twitter_searcher.h"
#include "string_utils.h"
#include "config_reader.h"
#include "naive_bayes_classifier.h"

#ifdef DEBUG
#if DEBUG>0
#define TC_DEBUG DEBUG
#endif
#endif
//#define TC_DEBUG 5

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

int TextClassifier::InitDependencies(int argc, char* argv[]) {

  if (argc != 1) {
    std::cerr << "ERROR: text classifier needs keytuples config file\n";
    return -1;
  }

  const char* keytuples_config_file = argv[1];

  if (m_keytuples_extracter.Init(keytuples_config_file) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
    return -1;
  }

  return 0;
} 
// 
int TextClassifier::InitTraining(const char* stopwords_file,
                                 const char* dictionary_file,
                                 const char* unsafe_dictionary_file) {

  if (m_keytuples_extracter.Init(stopwords_file,
                                 dictionary_file,
                                 unsafe_dictionary_file) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
    return -1;
  }

  return 0;
}

int TextClassifier::LoadKeyTuplesDictionary(const char* dictionary_file) {

  m_keytuples_extracter.m_dictionary.Clear();

  if (m_keytuples_extracter.m_dictionary.Load(dictionary_file) < 0) {
    std::cout << "ERROR: couldn't load dictionary file: " << dictionary_file << std::endl;
  }

  return 0;
}

int TextClassifier::Classify(const std::string& text,
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

int TextClassifier::Classify(std::set<std::string>& words_set,
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

  if (text.length() < 1) {
    std::cerr << "ERROR: empty string. cannot get corpus\n";
    return -1;
  }

  char buffer[MAX_BUFFER_LEN];
  strcpy((char *) buffer, text.c_str());

  std::set<std::string> keywords_set;
  std::set<std::string> hashtags_set;
  std::set<std::string> keyphrases_set;
  std::string safe_status;
  std::string script;
  std::string lang;
  if (m_keytuples_extracter.GetKeyTuples(buffer,
                                         safe_status, script, lang,
                                         keywords_set, hashtags_set, keyphrases_set) < 0) {
    std::cerr << "ERROR: could not get words for: " << text << std::endl;
    return -1;
  }

  if ((script.compare(0, 2, "en")) != 0) {
     // || (lang.compare(0, 2, "en")) != 0) {
#ifndef TC_DEBUG
    // std::cout << buffer << "\nnon-english tweet. no keytuples." << std::endl;
#endif
    keyphrases_set.clear();
    keywords_set.clear();
    hashtags_set.clear();
    return 0;
  }

  std::set<std::string>::iterator set_iter;
  std::set<std::string> words;
  std::set<std::string>::iterator words_iter;
  std::set<std::string> corpus_set;

  if (keywords_set.size() > 0) {
#ifdef TC_DEBUG
    std::cout << "keywords:\n";
#endif
    for (set_iter = keywords_set.begin(); set_iter != keywords_set.end(); set_iter++) {
      corpus_set.insert(*set_iter);
#ifdef TC_DEBUG
      std::cout << *set_iter << std::endl;
#endif
/*
      if (inagist_utils::Tokenize(*set_iter, words) > 1) {
        for (words_iter = words.begin(); words_iter != words.end(); words_iter++) {
          corpus_set.insert(*words_iter);
        }
      }
*/
    }
    keywords_set.clear();
  }
  if (hashtags_set.size() > 0) {
#ifdef TC_DEBUG
    std::cout << "hashtags:\n";
#endif
    for (set_iter = hashtags_set.begin(); set_iter != hashtags_set.end(); set_iter++) {
      corpus_set.insert(*set_iter);
#ifdef TC_DEBUG
      std::cout << *set_iter << std::endl;
#endif
/*
      if (inagist_utils::Tokenize(*set_iter, words) > 1) {
        for (words_iter = words.begin(); words_iter != words.end(); words_iter++) {
          corpus_set.insert(*words_iter);
        }
      }
*/
    }
    hashtags_set.clear();
  }
  if (keyphrases_set.size() > 0) {
#ifdef TC_DEBUG
    std::cout << "keyphrases:\n";
#endif
    for (set_iter = keyphrases_set.begin(); set_iter != keyphrases_set.end(); set_iter++) {
      corpus_set.insert(*set_iter);
#ifdef TC_DEBUG
      std::cout << *set_iter << std::endl;
#endif
/*
      if (inagist_utils::Tokenize(*set_iter, words) > 1) {
        for (words_iter = words.begin(); words_iter != words.end(); words_iter++) {
          corpus_set.insert(*words_iter);
        }
      }
*/
    }
    keyphrases_set.clear();
  }
  buffer[0] = '\0';

  int words_count = corpus_set.size();
  for (set_iter = corpus_set.begin(); set_iter != corpus_set.end(); set_iter++) {
    corpus[*set_iter] = 1;
  }
  corpus_set.clear();

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

int TextClassifier::ClearDependencies() {
  try {
    m_keytuples_extracter.DeInit();
  } catch (...) {
    std::cerr << "ERROR: KeyTuples Extracter throws exception" << std::endl;
  }
  return 0;
}

} // namespace inagist_classifiers

