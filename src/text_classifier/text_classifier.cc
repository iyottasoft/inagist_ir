#include "text_classifier.h"
#include <iostream>
#include <fstream>
#include <cstring>
#include <cstdlib>
#include "string_utils.h"
#include "classifier_config.h"
#include "naive_bayes_classifier.h"

#ifdef DEBUG
#if DEBUG>0
#define TC_DEBUG DEBUG
#endif
#endif
//#define TC_DEBUG 5

#define MAX_BUFFER_LEN 1024

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

  const char* keytuples_config_file = argv[0];
  if (!keytuples_config_file || strlen(keytuples_config_file) < 4) {
    std::cerr << "ERROR: invalid keytuples config file name\n";
  }

  if (m_keytuples_extracter.Init(keytuples_config_file) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter from file:" \
              << keytuples_config_file << std::endl;
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

int TextClassifier::Classify(const std::string& text, const unsigned int& text_len,
                             std::string& text_class,
                             std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                             , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                             , bool ignore_case) {
  int num_words = 0;
  Corpus test_corpus;

  if ((num_words = GetCorpus(text, test_corpus)) < 0) {
    std::cerr << "ERROR: no words found" << std::endl;
    return -1;
  }

  if (num_words == 0) {
    top_classes_count = 0;
#ifdef TC_DEBUG
    if (m_debug_level > 0)
      std::cout << "no ngrams found for ... \n" << text << std::endl;
    text_class.assign("RR");
    top_classes.assign("RR");
    top_classes_count = 1;
#endif
    return 0;
  }

#ifdef TC_DEBUG
  if (m_debug_level > 1)
    std::cout << "now guessing class for ... \n" << text << std::endl;
#endif

  if (NaiveBayesClassifier::GuessClass2(m_corpus_manager.m_corpus_map,
                                        m_corpus_manager.m_classes_freq_map,
                                        test_corpus,
                                        text_class,
                                        top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                        , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                       ) < 0) {
    std::cout << "ERROR: naive bayes classifier could not guess the text class\n";
    test_corpus.clear();
    top_classes_count = 0;
#ifdef TC_DEBUG
    text_class.assign("RR");
    top_classes.assign("RR");
    top_classes_count = 1;
#endif
    return -1;
  }

#ifdef TC_DEBUG
  if (m_debug_level > 1) {
    std::cout << "guess_class: " << text_class << std::endl;
    std::cout << "top_classes: " << top_classes << std::endl;
  }
#endif

  test_corpus.clear();

  return 1;
}

int TextClassifier::Classify(const unsigned char* text_word_list,
                             const unsigned int& list_len,
                             const unsigned int& word_count, 
                             char* guess_text_class_buffer,
                             const unsigned int& guess_text_class_buffer_len,
                             char* top_classes_buffer,
                             const unsigned int& top_classes_buffer_len,
                             unsigned int& top_classes_len,
                             unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                             , unsigned char* class_contributors_buffer,
                             const unsigned int& class_contributors_buffer_len,
                             unsigned int& class_contributors_len,
                             unsigned int& class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
                             , bool ignore_case) {

  if (!text_word_list || list_len <= 0 || word_count <= 0) {
    return -1;
  }

#ifdef CLASS_CONTRIBUTORS_ENABLED
  if (!class_contributors_buffer) {
    return -1;
  }
  class_contributors_len = 0;
  class_contributors_count = 0;
  class_contributors_buffer[0] = '\0';
#endif // CLASS_CONTRIBUTORS_ENABLED

  Corpus test_corpus;

  unsigned char* start = (unsigned char*) text_word_list;
  unsigned char* end = (unsigned char*) strchr((char *) start, '|');
  unsigned int word_len = 0;
  std::string word;
  while (start && end && *start != '\0') {
    word_len = end - start; 
    word.clear();
    word.assign((char *) start, word_len);
    test_corpus[word] = 1;
    start = end + 1;
    end = (unsigned char*) strchr((char *) start, '|');
  }
  start = NULL;
  end = NULL;

  if (test_corpus.empty()) {
    return 0;
  }

  std::string text_class;
  std::string top_classes;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED
  if (NaiveBayesClassifier::GuessClass2(m_corpus_manager.m_corpus_map,
                                        m_corpus_manager.m_classes_freq_map,
                                        test_corpus,
                                        text_class,
                                        top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                        , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                       ) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the text class\n";
    test_corpus.clear();
    top_classes_len = top_classes.length();
    return -1;
  }

#ifdef TC_DEBUG
  if (m_debug_level > 1) {
    std::cout << "guess_class: " << text_class << std::endl;
    std::cout << "top_classes: " << top_classes << std::endl;
  }
#endif

  test_corpus.clear();
  strcpy(guess_text_class_buffer, text_class.c_str());
  strcpy(top_classes_buffer, top_classes.c_str());
  top_classes_len = top_classes.length();
#ifdef CLASS_CONTRIBUTORS_ENABLED
  if (class_contributors_map.empty()) {
#ifdef TC_DEBUG
      std::cerr << "WARNING: no class_contributors found\n";
#endif
  } else {
    if (inagist_utils::StringMapToPipeList(class_contributors_map,
                            class_contributors_buffer, class_contributors_buffer_len,
                            class_contributors_len, class_contributors_count) < 0) {
#ifdef TC_DEBUG
      std::cerr << "ERROR: could not make a list of class_contributors\n";
#endif
    }
    class_contributors_map.clear();
  }
#endif // CLASS_CONTRIBUTORS_ENABLED

  return 1;
}

int TextClassifier::Classify(std::set<std::string>& words_set,
                             std::string& text_class,
                             std::string& top_classes,
                             unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                             , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                             , bool ignore_case) {

  if (words_set.size() <= 0) {
    top_classes_count = 0;
#ifdef TC_DEBUG
    text_class.assign("RR");
    top_classes.assign("RR");
    top_classes_count = 1;
#endif
    return 0;
  }

  Corpus test_corpus;

  std::set<std::string>::iterator set_iter;
  for (set_iter = words_set.begin(); set_iter != words_set.end(); set_iter++) {
    test_corpus[*set_iter] = 1;
  }

  int ret_value = 0;
  if ((ret_value = Classify(test_corpus,
                            text_class,
                            top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                             , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                           )) < 0) {
    std::cerr << "ERROR: could not classify the given corpus\n";
  }

  test_corpus.clear();

  return ret_value;
}

int TextClassifier::Classify(Corpus& corpus,
                             std::string& text_class,
                             std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                             , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                            ) {

  int ret_value = 0;
  if ((ret_value = NaiveBayesClassifier::GuessClass2(m_corpus_manager.m_corpus_map,
                                       m_corpus_manager.m_classes_freq_map,
                                       corpus,
                                       text_class,
                                       top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                       , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                      )) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the text class\n";
    return -1;
  }

#ifdef TC_DEBUG
  if (m_debug_level > 1) {
    std::cout << "guess_class: " << text_class << std::endl;
    std::cout << "top_classes: " << top_classes << std::endl;
  }
#endif

  return ret_value;
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
  std::string script;
  std::string safe_status;

#ifndef KEYWORDS_DISABLED
  std::set<std::string> keywords_set;
#endif // KEYWORDS_DISABLED
#ifdef HASHTAGS_ENABLED
  std::set<std::string> hashtags_set;
#endif // HASHTAGS_ENABLED
#ifdef KEYPHRASE_ENABLED
  std::set<std::string> keyphrases_set;
#endif // KEYPHRASE_ENABLED
#ifdef LANG_WORDS_ENABLED
  std::set<std::string> lang_words_set;
#endif // LANG_WORDS_ENABLED

  std::set<std::string> text_class_words_set;

  if (m_keytuples_extracter.GetKeyTuples(buffer,
                                         safe_status,
                                         script
#ifndef KEYWORDS_DISABLED
                                         , keywords_set
#endif // KEYWORDS_DISABLED
#ifdef HASHTAGS_ENABLED
                                         , hashtags_set
#endif // HASHTAGS_ENABLED
#ifdef KEYPHRASE_ENABLED
                                         , keyphrases_set
#endif // KEYPHRASE_ENABLED
#ifdef LANG_WORDS_ENABLED
                                         , lang_words_set
#endif // LANG_WORDS_ENABLED
                                         , text_class_words_set
                                        ) < 0) {
    std::cerr << "ERROR: could not get words for: " << text << std::endl;
    return -1;
  }

#ifndef KEYWORDS_DISABLED
  keywords_set.clear();
#endif // KEYWORDS_DISABLED
#ifdef HASHTAGS_ENABLED
  hashtags_set.clear();
#endif // HASHTAGS_ENABLED
#ifdef KEYPHRASE_ENABLED
  keyphrases_set.clear();
#endif // KEYPHRASE_ENABLED
#ifdef LANG_WORDS_ENABLED
  lang_words_set.clear();
#endif // LANG_WORDS_ENABLED

  if (script.compare(0, 2, "en") != 0) {
#ifndef TC_DEBUG
    // std::cout << buffer << "\nnon-english tweet. no keytuples." << std::endl;
#endif
    return 0;
  }

  std::set<std::string>::iterator set_iter;
  std::set<std::string> words;
  std::set<std::string>::iterator words_iter;
  std::set<std::string> corpus_set;

  if (text_class_words_set.size() > 0) {
#ifdef TC_DEBUG
    std::cout << "text_class_words:\n";
#endif
    for (set_iter = text_class_words_set.begin(); set_iter != text_class_words_set.end(); set_iter++) {
      corpus_set.insert(*set_iter);
#ifdef TC_DEBUG
      std::cout << *set_iter << std::endl;
#endif
    }
    text_class_words_set.clear();
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

