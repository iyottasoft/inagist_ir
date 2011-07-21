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
#define TC_DEBUG 0

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

  std::set<std::string> top_classes_set;
  if (NaiveBayesClassifier::GuessClass2(m_corpus_manager.m_corpus_map,
                                        m_corpus_manager.m_classes_freq_map,
                                        test_corpus,
                                        text_class,
                                        top_classes_set
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                        , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                       ) < 0) {
    top_classes_count = 0;
    test_corpus.clear();
#ifdef TC_DEBUG
    std::cout << "ERROR: naive bayes classifier could not guess the text class\n";
    text_class.assign("RR");
    top_classes.assign("RR");
    top_classes_count = 1;
#endif // TC_DEBUG
    return -1;
  }

  if (m_class_labels_map.empty()) {
#ifdef TC_DEBUG
    std::cerr << "ERROR: class labels map empty\n";
    top_classes.assign("RR|");
    top_classes_count = 1;
#endif // TC_DEBUG
    return -1;
  }

  std::set<std::string>::iterator name_set_iter;
  std::map<std::string, std::string>::iterator label_map_iter;
  for (name_set_iter = top_classes_set.begin();
       name_set_iter != top_classes_set.end();
       name_set_iter++) {
    if ((label_map_iter = m_class_labels_map.find(*name_set_iter)) != m_class_labels_map.end()) {
      top_classes += label_map_iter->second;
    } else {
      top_classes += *name_set_iter; 
      std::cerr << "ERROR: no class label found for: " << *name_set_iter << std::endl;
    }
    top_classes += "|";
    top_classes_count++;
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

  if (list_len <= 0 ||
      word_count <= 0) {
#ifdef TC_DEBUG
    std::cout << "WARNING: empty word list. no classification done\n";
#endif
    return 0;
  }

  if (!text_word_list) {
    std::cerr << "ERROR: invalid input text word list\n";
    return -1;
  }

  if (!guess_text_class_buffer ||
      !top_classes_buffer) {
    std::cerr << "ERROR: invalid input text class buffers\n";
    return -1;
  }

#ifdef CLASS_CONTRIBUTORS_ENABLED
  if (!class_contributors_buffer) {
    std::cerr << "ERROR: invalid class contributors buffer\n";
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
  std::set<std::string> top_classes_set;
  std::set<std::string> top_classes_labels_set;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED
  if (NaiveBayesClassifier::GuessClass2(m_corpus_manager.m_corpus_map,
                                        m_corpus_manager.m_classes_freq_map,
                                        test_corpus,
                                        text_class,
                                        top_classes_set
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                        , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                       ) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the text class\n";
    test_corpus.clear();
    top_classes_len = top_classes.length();
    return -1;
  }
  test_corpus.clear();

  // text class
#ifdef TC_DEBUG
  if (m_debug_level > 1) {
    std::cout << "guess_class: " << text_class << std::endl;
  }
#endif // TC_DEBUG
  strcpy(guess_text_class_buffer, text_class.c_str());
  text_class.clear();

  // top classes
  std::map<std::string, std::string>::iterator label_map_iter;
  if (m_class_labels_map.empty()) {
    top_classes_set.clear();
#ifdef TC_DEBUG
    std::cerr << "ERROR: class labels map empty\n";
    strcpy(top_classes_buffer, "RR|");
    top_classes_count = 1;
    top_classes_len = 3; 
#endif // TC_DEBUG
    return -1;
  }

  top_classes_len = 0;
  top_classes_count = 0;
  char* ptr = top_classes_buffer;
  std::set<std::string>::iterator set_iter;
  std::string element;
  for (set_iter = top_classes_set.begin(); set_iter != top_classes_set.end(); set_iter++) {
    if ((label_map_iter = m_class_labels_map.find(*set_iter)) != m_class_labels_map.end()) {
      element.assign(label_map_iter->second);
    } else {
      element.assign(*set_iter); 
    }
    strcpy(ptr, element.c_str()); 
    ptr += element.length();
    strcpy(ptr, "|");
    ptr += 1;
    top_classes_count++;
  }
  top_classes_len = ptr - top_classes_buffer;
  ptr = NULL;
  top_classes_set.clear();
  element.clear();

  // class contributors
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
  std::set<std::string> top_classes_set;
  if ((ret_value = NaiveBayesClassifier::GuessClass2(m_corpus_manager.m_corpus_map,
                                       m_corpus_manager.m_classes_freq_map,
                                       corpus,
                                       text_class,
                                       top_classes_set
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                       , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                      )) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the text class\n";
    return -1;
  }

  if (m_class_labels_map.empty()) {
#ifdef TC_DEBUG
    std::cerr << "ERROR: class labels map empty\n";
    top_classes.assign("RR|");
    top_classes_count = 1;
#endif // TC_DEBUG
    return -1;
  }

  std::set<std::string>::iterator name_set_iter;
  std::map<std::string, std::string>::iterator label_map_iter;
  for (name_set_iter = top_classes_set.begin();
       name_set_iter != top_classes_set.end();
       name_set_iter++) {
    if ((label_map_iter = m_class_labels_map.find(*name_set_iter)) != m_class_labels_map.end()) {
      top_classes += label_map_iter->second;
    } else {
      top_classes += *name_set_iter; 
      std::cerr << "no class label found for: " << *name_set_iter << std::endl;
    }
    top_classes += "|";
    top_classes_count++;
  }
  top_classes_set.clear();

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

#ifdef KEYWORDS_ENABLED
  std::set<std::string> keywords_set;
#endif // KEYWORDS_ENABLED
#ifdef HASHTAGS_ENABLED
  std::set<std::string> hashtags_set;
#endif // HASHTAGS_ENABLED
#ifdef KEYPHRASE_ENABLED
  std::set<std::string> keyphrases_set;
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
  std::set<std::string> lang_words_set;
#endif // LANG_ENABLED
#ifdef INTENT_ENABLED
  std::string intent_words;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  std::string sentiment;
#endif // SENTIMENT_ENABLED

  std::set<std::string> text_class_words_set;

  if (m_keytuples_extracter.GetKeyTuples(buffer,
                                         safe_status,
                                         script
#ifdef KEYWORDS_ENABLED
                                         , keywords_set
#endif // KEYWORDS_ENABLED
#ifdef HASHTAGS_ENABLED
                                         , hashtags_set
#endif // HASHTAGS_ENABLED
#ifdef KEYPHRASE_ENABLED
                                         , keyphrases_set
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                                         , lang_words_set
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                                         , text_class_words_set
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                                         , intent_words
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                                         , sentiment
#endif // SENTIMENT_ENABLED
                                        ) < 0) {
    std::cerr << "ERROR: could not get words for: " << text << std::endl;
    return -1;
  }

#ifdef KEYWORDS_ENABLED
  keywords_set.clear();
#endif // KEYWORDS_ENABLED
#ifdef HASHTAGS_ENABLED
  hashtags_set.clear();
#endif // HASHTAGS_ENABLED
#ifdef KEYPHRASE_ENABLED
  keyphrases_set.clear();
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
  lang_words_set.clear();
#endif // LANG_ENABLED

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

