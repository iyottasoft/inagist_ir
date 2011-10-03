#include "language_classifier.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include "string_utils.h"
#include "classifier_config.h"

#ifdef DEBUG
#if DEBUG>0
#define LD_DEBUG DEBUG
#endif
#endif
//#define LD_DEBUG 5

#define MAX_BUFFER_LEN 1024

namespace inagist_classifiers {

LanguageClassifier::LanguageClassifier() {
#ifdef LD_DEBUG
  m_debug_level = LD_DEBUG;
#else
  m_debug_level = 0;
#endif
}

LanguageClassifier::~LanguageClassifier() {
  Clear();
}

int LanguageClassifier::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  m_ngrams_generator.SetDebugLevel(debug_level);
  return 0;
}
 
int LanguageClassifier::Classify(const std::string& text, const unsigned int& text_len,
                               std::string& guess_lang_output,
                               std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASSIFIER_DATA_TESTING_ENABLED
                               , Corpus& test_corpus
#endif // CLASSIFIER_DATA_TESTING_ENABLED
#ifdef CLASS_CONTRIBUTORS_ENABLED
                               , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                               , bool ignore_case) {

  guess_lang_output.clear();
  top_classes.clear();
  top_classes_count = 0;

  int num_ngrams = 0;
#ifndef CLASSIFIER_DATA_TESTING_ENABLED
  Corpus test_corpus;
#endif // CLASSIFIER_DATA_TESTING_ENABLED

  std::string script;
  //if ((num_ngrams = m_ngrams_generator.GetNgramsFromTweet(text, test_corpus, ignore_case)) < 0)
  if ((num_ngrams = GetCorpus(text, script, test_corpus)) < 0) {
    top_classes_count = 0;
#ifdef LD_DEBUG
    std::cerr << "ERROR: get corpus returned -1" << std::endl;
    guess_lang_output.assign("RR");
    top_classes.assign("RR");
    top_classes_count = 1;
#endif
    return -1;
  }

  if (num_ngrams == 0) {
    if (script.compare(0, 2, "en") != 0) {
      guess_lang_output.assign(script);
      top_classes.assign(script);
      top_classes_count = 1;
      return 0;
    }
    top_classes_count = 0;
#ifdef LD_DEBUG
    if (m_debug_level > 2)
      std::cout << "no ngrams found for ... \n" << text << std::endl;
    guess_lang_output.assign("XX");
    top_classes.assign("XX");
    top_classes_count = 1;
#endif
    return 0;
  }

  if (Classifier::Classify(test_corpus,
               guess_lang_output,
               top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
               , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
              ) < 0) {
    std::cout << "ERROR: could not classify the given corpus\n";
  }

#ifndef CLASSIFIER_DATA_TESTING_ENABLED
  test_corpus.clear();
#endif // CLASSIFIER_DATA_TESTING_ENABLED

  return 1;
}

int LanguageClassifier::Classify(const unsigned char* text_word_list,
                               const unsigned int& list_len,
                               const unsigned int& word_count, 
                               char* guess_lang_buffer,
                               const unsigned int& guess_lang_buffer_len,
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

  if (!guess_lang_buffer || !top_classes_buffer) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  guess_lang_buffer[0] = '\0';
  top_classes_buffer[0] = '\0';
  top_classes_len = 0;
  top_classes_count = 0;

  int num_ngrams = 0;
  Corpus test_corpus;

  if ((num_ngrams = m_ngrams_generator.GetNgramsFromWords(text_word_list,
                                                          list_len,
                                                          word_count,
                                                          test_corpus,
                                                          ignore_case)) < 0) {
    top_classes_count = 0;
#ifdef LD_DEBUG
    std::cerr << "ERROR: m_ngrams_generator returned -1" << std::endl;
    strcpy(top_classes_buffer, "RR|");
    top_classes_len = 3;
    top_classes_count = 1;
#endif
    return -1;
  }

  if (num_ngrams == 0) {
    top_classes_count = 0;
#ifdef LD_DEBUG
    if (m_debug_level > 2)
      std::cout << "no ngrams found for the given word set" << std::endl;
    strcpy(top_classes_buffer, "XX|");
    top_classes_len = 3;
    top_classes_count = 1;
#endif
    return 0;
  }

  std::string guess_lang_output;
  std::string top_classes;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED

  if (Classifier::Classify(test_corpus,
               guess_lang_output,
               top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
               , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
              ) < 0) {
#ifdef LD_DEBUG
    std::cout << "ERROR: could not classify the given corpus\n";
#endif
  }

#ifdef CLASS_CONTRIBUTORS_ENABLED
  class_contributors_map.clear();
#endif // CLASS_CONTRIBUTORS_ENABLED

#ifdef LD_DEBUG
  if (m_debug_level > 1) {
    std::cout << "guess_lang: " << guess_lang_output << std::endl;
    std::cout << "top_classes_buffer: " << top_classes << std::endl;
  }
#endif

  test_corpus.clear();
  strcpy(guess_lang_buffer, guess_lang_output.c_str());
  strcpy(top_classes_buffer, top_classes.c_str());
  top_classes_len = top_classes.length();

  return 1;
}

int LanguageClassifier::ClassifyLanguage(std::set<std::string>& words_set,
                                     std::string& guess_lang_output,
                                     std::string& top_classes,
                                     unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                     , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                     , bool ignore_case) {
  int num_ngrams = 0;
  Corpus test_corpus;

  if ((num_ngrams = m_ngrams_generator.GetNgramsFromWords(words_set, test_corpus, ignore_case)) < 0) {
#ifdef LD_DEBUG
    std::cerr << "ERROR: m_ngrams_generator returned -1" << std::endl;
#endif
    return -1;
  }

  if (num_ngrams == 0) {
    top_classes_count = 0;
#ifdef LD_DEBUG
    std::cout << "no ngrams found for the given word set" << std::endl;
    guess_lang_output.assign("RR");
    top_classes.assign("RR|");
    top_classes_count = 1;
#endif
    return 0;
  }

  if (Classifier::Classify(test_corpus,
               guess_lang_output,
               top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
               , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
     ) < 0) {
#ifdef LD_DEBUG
    std::cout << "ERROR: could not classify the given corpus\n";
#endif
  }

#ifdef LD_DEBUG
  if (m_debug_level > 1) {
    std::cout << "guess_lang: " << guess_lang_output << std::endl;
    std::cout << "top_classes: " << top_classes << std::endl;
  }
#endif

  test_corpus.clear();

  return 1;
}

// not sure if this is used anymore
int LanguageClassifier::GetNgramFrequencies(const std::string& input_file_name,
                                          Corpus& corpus) {

  std::ifstream ifs(input_file_name.c_str());
  if (!ifs) {
#ifdef LD_DEBUG
    std::cout << "ERROR: could not open file " << input_file_name << std::endl;
#endif
    return -1;
  }

  std::string line;
  int num_docs = 0;
  while (getline(ifs, line)) {
    if (m_ngrams_generator.GetNgrams((unsigned char*) line.c_str(), line.length(), corpus) <= 0) {
      num_docs++;
    }
  }
  ifs.close();

  return corpus.size();
}

/*
int LanguageClassifier::GenerateLangModel(const std::string& input_file_name,
                                     const std::string& output_file_name) {

  Corpus lang_corpus;
  int count = 0;
  if ((count = GetNgramFrequencies(input_file_name, lang_corpus)) < 0) {
    std::cout << "ERROR: could not get features for lang in file " << input_file_name;
    return -1;
  } else if (count > 0) {
    if (m_corpus_manager.WriteCorpusToFile(lang_corpus, output_file_name) < 0) {
      std::cout << "ERROR: could not write to features to output file " << output_file_name << std::endl;
    }
  }
  lang_corpus.clear();

  return count;
}
*/

int LanguageClassifier::GetCorpus(const std::string& text, Corpus& corpus) {
  std::string script;
  return GetCorpus(text, script, corpus);
}

int LanguageClassifier::GetCorpus(const std::string& text, std::string& script, Corpus& corpus) {

  if (text.length() < 1) {
    std::cerr << "ERROR: empty string. cannot get corpus\n";
    return -1;
  }

  char buffer[MAX_BUFFER_LEN];
  strcpy((char *) buffer, text.c_str());

#ifdef NAMED_ENTITIES_ENABLED
  std::set<std::string> named_entities_set;
#endif // NAMED_ENTITIES_ENABLED

#ifdef KEYPHRASE_ENABLED
  std::set<std::string> keyphrases_set;
#endif // KEYPHRASE_ENABLED

#ifdef KEYWORDS_ENABLED
  std::set<std::string> keywords_set;
#endif // KEYWORDS_ENABLED

  std::set<std::string> lang_words_set;

#ifdef TEXT_CLASSIFICATION_ENABLED
  std::set<std::string> text_class_words_set;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef INTENT_ENABLED
  int intent_valence = 0;
#endif // INTENT_ENABLED

#ifdef SENTIMENT_ENABLED
  int sentiment_valence = 0;
#endif // SENTIMENT_ENABLED

  std::string safe_status;

  int ret_val = 0;
  ret_val = m_keytuples_extracter.GetKeyTuples((char *) buffer,
                                               safe_status,
                                               script
#ifdef NAMED_ENTITIES_ENABLED
                                               , named_entities_set
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYPHRASE_ENABLED
                                               , keyphrases_set
#endif // KEYPHRASE_ENABLED
#ifdef KEYWORDS_ENABLED
                                               , keywords_set
#endif // KEYWORDS_ENABLED
#ifdef LANG_ENABLED
                                               , lang_words_set
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                                               , text_class_words_set
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                                               , intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                                               , sentiment_valence
#endif // SENTIMENT_ENABLED
                                              );

  if (ret_val < 0) {
#ifdef LD_DEBUG
    std::cerr << "ERROR: could not get keytuples for: " << text << std::endl;
#endif // LD_DEBUG
    lang_words_set.clear();
    return -1;
  }

  if (script.compare(0, 2, "en") != 0) {
    lang_words_set.clear();
    return 0;
  }

  int ngrams_count = 0;
  if (lang_words_set.empty()) {
#ifdef LD_DEBUG
    std::cerr << "ERROR: lang words set empty. cannot classify\n";
#endif // LD_DEBUG
  } else {
    if ((ngrams_count = m_ngrams_generator.GetNgramsFromWords(lang_words_set, corpus)) < 0) {
#ifdef LD_DEBUG
      std::cerr << "ERROR: could not find ngrams from tweet: " << text << std::endl;
#endif // LD_DEBUG
    }
    if (ngrams_count == 0) {
#ifdef LD_DEBUG
      std::cerr << "ERROR: no ngrams found\n";
#endif // LD_DEBUG
    }
  }

#ifdef NAMED_ENTITIES_ENABLED
  named_entities_set.clear();
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYPHRASE_ENABLED
  keyphrases_set.clear();
#endif // KEYPHRASE_ENABLED
#ifdef KEYWORDS_ENABLED
  keywords_set.clear();
#endif // KEYWORDS_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  text_class_words_set.clear();
#endif // TEXT_CLASSIFICATION_ENABLED

  lang_words_set.clear();

  return ngrams_count;

}

int LanguageClassifier::Clear() {
  return 0;
}

int LanguageClassifier::InitDependencies(int argc, char* argv[]) {

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

int LanguageClassifier::ClearDependencies() {
  try {
    m_keytuples_extracter.DeInit();
  } catch (...) {
    std::cerr << "ERROR: KeyTuples Extracter throws exception" << std::endl;
  }
  return 0;
}

} // namespace inagist_classifiers

