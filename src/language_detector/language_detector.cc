#include "language_detector.h"
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

LanguageDetector::LanguageDetector() {
#ifdef LD_DEBUG
  m_debug_level = LD_DEBUG;
#else
  m_debug_level = 0;
#endif
}

LanguageDetector::~LanguageDetector() {
  Clear();
}

int LanguageDetector::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  m_ngrams_generator.SetDebugLevel(debug_level);
  m_naive_bayes_classifier.SetDebugLevel(debug_level);
  return 0;
}
 
int LanguageDetector::Classify(const std::string& text, const unsigned int& text_len,
                               std::string& guess_lang_output,
                               std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                               , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                               , bool ignore_case) {

  top_classes_count = 0;

  int num_ngrams = 0;
  Corpus test_corpus;

  if ((num_ngrams = m_ngrams_generator.GetNgramsFromTweet(text, test_corpus, ignore_case)) < 0) {
#ifdef LD_DEBUG
    std::cerr << "ERROR: m_ngrams_generator returned -1" << std::endl;
#endif
    guess_lang_output.assign("RR");
    top_classes.assign("RR");
    top_classes_count = 1;
    return -1;
  } else {
#ifdef LD_DEBUG
    if (m_corpus_manager.PrintCorpus(test_corpus) < 0) {
      std::cerr << "ERROR: could not print corpus in DetectLanguage" << std::endl;
    }
#endif
  }

  if (num_ngrams == 0) {
#ifdef LD_DEBUG
    if (m_debug_level > 0)
      std::cout << "no ngrams found for ... \n" << text << std::endl;
#endif
    guess_lang_output.assign("XX");
    top_classes.assign("XX");
    top_classes_count = 1;
    return 0;
  }

#ifdef LD_DEBUG
  if (m_debug_level > 1)
    std::cout << "now guessing class for ... \n" << text << std::endl;
#endif

  if (m_naive_bayes_classifier.GuessClass2(m_corpus_manager.m_corpus_map,
                                           m_corpus_manager.m_classes_freq_map,
                                           test_corpus,
                                           guess_lang_output,
                                           top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                           , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                           , m_debug_level) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the language\n";
    test_corpus.clear();
    guess_lang_output.assign("UU");
    top_classes.assign("UU");
    top_classes_count = 1;
    return -1;
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

int LanguageDetector::Classify(const unsigned char* text_word_list,
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
#ifdef LD_DEBUG
    std::cerr << "ERROR: m_ngrams_generator returned -1" << std::endl;
#endif
    strcpy(top_classes_buffer, "RR|");
    top_classes_len = 3;
    top_classes_count = 1;
    return -1;
  }

  if (num_ngrams == 0) {
#ifdef LD_DEBUG
    if (m_debug_level > 0)
      std::cout << "no ngrams found for the given word set" << std::endl;
#endif
    strcpy(top_classes_buffer, "XX|");
    top_classes_len = 3;
    top_classes_count = 1;
    return 0;
  }

  std::string guess_lang_output;
  std::string top_classes;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED
  if (m_naive_bayes_classifier.GuessClass2(m_corpus_manager.m_corpus_map,
                                          m_corpus_manager.m_classes_freq_map,
                                          test_corpus,
                                          guess_lang_output,
                                          top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                          , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                          , m_debug_level) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the language\n";
    test_corpus.clear();
    strcpy(top_classes_buffer, "UU|");
    top_classes_len = 3;
    top_classes_count = 1;
    return -1;
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

int LanguageDetector::DetectLanguage(std::set<std::string>& words_set,
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
    std::cerr << "ERROR: m_ngrams_generator returned -1" << std::endl;
    return -1;
  }

  if (num_ngrams == 0) {
#ifdef LD_DEBUG
    if (m_debug_level > 0)
      std::cout << "no ngrams found for the given word set" << std::endl;
#endif
    guess_lang_output.assign("RR");
    top_classes.assign("RR|");
    top_classes_count = 1;
    return 0;
  }

  if (m_naive_bayes_classifier.GuessClass2(m_corpus_manager.m_corpus_map,
                                           m_corpus_manager.m_classes_freq_map,
                                           test_corpus,
                                           guess_lang_output,
                                           top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                           , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                           , m_debug_level) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the language\n";
    test_corpus.clear();
    guess_lang_output.assign("UU");
    top_classes.assign("UU|");
    top_classes_count = 1;
    return -1;
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
int LanguageDetector::GetNgramFrequencies(const std::string& input_file_name,
                                          Corpus& corpus) {

  std::ifstream ifs(input_file_name.c_str());
  if (!ifs) {
    std::cout << "ERROR: could not open file " << input_file_name << std::endl;
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

int LanguageDetector::GenerateLangModel(const std::string& input_file_name,
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

int LanguageDetector::GetCorpus(const std::string& text, Corpus& corpus) {

  if (text.length() < 1) {
    std::cerr << "ERROR: empty string. cannot get corpus\n";
    return -1;
  }

  char buffer[MAX_BUFFER_LEN];
  strcpy((char *) buffer, text.c_str());

#ifndef KEYWORDS_DISABLED
  std::set<std::string> keywords_set;
#endif // KEYWORDS_DISABLED
#ifdef KEYPHRASE_ENABLED
  std::set<std::string> keyphrases_set;
#endif // KEYPHRASE_ENABLED
#ifdef HASHTAGS_ENABLED
  std::set<std::string> hashtags_set;
#endif // HASHTAGS_ENABLED

  std::set<std::string> lang_words_set;

#ifdef TEXT_CLASS_WORDS_ENABLED
  std::set<std::string> text_class_words_set;
#endif // TEXT_CLASS_WORDS_ENABLED

  std::string safe_status;
  std::string script;

  int ret_val = 0;
  ret_val = m_keytuples_extracter.GetKeyTuples((char *) buffer,
                                               safe_status,
                                               script
#ifndef KEYWORDS_DISABLED
                                               , keywords_set
#endif // KEYWORDS_DISABLED
#ifdef KEYPHRASE_ENABLED
                                               , keyphrases_set
#endif // KEYPHRASE_ENABLED
#ifdef HASHTAGS_ENABLED
                                               , hashtags_set
#endif // HASHTAGS_ENABLED
                                               , lang_words_set
#ifdef TEXT_CLASS_WORDS_ENABLED
                                               , text_class_words_set
#endif // TEXT_CLASS_WORDS_ENABLED
                                              );

  if (ret_val < 0) {
    std::cerr << "ERROR: could not get words for: " << text << std::endl;
    lang_words_set.clear();
    return -1;
  }

  int ngrams_count = 0;
  if ((ngrams_count = m_ngrams_generator.GetNgramsFromWords(lang_words_set, corpus)) < 0) {
    std::cerr << "ERROR: could not find ngrams from tweet: " << text << std::endl;
  }

#ifndef KEYWORDS_DISABLED
  keywords_set.clear();
#endif // KEYWORDS_DISABLED
#ifdef KEYPHRASE_ENABLED
  keyphrases_set.clear();
#endif // KEYPHRASE_ENABLED
#ifdef HASHTAGS_ENABLED
  hashtags_set.clear();
#endif // HASHTAGS_ENABLED
#ifdef TEXT_CLASS_WORDS_ENABLED
  text_class_words_set.clear();
#endif // TEXT_CLASS_WORDS_ENABLED

  lang_words_set.clear();

  return ngrams_count;

}

int LanguageDetector::Clear() {
  return 0;
}

int LanguageDetector::InitDependencies(int argc, char* argv[]) {

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

int LanguageDetector::ClearDependencies() {
  try {
    m_keytuples_extracter.DeInit();
  } catch (...) {
    std::cerr << "ERROR: KeyTuples Extracter throws exception" << std::endl;
  }
  return 0;
}

} // namespace inagist_classifiers

