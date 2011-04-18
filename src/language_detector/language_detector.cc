#include "language_detector.h"
#include <iostream>
#include <fstream>
#include "twitter_searcher.h"
#include "string_utils.h"
#include "config_reader.h"

#ifdef DEBUG
#if DEBUG>0
#define LD_DEBUG DEBUG
#endif
#endif
//#define LD_DEBUG 5

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
 
int LanguageDetector::Classify(const std::string& text,
                                 const unsigned int& text_len,
                                 std::string& guess_lang_output,
                                 bool ignore_case) {
  int num_ngrams = 0;
  Corpus test_corpus;

  if ((num_ngrams = m_ngrams_generator.GetNgramsFromTweet(text, test_corpus, ignore_case)) < 0) {
    std::cerr << "ERROR: m_ngrams_generator returned -1" << std::endl;
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
    guess_lang_output.assign("RR");
    return 0;
  }

#ifdef LD_DEBUG
  if (m_debug_level > 1)
    std::cout << "now guessing class for ... \n" << text << std::endl;
#endif

  if (m_naive_bayes_classifier.GuessClass(m_corpus_manager.m_corpus_map,
                                          m_corpus_manager.m_classes_freq_map,
                                          test_corpus,
                                          guess_lang_output,
                                          m_debug_level) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the language\n";
    test_corpus.clear();
    return -1;
  }

#ifdef LD_DEBUG
  if (m_debug_level > 1)
    std::cout << "guess_lang: " << guess_lang_output << std::endl;
#endif

  test_corpus.clear();

  return 1;
}

int LanguageDetector::DetectLanguage(std::set<std::string>& words_set,
                                     std::string& guess_lang_output,
                                     bool ignore_case) {
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
    return 0;
  }

  if (m_naive_bayes_classifier.GuessClass(m_corpus_manager.m_corpus_map,
                                          m_corpus_manager.m_classes_freq_map,
                                          test_corpus,
                                          guess_lang_output,
                                          m_debug_level) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the language\n";
    test_corpus.clear();
    return -1;
  }

#ifdef LD_DEBUG
  if (m_debug_level > 1)
    std::cout << "guess_lang: " << guess_lang_output << std::endl;
#endif

  test_corpus.clear();

  return 1;
}

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
  int ngrams_count = 0;
  if ((ngrams_count = m_ngrams_generator.GetNgramsFromTweet(text, corpus)) < 0) {
    std::cerr << "ERROR: could not find ngrams from tweet: " << text << std::endl;
  }
  return ngrams_count;
}

int LanguageDetector::Clear() {
  return 0;
}

int LanguageDetector::InitDependencies(int argc, char* argv[]) {
  return 0;
}

int LanguageDetector::ClearDependencies() {
  return 0;
}

} // namespace inagist_classifiers

