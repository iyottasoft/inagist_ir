#ifndef _INAGIST_CLASSIFIERS_LANGUAGE_DETECTOR_H_
#define _INAGIST_CLASSIFIERS_LANGUAGE_DETECTOR_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <map>
#include <set>
#include "ngrams_generator.h"
#include "corpus_manager.h"
#include "naive_bayes_classifier.h"

namespace inagist_classifiers {

class LanguageDetector {
 public:
  LanguageDetector();
  ~LanguageDetector();
  int Init(std::string config_file_name);
  int DetectLanguage(const std::string& text, const unsigned int& text_len,
                     std::string& guess_lang_output);
  int GenerateLangModel(const std::string& input_file_name,
                        const std::string& output_file_name);
  int GenerateLangModelFromTweets(const std::string& twitter_handles_file_name,
                                  const std::string& tweets_file_name,
                                  const std::string& output_file_name);
  int GetNgramFrequencies(const std::string& input_file_name,
                          Corpus& corpus);
  int Clear();
  
 private:
  NgramsGenerator m_ngrams_generator;
  CorpusManager m_corpus_manager;
  NaiveBayesClassifier m_naive_bayes_classifier;

  DISALLOW_COPY_AND_ASSIGN(LanguageDetector); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_LANGUAGE_DETECTOR_H_
