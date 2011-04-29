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
#include "classifier.h"
#include "ngrams_generator.h"
#include "corpus_manager.h"
#include "naive_bayes_classifier.h"
#include "keytuples_extracter.h"

namespace inagist_classifiers {

class LanguageDetector : public Classifier {
 public:
  LanguageDetector();
  ~LanguageDetector();

  // inherited from classifier
  // int Init(std::string config_file_name);

  /* implementation of pure virtual functions from classifier.h */
  int InitDependencies(int argc=0, char* argv[]=NULL);
  int Classify(const std::string& text,
                 const unsigned int& text_len,
                 std::string& guess_lang_output,
                 bool ignore_case=false);
  int GetCorpus(const std::string& text, Corpus& corpus);
  int ClearDependencies();
  /* end */

  int InitTraining(const char *stopwords_file,
                   const char *dictionary_file,
                   const char *unsafe_dictionary_file);
  int DetectLanguage(std::set<std::string>& words_set,
                     std::string& guess_lang_output,
                     bool ignore_case=false);
  int GenerateLangModel(const std::string& input_file_name,
                        const std::string& output_file_name);
  int GenerateLangModelFromTweets(const std::string& twitter_handles_file_name,
                                  const std::string& tweets_file_name,
                                  const std::string& output_file_name);
  int GetNgramFrequencies(const std::string& input_file_name,
                          Corpus& corpus);
  int Clear();
  int SetDebugLevel(unsigned int debug_level);
  
 private:
  NgramsGenerator m_ngrams_generator;
  NaiveBayesClassifier m_naive_bayes_classifier;
  inagist_trends::KeyTuplesExtracter m_keytuples_extracter;

  unsigned int m_debug_level;

  DISALLOW_COPY_AND_ASSIGN(LanguageDetector); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_LANGUAGE_DETECTOR_H_
