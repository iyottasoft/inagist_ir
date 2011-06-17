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
  int Classify(const std::string& text, const unsigned int& text_len,
               std::string& guess_lang_output,
               std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
               , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
               , bool ignore_case=false);
  int GetCorpus(const std::string& text, Corpus& corpus);
  int ClearDependencies();
  /* end */

  int Classify(const unsigned char* text_word_list,
               const unsigned int& list_len,
               const unsigned int& word_count,
               char* guess_lang_buffer, const unsigned int& guess_lang_buffer_len,
               char* top_classes_buffer, const unsigned int& top_classes_buffer_len,
               unsigned int& top_classes_len, unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
               , unsigned char* class_contributors_buffer,
               const unsigned int& class_contributors_buffer_len,
               unsigned int& class_contributors_len,
               unsigned int& class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
               , bool ignore_case=false);

  int InitTraining(const char *stopwords_file,
                   const char *dictionary_file,
                   const char *unsafe_dictionary_file);
  int DetectLanguage(std::set<std::string>& words_set,
                     std::string& guess_lang_output,
                     std::string& top_classes,
                     unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                     , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                     , bool ignore_case=false);
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
