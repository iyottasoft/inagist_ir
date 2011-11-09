#ifndef _INAGIST_CLASSIFIER_H_
#define _INAGIST_CLASSIFIER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName); \
  void operator=(const TypeName&
#endif

#include <string>
#include "classifier_config.h"
#include "corpus_manager.h"
#include "naive_bayes_classifier.h"

#define MIN_TWEETS_REQUIRED 15

namespace inagist_classifiers {

// its assumed that a more specialised classifier will inherit this class
class Classifier {

 public:

  Classifier();
  virtual ~Classifier();

  int Init(std::string config_file_name, bool ignore_history=false, unsigned int corpus_type=0);
  virtual int GetCorpus(const std::string& text, Corpus& corpus);
  int Classify(Corpus& test_corpus,
               std::string& output_class,
               std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
               , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
              );
  int Classify(const std::string& text,
                       const unsigned int& text_len,
                       std::string& output_class,
                       std::string& top_classes,
                       unsigned int& top_classes_count
#ifdef CLASSIFIER_DATA_TESTING_ENABLED
                       , Corpus& test_corpus
#endif // CLASSIFIER_DATA_TESTING_ENABLED
#ifdef CLASS_CONTRIBUTORS_ENABLED
                       , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                       , bool ignore_case=false);

  int InitDependencies(int argc=0, char* argv[]=NULL);
  int ClearDependencies();

  int SetDebugLevel(unsigned int debug_level);

  // training

  // train_not_test is a bool
  // true = training
  // false = testing
  int GetData(const bool& train_not_test, const char* config_file_name);
  int GetData(const bool& train_not_test,
              const std::string& class_name,
              const std::string& twitter_handles_file_name,
              const std::string& output_tweets_file_name,
              unsigned int& output_num_docs,
              const std::string& output_corpus_file_name,
              unsigned int& output_corpus_size);
  int GetData(const bool& train_not_test,
              const std::string& handle,
              const std::string& expected_class_name,
              Corpus& output_corpus,
              unsigned int& output_corpus_size,
              unsigned int& output_num_docs,
              Corpus& output_freqs_map,
              std::ostream& output_stream,
              bool get_user_info=false);

  int CleanCorpusFile(std::string& corpus_file_name,
                      std::string& output_prefix,
                      unsigned int& clean_type);
  int CleanCorpus(const bool& train_not_test,
                  unsigned int& input_type,
                  std::string& file_name,
                  std::string& output_prefix,
                  unsigned int& clean_type);

  int MakeDictionary(const char* classifier_dictionary_file);

 protected:
  std::map<std::string, std::string> m_class_labels_map;
  std::map<std::string, std::string> m_class_numbers_map;
  CorpusMap m_corpus_map;
  Corpus m_classes_freq_map;
  Corpus m_corpus;
 private:
  NaiveBayesClassifier m_naive_bayes_classifier;
  Config m_config;
  unsigned int m_debug_level;

};

} // namespace inagist_classifiers

#endif // _INAGIST_CLASSIFIER_H_
