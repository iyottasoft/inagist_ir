#ifndef _INAGIST_CLASSIFIER_H_
#define _INAGIST_CLASSIFIER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName); \
  void operator=(const TypeName&
#endif

#include <string>
#include <fstream>
#include "classifier_config.h"
#include "corpus_manager.h"

namespace inagist_classifiers {

// its assumed that a more specialised classifier will inherit this class
class Classifier {

 public:

  Classifier();
  virtual ~Classifier();
  // classification functions
  int Init(std::string config_file_name, bool ignore_history=false);
  virtual int GetCorpus(const std::string& text, Corpus& corpus)=0;
  virtual int Classify(const std::string& text,
                       const unsigned int& text_len,
                       std::string& output_class,
                       std::string& top_classes,
                       unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                       , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                       , bool ignore_case=false)=0;

  // dependency related for training and testing

  virtual int InitDependencies(int argc=0, char* argv[]=NULL)=0;
  virtual int ClearDependencies()=0;

  // training

#ifdef DATA_TRAINING_ENABLED
  int GetTrainingData(const char* config_file_name);
  int GetTrainingData(const std::string& class_name,
                      const std::string& twitter_handles_file_name,
                      const std::string& output_tweets_file_name,
                      unsigned int& output_num_docs,
                      const std::string& output_corpus_file_name,
                      unsigned int& output_corpus_size);
  int GetTrainingData(const std::string& handle,
                      unsigned int& output_num_docs,
                      Corpus& corpus,
                      unsigned int& output_corpus_size,
                      bool get_user_info=false);
#endif // DATA_TRAINING_ENABLED

  // testing

 protected:
  CorpusManager m_corpus_manager;
 private:
  Config m_config;
};

} // namespace inagist_classifiers

#endif // _INAGIST_CLASSIFIER_H_
