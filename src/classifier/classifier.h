#ifndef _INAGIST_CLASSIFIER_H_
#define _INAGIST_CLASSIFIER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName); \
  void operator=(const TypeName&
#endif

#include <string>
#include "corpus_manager.h"

namespace inagist_classifiers {

class Classifier {
 public:
  Classifier();
  ~Classifier();
  virtual int GetCorpus(const std::string& text, Corpus& corpus)=0;
  int GetTrainingData(const char* config_file_name);
  int GetTrainingData(const std::string& twitter_handles_file_name,
                      const std::string& output_tweets_file_name,
                      const std::string& output_corpus_file_name);
  int GetTestData();

 private:
};

} // namespace inagist_classifiers
#endif // _INAGIST_CLASSIFIER_H_
