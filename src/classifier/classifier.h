#ifndef _INAGIST_CLASSIFIER_H_
#define _INAGIST_CLASSIFIER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName); \
  void operator=(const TypeName&
#endif

#include <string>
#include <fstream>
#include "config_reader.h"
#include "corpus_manager.h"

namespace inagist_classifiers {

typedef struct _test_result_struct {
  unsigned int total;
  unsigned int undefined;
  unsigned int correct;
  unsigned int wrong;
  int clear() {
    total = 0;
    undefined = 0;
    correct = 0;
    wrong = 0;
    return 0;
  }
} TestResult;

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
                         bool ignore_case=false)=0;

  // dependency related for training and testing

  virtual int InitDependencies(int argc=0, char* argv[]=NULL)=0;
  virtual int ClearDependencies()=0;

  // training

  int GetTrainingData(const char* config_file_name);
  int GetTrainingData(const std::string& twitter_handles_file_name,
                      const std::string& output_tweets_file_name,
                      const std::string& output_corpus_file_name);
  int GetTrainingData(const std::string& handle,
                      Corpus& corpus,
                      bool get_user_info=false);

  // testing

  // leave handle blank for public timeline
  int TestTwitterTimeline(const std::string& handle,
                          const std::string& expected_class_name,
                          Corpus& class_freq_map,
                          TestResult& test_result,
                          std::ostream& output_stream);

  int TestTrainingSources(const char* training_class,
                          Corpus& class_freq_map,
                          TestResult& test_result,
                          std::ostream& output_stream,
                          bool random_selection=false);

  int GetTestData(const unsigned int& input_type,
                  const char* input_file,
                  const char* input_handle,
                  const std::string& expected_class_name,
                  const unsigned int& output_type,
                  const char* output_file);

  int TestTrainingTexts(const char* training_texts_file,
                        const std::string& expected_class_name,
                        Corpus& class_freq_map,
                        TestResult& test_result,
                        std::ostream &output_stream);

  int WriteTestData(Corpus& corpus, const char* classes_freq_file);

 protected:
  CorpusManager m_corpus_manager;
 private:
  Config m_config;
};

} // namespace inagist_classifiers

#endif // _INAGIST_CLASSIFIER_H_
