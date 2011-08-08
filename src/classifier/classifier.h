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

#define MIN_TWEETS_REQUIRED 15

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

  int Init(std::string config_file_name, bool ignore_history=false);
  virtual int GetCorpus(const std::string& text, Corpus& corpus)=0;
  virtual int Classify(const std::string& text,
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
                       , bool ignore_case=false)=0;

  virtual int InitDependencies(int argc=0, char* argv[]=NULL)=0;
  virtual int ClearDependencies()=0;

  int SetDebugLevel(unsigned int debug_level);

  // training

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
  int CleanCorpusFile(std::string& corpus_file_name,
                      std::string& output_prefix);
  int CleanCorpus(unsigned int input_type,
                 std::string& file_name,
                 std::string& output_prefix);


  // testing

  int GetTestData(const unsigned int& input_type,
                  const char* input_file,
                  const char* input_handle,
                  const std::string& expected_class_name,
                  const unsigned int& output_type,
                  const char* output_file);

  // leave handle blank for public timeline
  int TestTwitterTimeline(const std::string& handle,
                          const std::string& expected_class_name,
                          Corpus& test_freq_map,
                          CorpusMap& test_corpus_map,
                          TestResult& test_result,
                          std::ostream& output_stream);

  int TestTrainingSources(const char* training_class,
                          Corpus& test_freq_map,
                          CorpusMap& test_corpus_map,
                          TestResult& test_result,
                          std::ostream& output_stream,
                          bool random_selection=false);

  int TestTrainingTexts(const char* training_texts_file,
                        const std::string& expected_class_name,
                        Corpus& test_freq_map,
                        CorpusMap& test_corpus_map,
                        TestResult& test_result,
                        std::ostream &output_stream);

  int WriteTestData(Corpus& corpus, const char* classes_freq_file);

  static int ValidateTestDataInput(int argc, char* argv[],
                            const char* &config_file,
                            const char* &keytuples_config_file,
                            unsigned int &input_type,
                            unsigned int &output_type,
                            const char* &input_file,
                            const char* &output_file,
                            const char* &input_handle,
                            std::string &class_name);

  int NormalizeFrequencies();
  int NormalizeFrequencies(const char* raw_data_file, const char* relative_freq_file);

 protected:
  CorpusManager m_corpus_manager;
  std::map<std::string, std::string> m_class_labels_map;
 private:
  Config m_config;
  unsigned int m_debug_level;

};

} // namespace inagist_classifiers

#endif // _INAGIST_CLASSIFIER_H_
