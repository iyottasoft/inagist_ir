#ifndef _INAGIST_CLASSIFIERS_CLASSIFIER_TESTING_H_
#define _INAGIST_CLASSIFIERS_CLASSIFIER_TESTING_H_

#include "corpus_manager.h"
#include <string>
#include <set>

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

class ClassifierTesting {
 public:
  // functions
  ClassifierTesting();
  ~ClassifierTesting();

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

  static int ValidateTestDataInput(int argc, char* argv[],
                            const char* &config_file,
                            const char* &keytuples_config_file,
                            unsigned int &input_type,
                            unsigned int &output_type,
                            const char* &input_file,
                            const char* &output_file,
                            const char* &input_handle,
                            std::string &class_name);

};

} // namespace inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_CLASSIFIER_TESTING_H_
