#ifndef _INAGIST_CLASSIFIERS_NAIVE_BAYES_H_
#define _INAGIST_CLASSIFIERS_NAIVE_BAYES_H_

#include <map>
#include <set>
#include "corpus_manager.h"

#if defined CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED
#include "classifier_config.h"
#endif // CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED

namespace inagist_classifiers {

class NaiveBayesClassifier {
 public:
  NaiveBayesClassifier();
  ~NaiveBayesClassifier();
  int SetDebugLevel(unsigned int debug_level);
  static int GuessClass(CorpusMap& corpus_map,
                 Corpus& classes_freq_map,
                 Corpus& test_corpus,
                 std::string& guess_class_output,
                 unsigned int debug_level=0);
  static int GuessClass2(CorpusMap& corpus_map,
                 Corpus& classes_freq_map,
                 Corpus& test_corpus,
                 std::string& guess_class_output,
                 std::string& top_classes_output,
                 unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                 , std::map<std::string, std::string>& class_contributors
#endif // CLASS_CONTRIBUTORS_ENABLED
                 , unsigned int debug_level=0);
  static int GuessClass2(CorpusMap& corpus_map,
                 Corpus& classes_freq_map,
                 Corpus& test_corpus,
                 std::string& guess_class_output,
                 std::set<std::string>& top_classes_set
#ifdef CLASS_CONTRIBUTORS_ENABLED
                 , std::map<std::string, std::string>& class_contributors
#endif // CLASS_CONTRIBUTORS_ENABLED
                 , unsigned int debug_level=0);
  static int Heapify(double& top1, unsigned int& top1_index,
                     double& top2, unsigned int& top2_index,
                     double& top3, unsigned int& top3_index);

  int GuessClass(std::map<std::string, int> testfile_features_map,
                 std::map<std::string, int> class1_features_map,
                 std::map<std::string, int> class2_features_map); // this is perhaps no longer used

#if defined CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED
  int PrepareNaiveBayes(std::string config_file_name,
                        const bool& train_not_test,
                        bool ignore_history=true);
  int GenerateProbabilities(const bool& train_not_test);
  int GenerateProbabilities(Corpus* corpus,
                            const char* relative_freq_file,
                            const unsigned int& vocabulary_size);
  int MakePriorProbabilitiesFile(const char* classifier_prior_freqs_file);
  int Clear();
#endif // CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED

 private:
  unsigned int m_debug_level;
#if defined CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED
  CorpusMap m_corpus_map;
  Config m_config;
  Corpus m_classes_freq_map;
#endif // CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED

};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_NAIVE_BAYES_H_
