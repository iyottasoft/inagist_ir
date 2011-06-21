#ifndef _INAGIST_CLASSIFIERS_NAIVE_BAYES_H_
#define _INAGIST_CLASSIFIERS_NAIVE_BAYES_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <map>
#include <set>
#include "corpus_manager.h"

namespace inagist_classifiers {

class NaiveBayesClassifier {
 public:
  NaiveBayesClassifier();
  ~NaiveBayesClassifier();
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
  int GuessClass(std::map<std::string, int> testfile_features_map,
                 std::map<std::string, int> class1_features_map,
                 std::map<std::string, int> class2_features_map);
  int SetDebugLevel(unsigned int debug_level);
  static int Heapify(double& top1, unsigned int& top1_index,
                     double& top2, unsigned int& top2_index,
                     double& top3, unsigned int& top3_index);

 private:
  unsigned int m_debug_level;

  DISALLOW_COPY_AND_ASSIGN(NaiveBayesClassifier); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_NAIVE_BAYES_H_
