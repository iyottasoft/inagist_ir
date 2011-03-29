#ifndef _INAGIST_CLASSIFIERS_NAIVE_BAYES_H_
#define _INAGIST_CLASSIFIERS_NAIVE_BAYES_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <map>
#include "corpus_manager.h"

namespace inagist_classifiers {

class NaiveBayesClassifier {
 public:
  NaiveBayesClassifier();
  ~NaiveBayesClassifier();
  static int GuessClass(CorpusMap& corpus_map,
                 Corpus& classes_freq_map,
                 Corpus& test_corpus,
                 std::string& guess_class_output);
  int GuessClass(std::map<std::string, int> testfile_features_map,
                 std::map<std::string, int> class1_features_map,
                 std::map<std::string, int> class2_features_map);
  int SetDebugLevel(unsigned int debug_level);

 private:
  unsigned int m_debug_level;

  DISALLOW_COPY_AND_ASSIGN(NaiveBayesClassifier); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_NAIVE_BAYES_H_
