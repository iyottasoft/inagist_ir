#ifndef _INAGIST_CLASSIFIERS_TEXT_CLASSIFIER_H_
#define _INAGIST_CLASSIFIERS_TEXT_CLASSIFIER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <map>
#include <set>
#include "classifier.h"
#include "corpus_manager.h"

namespace inagist_classifiers {

class TextClassifier : public Classifier {
 public:
  TextClassifier();
  ~TextClassifier();
  int Init(std::string config_file_name);
  int GuessClass(const std::string& text,
                 const unsigned int& text_len,
                 std::string& text_class,
                 bool ignore_case=false);
  int GuessClass(std::set<std::string>& words_set,
                 std::string& text_class,
                 bool ignore_case=false);
  int Clear();
  int SetDebugLevel(unsigned int debug_level);
  int GetCorpus(const std::string& text, Corpus& corpus);
  int GetWordFrequencies(const std::string& input_file_name,
                         Corpus& corpus);

 private:
  CorpusManager m_corpus_manager;

  unsigned int m_debug_level;

  DISALLOW_COPY_AND_ASSIGN(TextClassifier); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_TEXT_CLASSIFIER_H_
