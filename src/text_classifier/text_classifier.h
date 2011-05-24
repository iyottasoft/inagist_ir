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
#include "keytuples_extracter.h"

namespace inagist_classifiers {

class TextClassifier : public Classifier {
 public:
  TextClassifier();
  ~TextClassifier();

  /* implementation of pure virtual functions from classifier.h */
  int InitDependencies(int argc=0, char* argv[]=NULL);
  int Classify(const std::string& text,
                 const unsigned int& text_len,
                 std::string& text_class,
                 bool ignore_case=false);
  int GetCorpus(const std::string& text, Corpus& corpus);
  int ClearDependencies();
  /* end */

  int Classify(Corpus& corpus, std::string& text_class);

  int Classify(std::set<std::string>& words_set,
                 std::string& text_class,
                 bool ignore_case=false);

  int Classify(const unsigned char* text_word_list,
               const unsigned int& list_len,
               const unsigned int& word_count,
               char* guess_text_class_buffer,
               const unsigned int& guess_text_class_buffer_len,
               bool ignore_case=false);

  int Clear();
  int SetDebugLevel(unsigned int debug_level);
  int GetWordFrequencies(const std::string& input_file_name,
                         Corpus& corpus);
  int LoadKeyTuplesDictionary(const char* dictionary_file);

 private:
  inagist_trends::KeyTuplesExtracter m_keytuples_extracter;

  unsigned int m_debug_level;

  DISALLOW_COPY_AND_ASSIGN(TextClassifier); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_TEXT_CLASSIFIER_H_
