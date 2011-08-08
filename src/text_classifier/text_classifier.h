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
  int Classify(const std::string& text, const unsigned int& text_len,
               std::string& text_class,
               std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASSIFIER_DATA_TESTING_ENABLED
               , Corpus& test_corpus
#endif // CLASSIFIER_DATA_TESTING_ENABLED
#ifdef CLASS_CONTRIBUTORS_ENABLED
               , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
               , bool ignore_case=false);
  int GetCorpus(const std::string& text, Corpus& corpus);
  int ClearDependencies();
  /* end */

  int Classify(Corpus& corpus,
               std::string& text_class,
               std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
               , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
              );

  int Classify(std::set<std::string>& words_set,
                 std::string& text_class,
                 std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                 , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                 , bool ignore_case=false);

  int Classify(const unsigned char* text_word_list, const unsigned int& list_len, const unsigned int& word_count,
               char* guess_text_class_buffer, const unsigned int& guess_text_class_buffer_len,
               char* top_classes_buffer, const unsigned int& top_classes_buffer_len,
               unsigned int& top_classes_len,
               unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
               , unsigned char* class_contributors_buffer,
               const unsigned int& class_contributors_buffer_len,
               unsigned int& class_contributors_len,
               unsigned int& class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
               , bool ignore_case=false);

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
