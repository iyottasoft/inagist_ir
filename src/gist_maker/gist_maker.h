/* gist_maker.h */

#ifndef _INAGIST_GIST_MAKER_H_
#define _INAGIST_GIST_MAKER_H_

#include <string>
#include "keytuples_extracter.h"
#include "language_detector.h"
#include "text_classifier.h"

#define ULTIMATE_BUFFER_LEN 10240

namespace inagist {

class GistMaker {

 public:

  GistMaker();
  ~GistMaker();

  int Init(const char* keytuples_extracter_config_file,
           const char* language_detection_config_file,
           const char* text_classification_config_file,
           const char* sentiment_analyser_config_file);

  int GetGist(const std::string& text);

  int GetGist(const std::string& text,
              std::string& safe_status,
              std::string& script,
              std::string& lang,
              std::set<std::string>& keywords,
              std::set<std::string>& hashtags,
              std::set<std::string>& keyphrases,
              std::set<std::string>& text_classes
#ifdef CLASS_CONTRIBUTORS_ENABLED
              , std::map<std::string, std::string>& text_class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
              , std::string& sentiment);

  int GetGist(const unsigned char* text, const unsigned int text_len,
        char* safe_status_buffer, const unsigned int safe_status_buffer_len,
        char* script_buffer, const unsigned int script_buffer_len,
        char* languages_buffer, const unsigned int languages_buffer_len,
        unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
        unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
        unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
        unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
        unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
        unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr,
        char* text_classes_buffer, const unsigned int text_classes_buffer_len,
        unsigned int* text_classes_len_ptr, unsigned int* text_classes_count_ptr
#ifdef CLASS_CONTRIBUTORS_ENABLED
        , unsigned char* text_class_contributors_buffer, const unsigned int text_class_contributors_buffer_len,
        unsigned int* text_class_contributors_len_ptr, unsigned int* text_class_contributors_count_ptr
#endif // CLASS_CONTRIBUTORS_ENABLED
        , char* sentiment_buffer, const unsigned int sentiment_buffer_len);

  int FindTextClasses(inagist_classifiers::Corpus& corpus,
        char* text_classes_buffer, const unsigned int text_classes_buffer_len,
        unsigned int& text_classes_len, unsigned int& text_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
        , unsigned char* text_class_contributors_buffer, const unsigned int& text_class_contributors_buffer_len,
        unsigned int& text_class_contributors_len, unsigned int& text_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
       );

 private:

  inagist_trends::KeyTuplesExtracter m_keytuples_extracter;
  inagist_classifiers::LanguageDetector m_language_detector;
  inagist_classifiers::TextClassifier m_text_classifier;

  char m_buffer[1024];
  bool m_disable_text_classification;
  bool m_disable_lang_classification;
};

} // namespace inagist

#endif // _INAGIST_GIST_MAKER_H_
