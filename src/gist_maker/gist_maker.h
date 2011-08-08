/* gist_maker.h */

#ifndef _INAGIST_GIST_MAKER_H_
#define _INAGIST_GIST_MAKER_H_

#include <string>
#include "keytuples_extracter.h"
#ifdef LANG_ENABLED
#include "language_detector.h"
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
#include "text_classifier.h"
#endif // TEXT_CLASSIFICATION_ENABLED

#define ULTIMATE_BUFFER_LEN 10240

namespace inagist {

class GistMaker {

 public:

  GistMaker();
  ~GistMaker();

int Init(const char* keytuples_extracter_config_file
#ifdef LANG_ENABLED
         , const char* language_detection_config_file
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
         , const char* text_classification_config_file
#endif // TEXT_CLASSIFICATION_ENABLED
        );

  int GetGist(const std::string& text);

  int GetGist(const std::string& text,
                       std::string& safe_status,
                       std::string& script
#ifdef LANG_ENABLED
                       , std::string& lang
#endif // LANG_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
                       , std::set<std::string>& named_entities
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                       , std::set<std::string>& keywords
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                       , std::set<std::string>& keyphrases
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                       , std::set<std::string>& text_classes
#ifdef CLASS_CONTRIBUTORS_ENABLED
                       , std::map<std::string, std::string>& text_class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                       , std::string& intent
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                       , std::string& sentiment
#endif // SENTIMENT_ENABLED
                      );

int GetGist(unsigned char* text_buffer, const unsigned int text_buffer_len,
      const unsigned int text_len,
      char* safe_status_buffer, const unsigned int safe_status_buffer_len,
      char* script_buffer, const unsigned int script_buffer_len
#ifdef NAMED_ENTITIES_ENABLED
      , unsigned char* named_entities_buffer, const unsigned int named_entities_buffer_len,
      unsigned int* named_entities_len_ptr, unsigned int* named_entities_count_ptr
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
      , unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
      unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
      , unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
      unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
      , unsigned char* lang_class_words_buffer, const unsigned int lang_class_words_buffer_len,
      unsigned int* lang_class_words_len_ptr, unsigned int* lang_class_words_count_ptr,
      char* lang_class_buffer, const unsigned int lang_class_buffer_len,
      unsigned int* lang_class_len_ptr, unsigned int* lang_class_count_ptr,
      char* top_lang_classes_buffer, const unsigned int top_lang_classes_buffer_len,
      unsigned int* top_lang_classes_len_ptr, unsigned int* top_lang_classes_count_ptr
#ifdef CLASS_CONTRIBUTORS_ENABLED
      , unsigned char* lang_class_contributors_buffer, const unsigned int lang_class_contributors_buffer_len,
      unsigned int* lang_class_contributors_len_ptr, unsigned int* lang_class_contributors_count_ptr
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , unsigned char* text_class_words_buffer, const unsigned int text_class_words_buffer_len,
      unsigned int* text_class_words_len_ptr, unsigned int* text_class_words_count_ptr,
      char* text_class_buffer, const unsigned int text_class_buffer_len,
      unsigned int* text_class_len_ptr, unsigned int* text_class_count_ptr,
      char* top_text_classes_buffer, const unsigned int top_text_classes_buffer_len,
      unsigned int* top_text_classes_len_ptr, unsigned int* top_text_classes_count_ptr
#ifdef CLASS_CONTRIBUTORS_ENABLED
      , unsigned char* text_class_contributors_buffer, const unsigned int text_class_contributors_buffer_len,
      unsigned int* text_class_contributors_len_ptr, unsigned int* text_class_contributors_count_ptr
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
      , char* intent_buffer, const unsigned int intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
      , char* sentiment_buffer, const unsigned int sentiment_buffer_len
#endif // SENTIMENT_ENABLED
     );

#ifdef TEXT_CLASSIFICATION_ENABLED
  int FindTextClasses(inagist_classifiers::Corpus& corpus,
        char* text_classes_buffer, const unsigned int text_classes_buffer_len,
        unsigned int& text_classes_len, unsigned int& text_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
        , unsigned char* text_class_contributors_buffer, const unsigned int& text_class_contributors_buffer_len,
        unsigned int& text_class_contributors_len, unsigned int& text_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
       );
#endif // TEXT_CLASSIFICATION_ENABLED

 private:

  inagist_trends::KeyTuplesExtracter m_keytuples_extracter;
#ifdef LANG_ENABLED
  inagist_classifiers::LanguageDetector m_language_detector;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  inagist_classifiers::TextClassifier m_text_classifier;
#endif // TEXT_CLASSIFICATION_ENABLED

  char m_buffer[1024];
};

} // namespace inagist

#endif // _INAGIST_GIST_MAKER_H_
