#ifndef _INAGIST_TRENDS_KEYTUPLES_EXTRACTER_H_
#define _INAGIST_TRENDS_KEYTUPLES_EXTRACTER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <set>
#include <map>
#include <fstream>
#include <iostream>
#include "dictionary_set.h"
#include "dictionary_map.h"
#include "int_dictionary_map.h"

#ifdef DEBUG
#if DEBUG>0
#define GM_DEBUG DEBUG
#endif
#endif
//#define GM_DEBUG 6

#define I18N_ENABLED 1
#define PROFANITY_CHECK_ENABLED 1
#define SCRIPT_DETECTION_ENABLED 1

namespace inagist_trends {

class KeyTuplesExtracter {
 public:
  // functions
  KeyTuplesExtracter();
  ~KeyTuplesExtracter();

  int Init(std::string config_file);
  int Init(const char* stopwords_file,
           const char* dictionary_file,
           const char* unsafe_dictionary_file,
           const char* stemmer_dictionary_file=NULL
#ifdef INTENT_ENABLED
           , const char* intent_words_file=NULL
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
           , const char* sentiment_words_file=NULL
#endif // SENTIMENT_ENABLED
          );
  int Clear();
  int SetDebugLevel(unsigned int& debug_level);
  int LoadClassifierDictionary(const char* classifier_dictionary_file);

  int GetKeyTuples(char* str
#ifdef PROFANITY_CHECK_ENABLED
                   , std::string& profanity_status
#endif // PROFANITY_CHECK_ENABLED
#ifdef SCRIPT_DETECTION_ENABLED
                   , std::string& script
#endif // SCRIPT_DETECTION_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
                   , std::set<std::string>& named_entities_set
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                   , std::set<std::string>& keywords_set
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                   , std::set<std::string>& keyphrases_set
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                   , std::set<std::string>& lang_words_set
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                   , std::set<std::string>& text_class_words_set
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                   , int& intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                   , int& sentiment_valence
#endif // SENTIMENT_ENABLED
                );

  int GetKeyTuples(unsigned char* text_buffer, const unsigned int& text_buffer_len,
                   const unsigned int& text_len
#ifdef PROFANITY_CHECK_ENABLED
                   , char* profanity_status_buffer, const unsigned int& profanity_status_buffer_len
#endif // PROFANITY_CHECK_ENABLED
#ifdef SCRIPT_DETECTION_ENABLED
                   , char* script_buffer, const unsigned int& script_buffer_len
#endif // SCRIPT_DETECTION_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
                   , unsigned char* named_entities_buffer,
                   const unsigned int& named_entities_buffer_len,
                   unsigned int& named_entities_len,
                   unsigned int& named_entities_count
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                   , unsigned char* keywords_buffer,
                   const unsigned int& keywords_buffer_len,
                   unsigned int& keywords_len,
                   unsigned int& keywords_count
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                   , unsigned char* keyphrases_buffer,
                   const unsigned int& keyphrases_buffer_len,
                   unsigned int& keyphrases_len,
                   unsigned int& keyphrases_count
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                   , unsigned char* lang_words_buffer,
                   const unsigned int& lang_words_buffer_len,
                   unsigned int& lang_words_len,
                   unsigned int& lang_words_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                   , unsigned char* text_class_words_buffer,
                   const unsigned int& text_class_words_buffer_len,
                   unsigned int& text_class_words_len,
                   unsigned int& text_class_words_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                   , int &intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                   , int &sentiment_valence
#endif // SENTIMENT_ENABLED
                );

  int GetURLwords(unsigned char* url_buffer, const unsigned int& url_buffer_len,
                  const unsigned int& url_len,
                  unsigned char* url_words_buffer,
                  const unsigned int& url_words_buffer_len,
                  unsigned int& url_words_len,
                  unsigned int& url_words_count);

 private:
  // variables
  unsigned int m_debug_level;
  inagist_utils::DictionarySet m_dictionary;
  inagist_utils::DictionarySet m_stopwords_dictionary;
  inagist_utils::DictionarySet m_unsafe_dictionary;
#ifdef INTENT_ENABLED
  inagist_utils::IntDictionaryMap m_intent_words_dictionary;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  inagist_utils::IntDictionaryMap m_sentiment_words_dictionary;
#endif // SENTIMENT_ENABLED

  // functions
  bool IsIgnore(char*& ptr, int& ignore_intent);
  inline void Insert(unsigned char* buffer, unsigned int& current_len,
                     unsigned char* str_to_add, const unsigned int& str_len,
                     unsigned int& buffer_content_count);
  inline void Insert(unsigned char* buffer, unsigned int& current_len,
                     std::string& str, unsigned int& buffer_content_count);
  inline void CopyMapInto(std::map<std::string, int>& sentence_intent_words,
                          std::map<std::string, int>& intent_words);
  DISALLOW_COPY_AND_ASSIGN(KeyTuplesExtracter);
};

} // namespace inagist_trends

#endif // _INAGIST_TRENDS_KEYTUPLES_EXTRACTER_H_
