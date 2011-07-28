#ifndef _INAGIST_TRENDS_KEYTUPLES_EXTRACT_H_
#define _INAGIST_TRENDS_KEYTUPLES_EXTRACT_H_

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
#define KE_DEBUG DEBUG
#endif
#endif
//#define KE_DEBUG 6

namespace inagist_trends {

class KeyTuplesExtracter {
 public:
  // variables
  inagist_utils::DictionarySet m_dictionary;

  // functions
  KeyTuplesExtracter();
  ~KeyTuplesExtracter();

  int Init(std::string config_file,
           bool load_classifier_dictionary=false);
  int Init(const char* stopwords_file,
           const char* dictionary_file,
           const char* unsafe_dictionary_file,
           const char* stemmer_dictionary_file=NULL,
#ifdef INTENT_ENABLED
           const char* intent_words_file=NULL,
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
           const char* sentiment_words_file=NULL,
#endif // SENTIMENT_ENABLED
           const char* input_file=NULL,
           const char* output_file=NULL);
  int LoadClassifierDictionary(const char* classifier_dictionary_file);
  int DeInit();

#ifdef KEYWORDS_ENABLED
  int GetKeywords(char* str,
                  std::string& safe_status,
                  std::string& script,
                  std::set<std::string>& keywords_set);

  int GetKeywords(char *str,
                  std::string& user,
                  std::set<std::string>& keywords_set,
                  std::map<std::string, std::string>& script_user_map,
                  std::map<std::string, std::string>& keyword_user_map);
#endif // KEYWORDS_ENABLED

  int GetKeyTuples(char* str,
                   std::string& safe_status,
                   std::string& script
#ifdef KEYWORDS_ENABLED
                   , std::set<std::string>& keywords_set
#endif // KEYWORDS_ENABLED
#ifdef HASHTAGS_ENABLED
                   , std::set<std::string>& hashtags_set
#endif // HASHTAGS_ENABLED
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
                   , std::string& intent
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                   , std::string& sentiment
#endif // SENTIMENT_ENABLED
                  );

  // directly writing to an output buffer instead of a set
  int GetKeyTuples(unsigned char* text_buffer, const unsigned int& text_buffer_len,
                   const unsigned int& text_len,
                   char* safe_status_buffer, const unsigned int& safe_status_buffer_len,
                   char* script_buffer, const unsigned int& script_buffer_len
#ifdef KEYWORDS_ENABLED
                   , unsigned char* keywords_buffer, const unsigned int& keywords_buffer_len,
                   unsigned int& keywords_len, unsigned int& keywords_count
#endif // KEYWORDS_ENABLED
#ifdef HASHTAGS_ENABLED
                   , unsigned char* hashtags_buffer, const unsigned int& hashtags_buffer_len,
                   unsigned int& hashtags_len, unsigned int& hashtags_count
#endif // HASHTAGS_ENABLED
#ifdef KEYPHRASE_ENABLED
                   , unsigned char* keyphrases_buffer, const unsigned int& keyphrases_buffer_len,
                   unsigned int& keyphrases_len, unsigned int& keyphrases_count
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                   , unsigned char* lang_words_buffer, const unsigned int& lang_words_buffer_len,
                   unsigned int& lang_words_len, unsigned int& lang_words_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                   , unsigned char* text_class_words_buffer, const unsigned int& text_class_words_buffer_len,
                   unsigned int& text_class_words_len, unsigned int& text_class_words_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                   , char* intent_buffer, const unsigned int& intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                   , char* sentiment_buffer, const unsigned int& sentiment_buffer_len
#endif // SENTIMENT_ENABLED
                  );

  void PrintKeywords(std::set<std::string> &keywords_set);
  int DetectScript(int code_point, std::string &script);

 private:
  std::ifstream m_tweet_stream;
  std::ofstream m_out_stream;
  inagist_utils::DictionarySet m_stopwords_dictionary;
  inagist_utils::DictionarySet m_unsafe_dictionary;
#ifdef INTENT_ENABLED
  inagist_utils::IntDictionaryMap m_intent_words_dictionary;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  inagist_utils::IntDictionaryMap m_sentiment_words_dictionary;
#endif // SENTIMENT_ENABLED

  DISALLOW_COPY_AND_ASSIGN(KeyTuplesExtracter);
  // using char* for word_has_apostrophe instead of bool&
  bool IsPunct(char*& ptr, char* prev=NULL, char* next=NULL/*, bool word_has_apostrophe=false*/);
  bool IsIgnore(char*& ptr);
  inline void Insert(unsigned char* buffer, unsigned int& current_len,
                     unsigned char* str_to_add, const unsigned int& str_len,
                     unsigned int& buffer_content_count);
  inline void Insert(unsigned char* buffer, unsigned int& current_len,
                     std::string& str, unsigned int& buffer_content_count);
};

} // namespace inagist_trends

#endif // _INAGIST_TRENDS_KEYTUPLES_EXTRACT_H_
