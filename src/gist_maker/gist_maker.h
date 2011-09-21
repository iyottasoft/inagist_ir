#ifndef _INAGIST_TRENDS_GIST_MAKER_H_
#define _INAGIST_TRENDS_GIST_MAKER_H_

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
#include "double_dictionary_map.h"
#include "string_to_map_dictionary.h"
#include "ngrams_generator.h"

#ifdef DEBUG
#if DEBUG>0
#define GM_DEBUG DEBUG
#endif
#endif
//#define GM_DEBUG 3

namespace inagist {

class GistMaker {
 public:
  // variables

  // functions
  GistMaker();
  ~GistMaker();

  int SetDebugLevel(unsigned int& debug_level);
  int Init(std::string config_file);
  int Init(const char* stopwords_file,
           const char* dictionary_file,
           const char* unsafe_dictionary_file
#ifdef INTENT_ENABLED
           , const char* intent_words_file=NULL
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
           , const char* sentiment_words_file=NULL
#endif // SENTIMENT_ENABLED
#ifdef LANG_ENABLED
           , const char *language_dictionary_file=NULL
           , const char *language_prior_freqs_file=NULL
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
           , const char *classifier_dictionary_file=NULL
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
           , const char *location_dictionary_file=NULL
#endif // LOCATION_ENABLED
           , const char* stemmer_dictionary_file=NULL
         );
  int DeInit();

  int MakeGist(char* str
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
               , std::string& language
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
               , std::set<std::string>& text_classes_set
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
               , std::set<std::string>& locations_set
#endif // LOCATION_ENABLED
#ifdef INTENT_ENABLED
               , int &intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
               , int &sentiment_valence
#endif // SENTIMENT_ENABLED
              );

  // directly writing to an output buffer instead of a set
  int MakeGist(unsigned char* text_buffer, const unsigned int& text_buffer_len,
               const unsigned int& text_len
#ifdef PROFANITY_CHECK_ENABLED
               , char* profanity_status_buffer, const unsigned int& profanity_status_buffer_len
#endif // PROFANITY_CHECK_ENABLED
#ifdef SCRIPT_DETECTION_ENABLED
               , char* script_buffer, const unsigned int& script_buffer_len
#endif // SCRIPT_DETECTION_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
               , unsigned char* named_entities_buffer, const unsigned int& named_entities_buffer_len,
               unsigned int& named_entities_len, unsigned int& named_entities_count
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
               , unsigned char* keywords_buffer, const unsigned int& keywords_buffer_len,
               unsigned int& keywords_len, unsigned int& keywords_count
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
               , unsigned char* keyphrases_buffer, const unsigned int& keyphrases_buffer_len,
               unsigned int& keyphrases_len, unsigned int& keyphrases_count
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
               , unsigned char* language_buffer, const unsigned int& language_buffer_len,
               unsigned int& language_len, unsigned int& language_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
               , char* text_classes_buffer, const unsigned int& text_classes_buffer_len,
               unsigned int& text_classes_len, unsigned int& text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
               , char* locations_buffer, const unsigned int& locations_buffer_len,
               unsigned int& locations_len, unsigned int& locations_count
#endif // LOCATION_ENABLED
#ifdef INTENT_ENABLED
               , int& intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
               , int& sentiment_valence
#endif // SENTIMENT_ENABLED
              );

  void PrintKeywords(std::set<std::string> &named_entities_set);
  int DetectScript(int code_point, std::string &script);
  int ProcessTextClassWord(std::string& text_class_word, std::map<std::string, double>& text_class_map);
  void inline Initialize(double array[], unsigned int size);
  int Heapify(double& top1, std::string& top1_class,
              double& top2, std::string& top2_class,
              double& top3, std::string& top3_class);

  int ProcessLangClassWord(std::string& lang_class_word,
                           std::map<std::string, double>& lang_class_map);
  int ProcessLocationWord(std::string& locations_word,
                          std::map<std::string, double>& locations_map);

 private:
  unsigned int m_debug_level;
  inagist_utils::DictionarySet m_stopwords_dictionary;
  inagist_utils::DictionarySet m_dictionary;
  inagist_utils::DictionarySet m_unsafe_dictionary;
#ifdef INTENT_ENABLED
  inagist_utils::IntDictionaryMap m_intent_words_dictionary;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  inagist_utils::IntDictionaryMap m_sentiment_words_dictionary;
#endif // SENTIMENT_ENABLED
#ifdef LANG_ENABLED
  inagist_classifiers::NgramsGenerator m_ngrams_generator;
  inagist_utils::StringToMapDictionary m_language_dictionary;
  inagist_utils::DoubleDictionaryMap m_language_prior_freqs;
#endif // LANG_ENABLED 
#ifdef TEXT_CLASSIFICATION_ENABLED
  inagist_utils::StringToMapDictionary m_classifier_dictionary;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
  inagist_utils::StringToMapDictionary m_location_dictionary;
#endif // LOCATION_ENABLED

  DISALLOW_COPY_AND_ASSIGN(GistMaker);
  // using char* for word_has_apostrophe instead of bool&
  bool IsPunct(char*& ptr, char* prev=NULL, char* next=NULL, int* punct_intent=NULL, int* punct_senti=NULL);
  bool IsIgnore(char*& ptr);
  inline void Insert(unsigned char* buffer, unsigned int& current_len,
                     unsigned char* str_to_add, const unsigned int& str_len,
                     unsigned int& buffer_content_count);
  inline void Insert(unsigned char* buffer, unsigned int& current_len,
                     std::string& str, unsigned int& buffer_content_count);
};

} // namespace inagist

#endif // _INAGIST_TRENDS_GIST_MAKER_H_
