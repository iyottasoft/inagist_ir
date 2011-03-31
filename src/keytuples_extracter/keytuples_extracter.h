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

#ifdef LANG_DETECT
#include "language_detector.h"
#endif

#ifndef HASHTAGS_ENABLED
#define HASHTAGS_ENABLED 1
#endif

#ifndef KEYPHRASE_ENABLED
#define KEYPHRASE_ENABLED 1
#endif

namespace inagist_trends {

class KeyTuplesExtracter {
 public:
  KeyTuplesExtracter();
  ~KeyTuplesExtracter();
  int Init(std::string config_file);
  int Init(const char* stopwords_file,
           const char* dictionary_file,
           const char* unsafe_dictionary_file,
           const char* lang_detect_config_file=NULL,
           const char* channels_dictionary_file=NULL,
           const char* stemmer_dictionary_file=NULL,
           const char* input_file=NULL,
           const char* output_file=NULL);
  int DeInit();
  int GetKeywords(); // not implemented yet, to be used for testing
  int GetKeyTuples(char* str,
                  std::string& safe_status,
                  std::string& script,
                  std::set<std::string>& keywords_set,
                  std::set<std::string>& keyphrases_set);
  int GetKeywords(char* str,
                  std::string& script,
                  std::set<std::string>& keywords_set);
  int GetKeywords(char* str,
                  std::string& safe_status,
                  std::string& script,
                  std::set<std::string>& keywords_set);
  int GetKeywords(char* str,
                  std::set<std::string> &keywords_set);
  int GetKeywords(char *str,
                  std::string& user,
                  std::set<std::string>& keywords_set,
                  std::map<std::string, std::string>& script_user_map,
                  std::map<std::string, std::string>& keyword_user_map);

#ifdef KEYPHRASE_ENABLED
#ifdef HASHTAGS_ENABLED
int GetKeyTuples(char* str,
                 std::string& safe_status,
                 std::string& script,
                 std::set<std::string>& keywords_set,
                 std::set<std::string>& hashtags_set,
                 std::set<std::string>& keyphrases_set);
#endif
#endif

#ifdef KEYPHRASE_ENABLED
#ifndef HASHTAGS_ENABLED
int GetKeyTuples(char* str,
                 std::string& safe_status,
                 std::string& script,
                 std::set<std::string>& keywords_set,
                 std::set<std::string>& keyphrases_set);
#endif
#endif

#ifndef KEYPHRASE_ENABLED
#ifdef HASHTAGS_ENABLED
int GetKeyTuples(char* str,
                 std::string& safe_status,
                 std::string& script,
                 std::set<std::string>& keywords_set,
                 std::set<std::string>& hashtags_set);
#endif
#endif

#ifndef KEYPHRASE_ENABLED
#ifndef HASHTAGS_ENABLED
int GetKeywords(char* str,
                std::string& safe_status,
                std::string& script,
                std::set<std::string>& keywords_set);
#endif
#endif

  // directly writing to an output buffer instead of a set
  int GetKeyTuples(unsigned char* buffer, const unsigned int& buffer_len,
                  char* safe_status_buffer, const unsigned int& safe_status_buffer_len,
                  char* script_buffer, const unsigned int& script_buffer_len,
                  unsigned char* keywords_buffer, const unsigned int& keywords_buffer_len,
                  unsigned int& keywords_len, unsigned int& keywords_count,
                  unsigned char* hashtags_buffer, const unsigned int& hashtags_buffer_len,
                  unsigned int& hashtags_len, unsigned int& hashtags_count,
                  unsigned char* keyphrases_buffer, const unsigned int& keyphrases_buffer_len,
                  unsigned int& keyphrases_len, unsigned int& keyphrases_count,
                  char* buffer1, const unsigned int& buffer1_len,
                  char* buffer2, const unsigned int& buffer2_len,
                  char* buffer3, const unsigned int& buffer3_len,
                  char* buffer4, const unsigned int& buffer4_len);

  int GetKeyTuples(unsigned char* buffer, const unsigned int buffer_len,
                  char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                  char* script_buffer, const unsigned int script_buffer_len,
                  unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                  unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                  unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                  unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                  unsigned char* keyphrases_buffer, const unsigned int& keyphrases_buffer_len,
                  unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr) {
    return 0;
  }

  int GetKeyTuples(char* buffer, const unsigned int& buffer_len,
                  char* safe_status_buffer, const unsigned int& safe_status_buffer_len,
                  char* script_buffer, const unsigned int& script_buffer_len,
                  char* keywords_buffer, const unsigned int& keywords_buffer_len,
                  unsigned int& keywords_len, unsigned int& keywords_count,
                  char* buffer1, const unsigned int& buffer1_len,
                  char* buffer2, const unsigned int& buffer2_len,
                  char* buffer3, const unsigned int& buffer3_len,
                  char* buffer4, const unsigned int& buffer4_len);

  void printKeywords(); // not implemented yet, to be used for testing
  void PrintKeywords(std::set<std::string> &keywords_set);
  int DetectScript(int code_point, std::string &script);

 private:
  std::ifstream m_tweet_stream;
  std::ofstream m_out_stream;
  inagist_utils::DictionarySet m_dictionary;
  inagist_utils::DictionarySet m_stopwords_dictionary;
  inagist_utils::DictionarySet m_unsafe_dictionary;
#ifdef LANG_DETECT
  inagist_classifiers::LanguageDetector m_language_detector;
#endif
  inagist_utils::DictionaryMap m_channels_dictionary_map;

  DISALLOW_COPY_AND_ASSIGN(KeyTuplesExtracter);
  bool IsPunct(char *ptr, char *prev=NULL, char *next=NULL);
  bool IsIgnore(char *&ptr);
  inline void Insert(unsigned char* buffer, unsigned int& current_len,
                     unsigned char* str_to_add, const unsigned int& str_len,
                     unsigned int& buffer_content_count);
  inline void Insert(unsigned char* buffer, unsigned int& current_len,
                     std::string& str, unsigned int& buffer_content_count);
};

} // namespace inagist_trends

#endif // _INAGIST_TRENDS_KEYTUPLES_EXTRACT_H_
