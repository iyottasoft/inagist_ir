#ifndef _INAGIST_TRENDS_KEYWORDS_EXTRACT_H_
#define _INAGIST_TRENDS_KEYWORDS_EXTRACT_H_

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
#include "dictionary.h"
#include "language_detector.h"

#ifndef HASHTAGS_ENABLED
#define HASHTAGS_ENABLED 1
#endif

#ifndef KEYPHRASE_ENABLED
#define KEYPHRASE_ENABLED 1
#endif

namespace inagist_trends {

class KeywordsExtract {
 public:
  KeywordsExtract();
  ~KeywordsExtract();
  int Init(const char* stopwords_file=NULL,
           const char* dictionary_file=NULL,
           const char* unsafe_dictionary_file=NULL,
           const char* stemmer_dictionary_file=NULL,
           const char* lang_detect_config_file=NULL,
           const char* input_file=NULL,
           const char* output_file=NULL);
  int DeInit();
  int GetKeywords(); // not implemented yet, to be used for testing
  int GetKeywords(char* str,
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
int GetKeywords(char* str,
                std::string& safe_status,
                std::string& script,
                std::set<std::string>& keywords_set,
                std::set<std::string>& hashtags_set,
                std::set<std::string>& keyphrases_set);
#endif
#endif

#ifdef KEYPHRASE_ENABLED
#ifndef HASHTAGS_ENABLED
int GetKeywords(char* str,
                std::string& safe_status,
                std::string& script,
                std::set<std::string>& keywords_set,
                std::set<std::string>& keyphrases_set);
#endif
#endif

#ifndef KEYPHRASE_ENABLED
#ifdef HASHTAGS_ENABLED
int GetKeywords(char* str,
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
  int GetKeywords(unsigned char* buffer, const unsigned int& buffer_len,
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

  int GetKeywords(char* buffer, const unsigned int& buffer_len,
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
  inagist_utils::Dictionary m_dictionary;
  inagist_utils::Dictionary m_stopwords_dictionary;
  inagist_utils::Dictionary m_unsafe_dictionary;
  inagist_classifiers::LanguageDetector m_language_detector;

  DISALLOW_COPY_AND_ASSIGN(KeywordsExtract);
  bool IsPunct(char *ptr, char *prev=NULL, char *next=NULL);
  bool IsIgnore(char *&ptr);
};

} // namespace inagist_trends

#endif // _INAGIST_TRENDS_KEYWORDS_EXTRACT_H_
