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

namespace inagist_trends {

class KeywordsExtract {
 public:
  KeywordsExtract();
  ~KeywordsExtract();
  int Init(const char* stopwords_file=NULL,
           const char* dictionary_file=NULL,
           const char* unsafe_dictionary_file=NULL,
           const char* stemmer_dictionary_file=NULL,
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

  // directly writing to an output buffer instead of a set
  int GetKeywords(unsigned char* buffer, const unsigned int& buffer_len,
                  char* safe_status_buffer, const unsigned int& safe_status_buffer_len,
                  char* script_buffer, const unsigned int& script_buffer_len,
                  unsigned char* keywords_buffer, const unsigned int& keywords_buffer_len,
                  unsigned int& keywords_len, unsigned int& keywords_count,
                  unsigned char* keyphrases_buffer, const unsigned int& keyphrases_buffer_len,
                  unsigned int& keyphrases_len, unsigned int& keyphrases_count);

  int GetKeywords(char* buffer, const unsigned int& buffer_len,
                  char* safe_status_buffer, const unsigned int& safe_status_buffer_len,
                  char* script_buffer, const unsigned int& script_buffer_len,
                  char* keywords_buffer, const unsigned int& keywords_buffer_len,
                  unsigned int& keywords_len, unsigned int& keywords_count);

  void printKeywords(); // not implemented yet, to be used for testing
  void PrintKeywords(std::set<std::string> &keywords_set);
  int DetectScript(int code_point, std::string &script);

 private:
  std::ifstream m_tweet_stream;
  std::ofstream m_out_stream;
  inagist_utils::Dictionary m_dictionary;
  inagist_utils::Dictionary m_stopwords_dictionary;
  inagist_utils::Dictionary m_unsafe_dictionary;

  DISALLOW_COPY_AND_ASSIGN(KeywordsExtract);
  bool IsPunct(char *ptr, char *prev=NULL, char *next=NULL);
  bool IsIgnore(char *&ptr);
};

} // namespace inagist_trends

#endif // _INAGIST_TRENDS_KEYWORDS_EXTRACT_H_
