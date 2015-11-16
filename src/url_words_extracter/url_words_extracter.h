#ifndef _INAGIST_TRENDS_URL_WORDS_EXTRACTER_H_
#define _INAGIST_TRENDS_URL_WORDS_EXTRACTER_H_

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

namespace inagist_trends {

class URLwordsExtracter {
 public:
  // functions
  URLwordsExtracter();
  ~URLwordsExtracter();

  int Init(const char* stopwords_file,
           const char* dictionary_file,
           const char* unsafe_dictionary_file
          );
  int Clear();
  int SetDebugLevel(unsigned int& debug_level);
  int LoadClassifierDictionary(const char* classifier_dictionary_file);

  int GetURLwords(unsigned char* text_buffer, const unsigned int& text_buffer_len,
                  const unsigned int& text_len,
                  unsigned char* url_words_buffer,
                  const unsigned int& url_words_buffer_len,
                  unsigned int& url_words_len,
                  unsigned int& url_words_count);

  inline static void Insert(unsigned char* buffer, unsigned int& current_len,
                   unsigned char* str_to_add, const unsigned int& str_len,
                   unsigned int& buffer_content_count);

  static int GetURLwords(unsigned char* text_buffer, const unsigned int& text_buffer_len,
                         const unsigned int& text_len,
                         inagist_utils::DictionarySet& dictionary,
                         inagist_utils::DictionarySet& stopwords_dictionary,
                         inagist_utils::DictionarySet& unsafe_dictionary,
                         unsigned char* url_words_buffer,
                         const unsigned int& url_words_buffer_len,
                         unsigned int& url_words_len,
                         unsigned int& url_words_count,
                         unsigned int debug_level=0
                        );

 private:
  unsigned int m_debug_level;
  inagist_utils::DictionarySet m_dictionary;
  inagist_utils::DictionarySet m_stopwords_dictionary;
  inagist_utils::DictionarySet m_unsafe_dictionary;
  DISALLOW_COPY_AND_ASSIGN(URLwordsExtracter);
};

} // namespace inagist_trends

#endif // _INAGIST_TRENDS_URL_WORDS_EXTRACTER_H_
