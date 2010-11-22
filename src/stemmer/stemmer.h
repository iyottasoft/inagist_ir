#ifndef _INAGIST_SEARCH_STEMMER_H_
#define _INAGIST_SEARCH_STEMMER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <set>
#include "string_utils.h"
#include "dictionary.h"

namespace inagist_search {

#define MAX_STEM_TEXT_LEN 1024
#define MAX_STEM_WORD_LEN 255

class Stemmer {
 public:
  Stemmer();
  ~Stemmer();
  int Init(const char *stopwords_file,
           const char *dictionary_file,
           const char *stemmer_dictionary_file);
  int Stem(const std::string& text, std::set<std::string>& stems);
  unsigned int Stem(const std::string& text,
                    const unsigned int& output_buffer_len,
                    char*& pipe_delimited_output);

 private:
  char m_buffer[MAX_STEM_TEXT_LEN];
  char m_stemmed_word[MAX_STEM_WORD_LEN];
  inagist_utils::Dictionary m_exclude_dictionary;
  inagist_utils::Dictionary m_include_dictionary;
  inagist_utils::StringUtils m_utils;
  DISALLOW_COPY_AND_ASSIGN(Stemmer); 
};

} // inagist_search 
#endif // _INAGIST_SEARCH_STEMMER_H_
