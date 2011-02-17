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
#include "dictionary_set.h"

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
  int Stem(const std::string& text,
           const unsigned int& output_buffer_len,
           unsigned char*& pipe_delimited_output);
  int Clear();

 private:
  unsigned char m_buffer[MAX_STEM_TEXT_LEN];
  unsigned char m_stemmed_word[MAX_STEM_WORD_LEN];
  unsigned char* m_max_end;
  inagist_utils::DictionarySet m_exclude_dictionary;
  inagist_utils::DictionarySet m_include_dictionary;
  DISALLOW_COPY_AND_ASSIGN(Stemmer); 
};

} // inagist_search 
#endif // _INAGIST_SEARCH_STEMMER_H_
