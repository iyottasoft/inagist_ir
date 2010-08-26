#ifndef _INAGIST_TRENDS_KEYWORDS_EXTRACT_H_
#define _INAGIST_TRENDS_KEYWORDS_EXTRACT_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <ext/hash_set>
#include <string>
#include <set>
#include <fstream>
#include <iostream>

namespace __gnu_cxx
{
  template<> struct hash< std::string > {
    size_t operator()( const std::string& x ) const {
      return hash< const char* >()( x.c_str() );
    }
  };
}

namespace inagist_trends {

typedef __gnu_cxx::hash_set<std::string, __gnu_cxx::hash<std::string> > string_hash_set;

class KeywordsExtract {
 public:
  KeywordsExtract();
  ~KeywordsExtract();
  int Init(const char *stopwords_file=NULL,
           const char *dictionary_file=NULL,
           const char *stemmer_dictionary_file=NULL,
           const char *input_file=NULL,
           const char *output_file=NULL);
  int DeInit();
  int GetKeywords(); // not implemented yet, to be used for testing
  int GetKeywords(char* str, std::set<std::string> &keywords_set);
  void printKeywords(); // not implemented yet, to be used for testing
  void PrintKeywords(std::set<std::string> &keywords_set);
  int DictionaryLookup(char *word);

 private:
  std::ifstream m_tweet_stream;
  std::ofstream m_out_stream;
  string_hash_set m_dictionary;
  string_hash_set m_stopwords_dictionary;

  DISALLOW_COPY_AND_ASSIGN(KeywordsExtract);
  int LoadDictionary(const char* file, string_hash_set &dictionary);
  int PrintDictionary(string_hash_set dictionary);
  bool IsPunct(char *ptr, char *prev=NULL, char *next=NULL);
  bool IsIgnore(char **ptr);
};

} // namespace inagist_trends

#endif // _INAGIST_TRENDS_KEYWORDS_EXTRACT_H_
