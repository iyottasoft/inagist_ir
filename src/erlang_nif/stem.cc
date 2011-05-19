/* stem.cc */

#ifdef _CPLUSPLUS
#include <iostream>
#endif

#include "stem.h"
#include "stemmer.h"

inagist_search::Stemmer g_stemmer;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitStemmer(const char* stopwords_file_path,
                const char* dictionary_file_path,
                const char* stemmer_dictionary_file_path) {

  if (!stopwords_file_path || !dictionary_file_path || !stemmer_dictionary_file_path)
    return -1;

  if (g_stemmer.Init(stopwords_file_path, dictionary_file_path, stemmer_dictionary_file_path) < 0)
    return -1;

  return 0;
}

// keywords and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int Stem(const unsigned char* text, const unsigned int tweet_len,
         unsigned char* stem_buffer, const unsigned int stems_buffer_len) {
  return g_stemmer.Stem(text, stems_buffer_len, stem_buffer); 
}

