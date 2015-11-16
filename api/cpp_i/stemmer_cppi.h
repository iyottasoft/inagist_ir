/* stemmer.h */

#ifndef _INAGIST_SEARCH_STEMMER_ERL_INTERFACE_H_
#define _INAGIST_SEARCH_STEMMER_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitStemmer(const char* stopwords_file_path,
                const char* dictionary_file_path,
                const char* stemmer_dictionary_file_path);
int Stem(const unsigned char* tweet, const unsigned int tweet_len,
         unsigned char* stem_buffer, const unsigned int stems_buffer_len);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_SEARCH_STEMMER_ERL_INTERFACE_H_
