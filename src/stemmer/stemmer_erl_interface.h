/* stemmer_erl_interface.h */

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
int SubmitTweet(const unsigned char* tweet, const unsigned int tweet_len,
                unsigned char* stem_buffer, const unsigned int stems_buffer_len);
int GetTestTweets(const char* user_name,
                  const unsigned int in_length,
                  unsigned char* tweets_buffer,
                  unsigned int *out_length);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_SEARCH_STEMMER_ERL_INTERFACE_H_
