/* stemmer_erl_interface.cc */

#ifdef _CPLUSPLUS
#include <set>
#include <cstring>
#include <cstdlib>
#include <iostream>
#endif
#include "stemmer.h"
#include "twitter_api.h"
#include "twitter_searcher.h"

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
int SubmitTweet(const unsigned char* tweet, const unsigned int tweet_len,
                unsigned char* stem_buffer, const unsigned int stems_buffer_len) {
#ifdef DEBUG
  std::cout << tweet << std::endl;
#endif
  return g_stemmer.Stem(std::string((char *) tweet), stems_buffer_len, stem_buffer); 
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetTestTweets(const char* user_name,
                  const unsigned int in_length,
                  unsigned char* tweets_buffer,
                  unsigned int *out_length) {

  if (!tweets_buffer)
    return -1;

  int num_docs = 0;

  // get tweets
  std::set<std::string> tweets;

  if (NULL == user_name) {
    inagist_api::TwitterAPI twitter_api;
    twitter_api.GetPublicTimeLine(tweets);
  } else {
    inagist_api::TwitterSearcher twitter_searcher;
    twitter_searcher.GetTweetsFromUser(std::string(user_name), tweets);
  }

  // write them to the output buffer
  std::set<std::string>::iterator iter;
  unsigned char *ptr = tweets_buffer;
  unsigned int len = 0;
  unsigned int total_len = 0;
  for (iter = tweets.begin(); iter != tweets.end(); iter++) {
    len = (*iter).length();
    total_len += (len + 1); // 1 for the pipe
    if (total_len < in_length) {
      strcpy((char *) ptr, (*iter).c_str());
      ptr += len;
      strcpy((char *) ptr, "|");
      ptr++;
      num_docs++;
    } else {
#ifdef DEBUG
      std::cout << "Not enuf space in the tweets buffer\n";
#endif
      break;
    }
  }
  *ptr = '\0';
  *out_length = ptr - tweets_buffer;
  tweets.clear();
  ptr = NULL;

  return num_docs;
}

