/* trends_manager.cc */

#include "trends_manager.h"
#include "twitter_api.h"
#include "twitter_searcher.h"

//#define DEBUG 1

#ifdef _CPLUSPLUS
#include <set>
#include <cstring>
#include <cstdlib>
#endif

#define MAX_BUFFER_SIZE 1024

inagist_trends::KeywordsExtract g_keywords_extract;
inagist_trends::KeywordsManager g_keywords_manager;

#ifdef _CPLUSPLUS
extern "C"
#endif
int Init(const char* stopwords_file_path,
         const char* dictionary_file_path,
         const char* unsafe_dictionary_file_path) {
  if (!stopwords_file_path || !dictionary_file_path || !unsafe_dictionary_file_path)
    return -1;

  if (g_keywords_extract.Init(stopwords_file_path, dictionary_file_path, unsafe_dictionary_file_path) < 0)
    return -1;

  return 0;
}

// keywords and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int SubmitTweet(const char* tweet, const unsigned int tweet_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                char* keywords_buffer, const unsigned int keywords_buffer_len,
                unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr) {

#ifdef DEBUG
  std::cout << tweet << std::endl;
#endif

  // this can be global. keeping it local for the time being
  char buffer[MAX_BUFFER_SIZE];
  if (tweet_len < MAX_BUFFER_SIZE) {
    strcpy(buffer, tweet);
  } else {
    return -1;
  }

  int ret_value = 0;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
  if ((ret_value = g_keywords_extract.GetKeywords(buffer, tweet_len,
                                                  safe_status_buffer, safe_status_buffer_len,
                                                  script_buffer, script_buffer_len,
                                                  keywords_buffer, keywords_buffer_len,
                                                  keywords_len, keywords_count,
                                                  keyphrases_buffer, keyphrases_buffer_len,
                                                  keyphrases_len, keyphrases_count)) <= 0) {
    if (ret_value < 0 ) {
      *safe_status_buffer = '\0';
      *script_buffer = '\0';
#ifdef DEBUG
      std::cout << "Error: could not get keywords from KeywordsExtract\n";
#endif
    }
    *keywords_buffer = '\0';
    *keywords_len_ptr = 0;
    *keywords_count_ptr = 0;
    *keyphrases_buffer = '\0';
    *keyphrases_len_ptr = 0;
    *keyphrases_count_ptr = 0;
  }
  buffer[0] = '\0';
  *keywords_len_ptr = keywords_len;
  *keywords_count_ptr = keywords_count;
  *keyphrases_len_ptr = keyphrases_len;
  *keyphrases_count_ptr = keyphrases_count;

  return ret_value;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetTestTweets(const char* user_name, const unsigned int in_length, char* tweets_buffer, unsigned int *out_length) {

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
  char *ptr = tweets_buffer;
  unsigned int len = 0;
  unsigned int total_len = 0;
  for (iter = tweets.begin(); iter != tweets.end(); iter++) {
    len = (*iter).length();
    total_len += (len + 1); // 1 for the pipe
    if (total_len < in_length) {
      strcpy(ptr, (*iter).c_str());
      ptr += len;
      strcpy(ptr, "|");
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

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetTrends(const char* user_name, char* trends_buffer) {
  if (!user_name || !trends_buffer)
    return -1;
  return 0;
}

