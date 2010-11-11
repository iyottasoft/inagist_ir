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

char g_buffer[MAX_BUFFER_SIZE];
inagist_trends::KeywordsExtract g_keywords_extract;
inagist_trends::KeywordsManager g_keywords_manager;
#ifdef _CPLUSPLUS
std::set<std::string> g_keywords_set;
std::set<std::string> g_keyphrases_set;
#endif

#ifdef _CPLUSPLUS
extern "C"
#endif
int Init(const char* stopwords_file_path, const char* dictionary_file_path) {
  if (!stopwords_file_path || !dictionary_file_path)
    return -1;

  if (g_keywords_extract.Init(stopwords_file_path, dictionary_file_path) < 0)
    return -1;

  memset(g_buffer, 0, MAX_BUFFER_SIZE);
  return 0;
}

// keywords and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int SubmitTweet(const char* tweet, const unsigned int tweet_len,
                char* tweet_script, const unsigned int script_buffer_len,
                char* keywords, const unsigned int keywords_buffer_len,
                char* keyphrases, const unsigned int keyphrases_buffer_len) {
  std::string script;
#ifdef DEBUG
  std::cout << tweet << std::endl;
#endif
  strcpy(g_buffer, tweet);
  int ret_value = 0;
  if ((ret_value = g_keywords_extract.GetKeywords(g_buffer, script, g_keywords_set, g_keyphrases_set)) <= 0) {
#ifdef DEBUG
    if (ret_value < 0)
      std::cout << "Error: could not get keywords from KeywordsExtract\n";
#endif
    if (script.length() < script_buffer_len)
      strcpy(tweet_script, script.c_str());
    g_buffer[0] = '\0';
    g_keyphrases_set.clear();
    g_keywords_set.clear();
    *keywords = '\0';
    *keyphrases = '\0';
    return ret_value;
  }
  g_buffer[0] = '\0';

  if (script.length() < script_buffer_len)
    strcpy(tweet_script, script.c_str());

  std::set<std::string>::iterator iter;
  char *ptr = keywords;
  unsigned int len = 0;
  unsigned int total_len = 0;
  for (iter = g_keywords_set.begin(); iter != g_keywords_set.end(); iter++) {
    len = (*iter).length();
    total_len += (len + 1); // 1 for the pipe
    if (total_len < keywords_buffer_len) {
      strcpy(ptr, (*iter).c_str());
      ptr += len;
      strcpy(ptr, "|");
      ptr++;
    } else {
#ifdef DEBUG
      std::cout << "Error: Not enuf space in the keywords buffer\n";
#endif
      *keywords = '\0';
      g_keyphrases_set.clear();
      g_keywords_set.clear();
      return -1;
    }
  }
  *ptr = '\0';

  ptr = keyphrases;
  len = 0;
  total_len = 0;
  for (iter = g_keyphrases_set.begin(); iter != g_keyphrases_set.end(); iter++) {
    len = (*iter).length();
    total_len += (len + 1); // 1 for the pipe
    if (total_len < keyphrases_buffer_len) {
      strcpy(ptr, (*iter).c_str());
      ptr += len;
      strcpy(ptr, "|");
      ptr++;
    } else {
#ifdef DEBUG
      std::cout << "Not enuf space in the keyphrase buffer\n";
#endif
      *keyphrases = '\0';
      g_keyphrases_set.clear();
      g_keywords_set.clear();
      return -1;
    }
  }
  *ptr = '\0';

  g_keyphrases_set.clear();
  g_keywords_set.clear();
  ptr = NULL;

  return 0;
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

