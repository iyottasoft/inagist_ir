/* language_detector_erl_interface.cc */

#ifdef _CPLUSPLUS
#include <set>
#include <cstring>
#include <cstdlib>
#endif

#include "language_detector.h"
#include "twitter_api.h"
#include "twitter_searcher.h"

inagist_classifiers::LanguageDetector g_language_detector;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitLangD(const char* config_file) {

  if (!config_file)
    return -1;

  if (g_language_detector.Init(std::string(config_file)) < 0)
    return -1;

  return 0;
}

// keywords and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int SubmitTweet(const char* tweet, const unsigned int tweet_len,
                char* lang_buffer, const unsigned int lang_buffer_len) {
#ifdef DEBUG
  std::cout << tweet << std::endl;
#endif
  std::string lang;
  return g_language_detector.DetectLanguage(std::string(tweet), tweet_len, lang); 
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetTestTweets(const char* user_name,
                  const unsigned int in_length,
                  char* tweets_buffer,
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

