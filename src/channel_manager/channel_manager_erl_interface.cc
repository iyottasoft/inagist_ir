/* channel_manager_erl_interface.cc */

#ifdef _CPLUSPLUS
#include <set>
#include <string>
#include <cstring>
#include <cstdlib>
#include <iostream>
#include <fstream>
#endif

#include "channel_manager.h"
#include "twitter_api.h"
#include "twitter_searcher.h"

inagist_classifiers::ChannelManager g_channel_manager;

#ifdef _CPLUSPLUS
extern "C"
#endif
int Init(const char* config_file) {

  if (!config_file)
    return -1;

  if (g_channel_manager.Init(std::string(config_file)) < 0)
    return -1;

  return 0;
}

// keywords and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int SubmitTweet(const unsigned char *tweet, const unsigned int tweet_len,
                char *channels_buffer, const unsigned int channels_buffer_len,
                unsigned int *channels_count_ptr, unsigned int *channels_len_ptr) {

#ifdef CM_DEBUG
  std::cout << tweet << std::endl;
#endif

  int ret_value = 0;
  unsigned int channels_count = 0;
  unsigned int channels_len = 0;
  if ((ret_value = g_channel_manager.FindChannels(tweet, tweet_len,
                   channels_buffer, channels_buffer_len, channels_count, channels_len, false)) < 0) {
    strcpy(channels_buffer, "ERR");
    channels_count = 1;
    channels_len = 3;
  }
  *channels_count_ptr = channels_count;
  *channels_len_ptr = channels_len;

  return ret_value;
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
#ifdef CM_DEBUG
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
int GetTestTweetsFromFile(const char* file_name,
                          const unsigned int in_length,
                          char *tweets_buffer,
                          unsigned int *out_length) {
  int num_docs = 0;

  // get tweeets
  std::ifstream ifs(file_name);
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open input file " << file_name << std::endl;
    return -1;
  }

  std::string line;
  char *ptr = tweets_buffer;
  unsigned int len = 0;
  unsigned int total_len = 0;
  while (getline(ifs, line)) {
#ifdef CM_DEBUG
    if (line.length() <= 0) {
      std::cerr << "Empty file\n";
      continue;
    }
#endif

    len = line.length();
    total_len += (len + 1); // 1 for the pipe
    if (total_len < in_length) {
      strcpy(ptr, line.c_str());
      ptr += len;
      strcpy(ptr, "|");
      ptr++;
      num_docs++;
    } else {
#ifdef CM_DEBUG
      std::cout << "Not enuf space in the tweets buffer\n";
#endif
      break;
    }
  
    num_docs++;
  }
  *ptr = '\0';
  ifs.close();
  *out_length = ptr - tweets_buffer;
  ptr = NULL;

  return num_docs;
}

