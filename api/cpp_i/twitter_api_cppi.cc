#include "twitter_api_cppi.h"
#include <iostream>
#include <string>
#include <cstring>
#include <fstream>
#include <set>
#include "twitter_api.h"
#include "twitter_searcher.h"

#ifdef DEBUG
#if DEBUG>0
#define ERL_NIF_DEBUG DEBUG
#endif
#endif
//#define ERL_NIF_DEBUG 3

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
#ifdef ERL_NIF_DEBUG
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
#ifdef ERL_NIF_DEBUG
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
      std::cout << "Not enuf space in the tweets buffer\n";
      break;
    }
  }
  *ptr = '\0';
  *out_length = ptr - tweets_buffer;
  tweets.clear();
  ptr = NULL;

  return num_docs;
}
