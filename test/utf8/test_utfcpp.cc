#ifdef _CPLUSPLUS
#include <cstdio>
#include <cstring>
#include <iostream>
#endif
#include "test_utfcpp.h"
#include "utf8.h"
#include "script_detector_utils.h"
#include "twitter_api.h"
#include "twitter_searcher.h"

#ifdef _CPLUSPLUS
extern "C" {
#endif
int test_detect_script(char* text, int text_len, char* script_buffer, int script_buffer_len) {
  strcpy(script_buffer, "yy");
  if (!text) {
    std::cout << "ERROR: invalid input\n";
    return -1;
  }
    
  char *ptr = text;
  char *end = strchr(text, '\0');
  int code_point = 0;
  std::string script = "uu";
  std::string script_temp = "uu";
  int script_count = 0;
  int english_count = 0;
  while (ptr && *ptr != '\0') {
    try {
      code_point = utf8::next(ptr, end);
      if (code_point > 0x7F) {
        if (inagist_classifiers::DetectScript(code_point, script_temp) > 0) {
          if (script_temp != "en") {
            if (script_temp != script) {
              script_count = 0;
              script = script_temp;
            }
            else {
              script_count++;
            }
          }
        }
      } else {
        if (code_point > 0x40 && code_point < 0x7B) {
          english_count++;
        }
      }
    } catch (...) {
      std::cout << "EXCEPTION: utf8 returned exception" << std::endl;
      memset(script_buffer, 0, script_buffer_len);
      strcpy(script_buffer, "exc");
      return 0;
    }
  }

  if (script_count == 0 && english_count > 10) {
    script = "en";
  } else if (script_count > 0 && (script_count < 11 || script_count < english_count)) {
    script = "uu";
  }
  strcpy(script_buffer, script.c_str());

  return 0;
}
#ifdef _CPLUSPLUS
}
#endif

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
//#ifdef DEBUG
      std::cout << "Not enuf space in the tweets buffer\n";
//#endif
      break;
    }
  }
  *ptr = '\0';
  *out_length = ptr - tweets_buffer;
  tweets.clear();
  ptr = NULL;

  return num_docs;
}
