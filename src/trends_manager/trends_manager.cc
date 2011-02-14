/* trends_manager.cc */

#include "trends_manager.h"
#include "twitter_api.h"
#include "twitter_searcher.h"

#ifdef DEBUG
#if DEBUG>0
#define TRENDS_DEBUG DEBUG
#endif
#endif
//#define TRENDS_DEBUG 1

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
         const char* unsafe_dictionary_file_path,
         const char* lang_detect_config_file_path) {

  if (!stopwords_file_path ||
      !dictionary_file_path ||
      !unsafe_dictionary_file_path ||
      !lang_detect_config_file_path) {
#ifdef TRENDS_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif
    return -1;
  }

  if (g_keywords_extract.Init(stopwords_file_path,
                              dictionary_file_path,
                              unsafe_dictionary_file_path,
                              NULL,
                              lang_detect_config_file_path) < 0) {
#ifdef TRENDS_DEBUG
    std::cerr << "ERROR: could not initialize KeywordsExtract\n";
#endif
    return -1;
  }

  return 0;
}

// keywords and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int SubmitTweet(const unsigned char* tweet, const unsigned int tweet_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr,
                char* buffer1, const unsigned int buffer1_len,
                char* buffer2, const unsigned int buffer2_len,
                char* buffer3, const unsigned int buffer3_len,
                char* buffer4, const unsigned int buffer4_len) {

#ifdef TRENDS_DEBUG
  std::cout << tweet << std::endl;
#endif

  // this can be global. keeping it local for the time being
  unsigned char buffer[MAX_BUFFER_SIZE];
  if (tweet_len > 0 && tweet_len < MAX_BUFFER_SIZE) {
    memcpy((char *) buffer, (char *) tweet, tweet_len);
    buffer[tweet_len] = '\0';
  } else {
#ifdef TRENDS_DEBUG
    strcpy(safe_status_buffer, "errST");
    strcpy(script_buffer,"rr");
    strcpy((char *) keywords_buffer, "error_submit_tweet_invalid_len");
    *keywords_len_ptr = strlen((char *) keywords_buffer);
    *keywords_count_ptr = 1;
    strcpy((char *) hashtags_buffer, "error_submit_tweet_invalid_len");
    *hashtags_len_ptr = strlen((char *) hashtags_buffer);
    *hashtags_count_ptr = 1;
    strcpy((char *) keyphrases_buffer, "error_submit_tweet_invalid_len");
    *keyphrases_len_ptr = strlen((char *) keyphrases_buffer);
    *keyphrases_count_ptr = 1;
#endif
    memset(buffer, '\0', MAX_BUFFER_SIZE);
    return -1;
  }

  int ret_value = 0;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
  if ((ret_value = g_keywords_extract.GetKeywords(buffer, tweet_len,
                                                  safe_status_buffer, safe_status_buffer_len,
                                                  script_buffer, script_buffer_len,
                                                  keywords_buffer, keywords_buffer_len,
                                                  keywords_len, keywords_count,
                                                  hashtags_buffer, hashtags_buffer_len,
                                                  hashtags_len, hashtags_count,
                                                  keyphrases_buffer, keyphrases_buffer_len,
                                                  keyphrases_len, keyphrases_count,
                                                  buffer1, buffer1_len,
                                                  buffer2, buffer2_len,
                                                  buffer3, buffer3_len,
                                                  buffer4, buffer4_len)) <= 0) {
    if (ret_value < 0 ) {
#ifdef TRENDS_DEBUG
      std::cout << "Error: could not get keywords from KeywordsExtract\n";
      strcpy(safe_status_buffer, "errGK");
      strcpy(script_buffer, "rr");
      return -1;
#else
      *safe_status_buffer = '\0';
      *script_buffer = '\0';
#endif
    }
    *keywords_buffer = '\0';
    *keywords_len_ptr = 0;
    *keywords_count_ptr = 0;
    *hashtags_buffer = '\0';
    *hashtags_len_ptr = 0;
    *hashtags_count_ptr = 0;
    *keyphrases_buffer = '\0';
    *keyphrases_len_ptr = 0;
    *keyphrases_count_ptr = 0;
    *buffer1 = '\0';
    *buffer2 = '\0';
    *buffer3 = '\0';
    *buffer4 = '\0';
  }
  buffer[0] = '\0';
  *keywords_len_ptr = keywords_len;
  *keywords_count_ptr = keywords_count;
  *hashtags_len_ptr = hashtags_len;
  *hashtags_count_ptr = hashtags_count;
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
#ifdef TRENDS_DEBUG
    std::cout << *iter << std::endl;
#endif
    len = (*iter).length();
    total_len += (len + 1); // 1 for the pipe
    if (total_len < in_length) {
      strcpy(ptr, (*iter).c_str());
      ptr += len;
      strcpy(ptr, "|");
      ptr++;
      num_docs++;
    } else {
#ifdef TRENDS_DEBUG
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
#ifdef LD_DEBUG
    if (line.length() <= 0) {
      std::cerr << "Empty file\n":
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
#ifdef LD_DEBUG
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
int GetTrends(const char* user_name, char* trends_buffer) {
  if (!user_name || !trends_buffer)
    return -1;
  return 0;
}

