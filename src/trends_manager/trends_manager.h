/* trends_manager.h */

#ifndef _INAGIST_TRENDS_MANAGER_H_
#define _INAGIST_TRENDS_MANAGER_H_

#ifdef _CPLUSPLUS
#include <string>
#include "keywords_extract.h"
#include "keywords_manager.h"
#endif

//namespace inagist_trends {

//class TrendsManager {
// public:
//  TrendsManager();
//  ~TrendsManager();
#ifdef _CPLUSPLUS
  extern "C" {
#endif
    int Init(const char* stopwords_file_path,
             const char* dictionary_file_path,
             const char* unsafe_dictionary_file_path);
    int GetTrends(const char* user_name, char* trends_buffer);
    int SubmitTweet(const unsigned char* tweet, const unsigned int tweet_len,
                    char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                    char* script_buffer, const unsigned int script_buffer_len,
                    unsigned char* keywords, const unsigned int keywords_buffer_len,
                    unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                    unsigned char* keyphrases, const unsigned int keyphrases_buffer_len,
                    unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr);
    // c - cant pass by reference
    int GetTestTweets(const char* user_name, const unsigned int in_length, char* tweets_buffer, unsigned int* out_length);
#ifdef _CPLUSPLUS
  }
#endif
// private:
#ifdef _CPLUSPLUS
  extern inagist_trends::KeywordsExtract g_keywords_extract;
  extern inagist_trends::KeywordsManager g_keywords_manager;
  extern char g_buffer[1024];
#endif
//};

//} // namespace inagist_trends

#endif // _INAGIST_TRENDS_MANAGER_H_
