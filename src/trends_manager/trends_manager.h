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
    int Init(const char* stopwords_file_path, const char* dictionary_file_path);
    int GetTrends(const char* user_name, char* trends_buffer);
    int SubmitTweet(/*const char* user_name,*/ const char* tweet, char *script, char *keywords, char *keyphrases);
    // c - cant pass by reference
    int GetTestTweets(const int in_length, char* tweets_buffer, int* out_length);
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
