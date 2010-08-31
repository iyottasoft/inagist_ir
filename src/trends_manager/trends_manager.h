/* trends_manager.h */

#ifndef _INAGIST_TRENDS_MANAGER_H_
#define _INAGIST_TRENDS_MANAGER_H_

#ifdef _cplusplus
#include <string>
#include "keywords_extract.h"
#include "keywords_manager.h"
#endif

//namespace inagist_trends {

//class TrendsManager {
// public:
//  TrendsManager();
//  ~TrendsManager();
#ifdef _cplusplus
  extern "C" {
#endif
    int Init();
    int GetTrends(const char* user_name, char* trends_buffer);
    int SubmitTweet(const char* user_name, const char* tweet, char *keywords);
#ifdef _cplusplus
  }
#endif
// private:
#ifdef _cplusplus
  extern inagist_trends::KeywordsExtract g_keywords_extract;
  extern inagist_trends::KeywordsManager g_keywords_manager;
  extern char g_buffer[1024];
#endif
//};

//} // namespace inagist_trends

#endif // _INAGIST_TRENDS_MANAGER_H_
