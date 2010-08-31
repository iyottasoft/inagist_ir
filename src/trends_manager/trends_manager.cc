/* trends_manager.cc */

#include "trends_manager.h"
#ifdef _cplusplus
#include <set>
#include <cstring>
#include <cstdlib>
#endif

#define STOPWORDS_FILE "/home/balaji/inagist/ir_cpp/data/static_data/stopwords.txt"

//namespace inagist_trends {

char g_buffer[1024];
inagist_trends::KeywordsExtract g_keywords_extract;
inagist_trends::KeywordsManager g_keywords_manager;

#ifdef _cplusplus
extern "C"
#endif
int Init() {
  g_keywords_extract.Init(STOPWORDS_FILE);
  memset(g_buffer, 0, 1024);
  return 0;
}

#ifdef _cplusplus
extern "C"
#endif
int SubmitTweet(const char* user_name, const char* tweet) {
  std::set<std::string> keywords_set;
  strcpy(g_buffer, tweet);
  g_keywords_extract.GetKeywords(g_buffer, keywords_set);
  g_keywords_extract.PrintKeywords(keywords_set);
  return 0;
}

#ifdef _cplusplus
extern "C"
#endif
int GetTrends(const char* user_name, char* trends_buffer) {
  if (!user_name || !trends_buffer)
    return -1;
  //std::cout << "trends for " << std::string(user_name) << std::endl;
  //strcpy(trends_buffer, "take thee thy inagist trends");
  return 0;
}

//}
