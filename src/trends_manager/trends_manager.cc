/* trends_manager.cc */

#include "trends_manager.h"
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
extern "C"
#endif
int Init(const char* stopwords_file_path) {
  if (!stopwords_file_path)
    return -1;

  if (g_keywords_extract.Init(stopwords_file_path) < 0)
    return -1;

  memset(g_buffer, 0, MAX_BUFFER_SIZE);
  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int SubmitTweet(/*const char* user_name,*/ const char* tweet, char* keywords) {
  std::set<std::string> keywords_set;
  strcpy(g_buffer, tweet);
  g_keywords_extract.GetKeywords(g_buffer, keywords_set);
  std::set<std::string>::iterator iter;
  char *ptr = keywords;
  for (iter = keywords_set.begin(); iter != keywords_set.end(); iter++) {
    int len = (*iter).length();
    if ((ptr - keywords) + len < MAX_BUFFER_SIZE) {
      strcpy(ptr, (*iter).c_str());
      ptr += len;
      strcpy(ptr, ",");
      ptr++;
    } else {
#ifdef DEBUG
      std::cout << "Not enuf space in the buffer\n";
#endif
      return -1;
    }
  }
  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetTrends(const char* user_name, char* trends_buffer) {
  if (!user_name || !trends_buffer)
    return -1;
  return 0;
}

