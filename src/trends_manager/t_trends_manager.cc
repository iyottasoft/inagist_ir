#include <iostream>
#include <cstring>
#include "trends_manager.h"

#define STOPWORDS_FILE "/home/balaji/inagist/ir_cpp/data/static_data/stopwords.txt"
#define DICTIONARY_FILE "/home/balaji/inagist/ir_cpp/data/static_data/dictionary.txt"

extern int Init(const char*, const char*);
extern int SubmitTweet(const char* str);
extern int GetTrends();

int main() {
  Init(STOPWORDS_FILE, DICTIONARY_FILE);

  char keywords[1024];
  memset(keywords, 0, 1024);
  SubmitTweet(/*"tantricninja",*/ "Contigous Caps will be counted. So will be contiguous nonstopwords", keywords);
  return 0;
}
