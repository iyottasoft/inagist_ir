#include <iostream>
#include <cstring>
#include "trends_manager.h"

extern int Init();
extern int SubmitTweet(const char* str);
extern int GetTrends();

int main() {
  Init();

  char keywords[1024];
  memset(keywords, 0, 1024);
  SubmitTweet("tantricninja", "Contigous Caps will be counted. So will be contiguous nonstopwords", keywords);
  return 0;
}
