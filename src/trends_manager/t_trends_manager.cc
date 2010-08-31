#include <iostream>
#include "trends_manager.h"

extern int Init();
extern int SubmitTweet(const char* str);
extern int GetTrends();

int main() {
  Init();
  SubmitTweet("tantricninja", "Contigous Caps will be counted. So will be contiguous nonstopwords");
  return 0;
}
