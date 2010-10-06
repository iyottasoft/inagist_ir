#include <iostream>
#include <cstring>
#include "trends_manager.h"

extern int Init(const char*, const char*);
extern int SubmitTweet(const char* str);
extern int GetTrends();

using std::string;

int main(int argc, char *argv[]) {
  if (argc > 2) {
    std::cout << "Usage: " << argv[0] << " <text within double quotes>" << std::endl;
    return -1;
  }

  string arguments(argv[0]);
  string::size_type loc = arguments.find("bin", 0);
  string root_dir;
  if (loc != string::npos) {
    loc-=1;
    root_dir.assign(argv[0], loc);
  }

  string stopwords_file = root_dir + "/data/static_data/stopwords.txt";
  string dictionary_file = root_dir + "/data/static_data/dictionary.txt";

  Init(stopwords_file.c_str(), dictionary_file.c_str());

  char script[4];
  memset(script, 0, 4);
  char keywords[1024];
  memset(keywords, 0, 1024);
  char keyphrases[1024];
  memset(keyphrases, 0, 1024);
  
  //SubmitTweet(/*"tantricninja",*/ "Contigous Caps will be counted. So will be contiguous nonstopwords", keywords, keyphrases);
  SubmitTweet(/*"tantricninja",*/ argv[1], script, keywords, keyphrases);
  std::cout << "keywords: " << std::endl << keywords << std::endl << "  keyphrases: " << std::endl << keyphrases << std::endl;

  return 0;
}
