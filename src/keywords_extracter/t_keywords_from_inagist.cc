#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include "keywords_extract.h"
#include "keywords_manager.h"
#include "inagist_api.h"

int main(int argc, char *argv[]) {

  if (argc > 2) {
    std::cout << "Usage: " << argv[0] << " <handle>\n";
    return -1;
  }

  // get top tweets from inagist api
  std::set<std::string> tweets;
  int num_docs = 0;
  if (argc == 2) {
    inagist_api::InagistAPI ia;
    if ((num_docs = ia.GetTrendingTweets(std::string(argv[1]), tweets)) < 0) {
      std::cout << "Error: could not get trending tweets from inagist\n";
      return -1;
    }
  } else {
    std::cout << "this feature has not been implemented yet\n";
  }

  inagist_trends::KeywordsExtract ke;
  if (ke.Init("./data/static_data/stopwords.txt", "./data/static_data/dictionary.txt", NULL, "./data/tweets.txt", "./data/static_data/output.txt") < 0) {
    std::cerr << "ERROR: couldn't initialize\n";
    return -1; 
  }
  inagist_trends::KeywordsManager km;

  char buffer[1024];
  std::string script;
  std::set<std::string> keywords_set;

  std::set<std::string>::iterator set_iter;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    strcpy(buffer, (char *) (*set_iter).c_str());
    ke.GetKeywords(buffer, script, keywords_set);
    std::cout << script << std::endl;
    ke.PrintKeywords(keywords_set);
    km.PopulateFreqMap(keywords_set);
    keywords_set.clear();
    memset(buffer, 0, 1024);
  }
  tweets.clear();
  std::cout << "Num tweets: " << num_docs << std::endl;

  return 0;
}
