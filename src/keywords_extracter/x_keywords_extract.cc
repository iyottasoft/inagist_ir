#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include "inagist_api.h"
#include "keywords_extract.h"
#include "keywords_manager.h"

int main(int argc, char *argv[]) {

  if (argc != 5) {
    std::cout << "usage: " << argv[0] << " <stopwords file> <dictionary file> <username> <keywords output file>\n";
    exit(0);
  }

  std::string stopwords_file = std::string(argv[1]);
  std::string dictionary_file = std::string(argv[2]);
  std::string user_name = std::string(argv[3]);
  std::string keywords_file = std::string(argv[4]);

  // get top and archieved tweets from inagist api
  std::set<std::string> tweets;
  int num_docs = 0;
  inagist_api::InagistAPI ia;
  if ((num_docs = ia.GetTrendingTweets(user_name, tweets)) < 0) {
    std::cout << "Error: could not get trending tweets from inagist\n";
    return -1;
  }
  // populate the same keyword set
  if ((num_docs = ia.GetArchievedTweets(user_name, tweets)) < 0) {
    std::cout << "Error: could not get archieved tweets from inagist\n";
    return -1;
  }

  inagist_trends::KeywordsExtract ke;
  if (ke.Init(argv[1], argv[2], NULL, "./data/tweets.txt", argv[4]) < 0) {
    std::cerr << "ERROR: couldn't initialize KeywordsExtract class\n";
    return -1; 
  }

  inagist_trends::KeywordsManager km;

  char buffer[1024];
  std::string script;
  std::set<std::string> keywords_set;

  std::set<std::string>::iterator tweets_iter;
  for (tweets_iter = tweets.begin(); tweets_iter != tweets.end(); tweets_iter++) {
    strcpy(buffer, (char *) (*tweets_iter).c_str());
    ke.GetKeywords(buffer, keywords_set);
    km.PopulateFreqMap(keywords_set);
    keywords_set.clear();
    memset(buffer, 0, 1024);
  }
  tweets.clear();

  km.CalculateIDF(num_docs, argv[4]);

  return 0;
}
