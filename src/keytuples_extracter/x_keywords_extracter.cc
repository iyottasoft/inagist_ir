#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include "inagist_api.h"
#include "keytuples_extracter.h"
#include "keywords_manager.h"

int main(int argc, char *argv[]) {

  if (argc != 6) {
    std::cout << "Usage: " << argv[0] << " <stopwords file> <dictionary file> <unsafe dictionary file> <username> <keywords output file>\n";
    return -1;
  }

#ifdef KEYWORDS_DISABLED
    std::cerr << "KEYWORDS_DISABLED. nothing to be done.\n";
    return -1;
#endif // KEYWORDS_DISABLED

  std::string bin_location = std::string(argv[0]);
  std::string::size_type loc = bin_location.find("bin", 0);
  std::string root_dir;
  if (loc == std::string::npos) {
    std::cout << "ERROR: could not find bin location\n" << std::endl;
    return -1;
  } else {
    root_dir = std::string(bin_location, 0, loc);
  }
  std::string data_dir = root_dir + "data/";

  std::string stopwords_file = std::string(argv[1]);
  std::string dictionary_file = std::string(argv[2]);
  std::string unsafe_dictionary_file = std::string(argv[3]);
  std::string user_name = std::string(argv[4]);
  std::string keywords_file = std::string(argv[5]);

  inagist_trends::KeyTuplesExtracter ke;
  std::string input_file = data_dir + "tweets.txt";
  if (ke.Init(argv[1], argv[2], argv[3], NULL, input_file.c_str(), argv[5]) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter class\n";
    return -1; 
  }

  // get top and archieved tweets from inagist api
  std::set<std::string> tweets;
  int num_docs = 0;
  inagist_api::InagistAPI ia;
  if ((num_docs = ia.GetTrendingTweets(user_name, tweets)) < 0) {
    std::cout << "ERROR: could not get trending tweets from inagist\n";
    return -1;
  }
  // populate the same keyword set
  if ((num_docs = ia.GetArchievedTweets(user_name, tweets)) < 0) {
    std::cout << "ERROR: could not get archieved tweets from inagist\n";
    return -1;
  }

  inagist_trends::KeywordsManager km;

  char buffer[1024];
  std::string script;
  std::string safe_status;
  std::set<std::string> keywords_set;

  std::set<std::string>::iterator tweets_iter;
  for (tweets_iter = tweets.begin(); tweets_iter != tweets.end(); tweets_iter++) {
    strcpy(buffer, (char *) (*tweets_iter).c_str());
#ifndef KEYWORDS_DISABLED
    ke.GetKeywords(buffer, safe_status, script, keywords_set);
#endif // KEYWORDS_DISABLED
    km.PopulateFreqMap(keywords_set);
    keywords_set.clear();
    memset(buffer, 0, 1024);
  }
  tweets.clear();

  km.CalculateIDF(num_docs, argv[5]);

  return 0;
}
