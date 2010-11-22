#include "string_utils.h"
#include <iostream>
#include <string>
#include <set>
#include "twitter_api.h"
#include "twitter_searcher.h"

int main(int argc, char* argv[]) {

  if (argc != 1 && argc != 2) {
    std::cout << "Usage: " << argv[0] << " \n";
    return -1;
  }

  inagist_utils::StringUtils util;

  std::string text;
  if (argc == 2 && (strcmp(argv[1], "-i") == 0)) {
    while(getline(std::cin, text)) {
      if (util.TestUtils(text, text.length()) < 0) {
        std::cout << "Error: TestUtils failed\n";
        return -1;
      }
    }
  }

  std::set<std::string> tweets;
  int num_docs = 0;
  if (argc == 1) { 
    inagist_api::TwitterAPI twitter_api;
    num_docs = twitter_api.GetPublicTimeLine(tweets);
  } else {
    inagist_api::TwitterSearcher twitter_searcher;
    num_docs = twitter_searcher.GetTweetsFromUser(std::string(argv[1]), tweets);
  }

  if (num_docs < 1) {
    std::cout << "no docs found\n";
    return -1;
  }

  std::set<std::string>::iterator set_iter;
  std::string tweet;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    tweet = *set_iter;
    std::cout << tweet << std::endl;
    if (util.TestUtils(tweet, tweet.length()) < 0) {
      std::cout << "Error: TestUtils failed\n";
      break;
    }
  }
  tweets.clear();

  return 0;
}

