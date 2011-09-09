#include "inagist_api.h"
#include <iostream>
#include <set>

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <handle>\n";
    return -1;
  }

  inagist_api::InagistAPI ia;
  std::set<std::string> tweets;
  int num_docs = 0;
  if ((num_docs = ia.GetTrendingTweets(argv[1], tweets)) < 0) {
    std::cout << "Error: could not get trending tweets from inagist\n";
    return -1;
  }

  std::set<std::string>::iterator set_iter;
  for (set_iter=tweets.begin(); set_iter != tweets.end(); set_iter++)
    std::cout << *set_iter << std::endl;
  tweets.clear();
  std::cout << "Num tweets: " << num_docs << std::endl;

  return 0;
}

