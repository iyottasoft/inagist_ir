#include "twitter_searcher.h"
#include <iostream>

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <handle>\n";
    return -1;
  }

  std::string handle = argv[1];

  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;

  inagist_api::TwitterSearcher ts;
  std::string url = std::string("http://search.twitter.com/search.json?q=from:" + handle/* + "&rpp=100"*/);
  if (ts.GetTweetsFromSearchUrl(url, tweets) < 0) {
    std::cout << "Error: test for twitter searcher failed\n";
  } else {
    for (set_iter=tweets.begin(); set_iter != tweets.end(); set_iter++)
      std::cout << *set_iter << std::endl;
  }
  tweets.clear();

  url = std::string("http://search.twitter.com/search.json?q=to\%3A" + handle);
  if (ts.GetTweetsFromSearchUrl(url, tweets) < 0) {
    std::cout << "Error: test for twitter searcher failed\n";
  } else {
    for (set_iter=tweets.begin(); set_iter != tweets.end(); set_iter++)
      std::cout << *set_iter << std::endl;
  }
  tweets.clear();

  return 0;
}
