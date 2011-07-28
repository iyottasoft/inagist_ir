#include "twitter_searcher.h"
#include <cstdlib>
#include <iostream>

int main(int argc, char* argv[]) {

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <0/1/2 - 0-search,1-tweets by user/2-mentions on user> <query/input_value>\n";
    return -1;
  }

  unsigned int input_type = atoi(argv[1]);
  std::string input_value = argv[2];

  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;
  std::string url;

  inagist_api::TwitterSearcher ts;

  switch (input_type) {
    case 0:
      if (ts.Search(input_value, tweets) < 0) {
        std::cout << "ERROR: No results found\n";
      }
      break;
    case 1:
      url = std::string("http://search.twitter.com/search.json?q=from:" + input_value/* + "&rpp=100"*/);
      if (ts.GetTweetsFromSearchUrl(url, tweets) < 0) {
        std::cout << "Error: test for twitter searcher failed\n";
      }
      break;
    case 2:
      url = std::string("http://search.twitter.com/search.json?q=to\%3A" + input_value);
      if (ts.GetTweetsFromSearchUrl(url, tweets) < 0) {
        std::cout << "Error: test for twitter searcher failed\n";
      }
      break;
    default:
      break;
  }

  for (set_iter=tweets.begin(); set_iter != tweets.end(); set_iter++)
    std::cout << *set_iter << std::endl;

  tweets.clear();

  return 0;
}
