#include "twitter_searcher.h"
#include <iostream>
#include <cstdlib>

int main(int argc, char* argv[]) {

  if (argc < 2 || argc > 4) {
    std::cout << "Usage: " << argv[0] << " <handle> <0|1 = regular|retweet stats> [output_file_name]\n";
    return -1;
  }

  std::string handle = argv[1];
  int type = 0;
  if (argc >= 3) {
    type = atoi(argv[2]);
    std::cout << "type: " << type << std::endl;
  }
  std::string output_file_name;
  if (argc == 4)
    output_file_name = std::string(argv[3]);

  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;

  inagist_api::TwitterSearcher ts;

  if ((argc == 2) || (type == 0)) {
    std::string url = std::string("http://search.twitter.com/search.json?q=from:" + handle/* + "&rpp=100"*/);
    if (ts.GetTweetsFromSearchUrl(url, tweets) < 0) {
      std::cout << "ERROR: test for twitter searcher failed\n";
    } else {
      for (set_iter=tweets.begin(); set_iter != tweets.end(); set_iter++)
        std::cout << *set_iter << std::endl;
    }
    tweets.clear();

    url = std::string("http://search.twitter.com/search.json?q=to\%3A" + handle);
    if (ts.GetTweetsFromSearchUrl(url, tweets) < 0) {
      std::cout << "ERROR: test for twitter searcher failed\n";
    } else {
      for (set_iter=tweets.begin(); set_iter != tweets.end(); set_iter++)
        std::cout << *set_iter << std::endl;
    }
    tweets.clear();
  } else if ((argc == 4) && (type == 1)) {
    std::cout << "calling retweet stats" << std::endl;
    if (ts.GetRetweetStatsForUser(handle, output_file_name) < 0) {
      std::cout << "ERROR: could not get retweet stats\n";
    }
  }

  return 0;
}
