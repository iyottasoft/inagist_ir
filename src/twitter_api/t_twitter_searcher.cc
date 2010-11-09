#include "twitter_searcher.h"
#include <iostream>

int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cout << "Usage: " << argv[0] << " <handle>\n";
    return -1;
  }

  inagist_api::TwitterSearcher ts;
  std::string handle = argv[1];
  std::string url = std::string("http://search.twitter.com/search.json?q=from:" + handle/* + "&rpp=100"*/);
  if (ts.Test(url) < 0)
    std::cout << "Error: test for twitter searcher failed\n";

  url = std::string("http://search.twitter.com/search.json?q=to\%3A" + handle);
  if (ts.Test(url) < 0)
    std::cout << "Error: test for twitter searcher failed\n";

  return 0;
}
