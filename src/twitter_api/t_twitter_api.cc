#include "twitter_api.h"
#include <set>
#include <iostream>

int main(int argc, char* argv[]) {
  inagist_api::TwitterAPI tapi;
  std::set<std::string> tweets;
  if (tapi.GetPublicTimeLine(tweets) < 0) {
    std::cout << "Error: could not get public timeline\n" << std::endl;
  } else {
    std::set<std::string>::iterator set_iter;
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      std::cout << *set_iter << std::endl;
    }
    tweets.clear();
  }
  return 0;
}
