#include "twitter_api.h"
#include <set>
#include <iostream>

int main(int argc, char* argv[]) {

  if (argc > 2) {
    std::cout << "Usage: " << argv[0] << " [user_name]\n";
    return -1;
  }

  inagist_api::TwitterAPI tapi;
  std::set<std::string> tweets;
  if (argc == 1) {
    if (tapi.GetPublicTimeLine(tweets) < 0) {
      std::cout << "Error: could not get public timeline\n" << std::endl;
      return -1;
    }
  } else if (argc == 2) {
    std::string user_name = std::string(argv[1]);
    if (tapi.GetUserTimeLine(user_name, tweets) < 0) {
      std::cout << "Error: could not get public timeline\n" << std::endl;
      return -1;
    }
  }
  std::set<std::string>::iterator set_iter;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    std::cout << *set_iter << std::endl;
  }
  tweets.clear();

  return 0;
}
