#include "channel_manager.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cassert>
#include "twitter_api.h"

int main(int argc, char* argv[]) {

  if (argc < 3 || argc > 4) {
    std::cout << "Usage: " << argv[0] << " \n\t<0/1/2, 0-interactive|1-file/2-tweet/3-manytweets> \n\t<0/1/2, 0-create|1-test|2-clean> \n\t[<output_file_name>]\n";
    return -1;
  }

  unsigned int input_type = atoi(argv[1]);
  assert((input_type >= 0 && input_type <= 3));
 
  unsigned int test_type = atoi(argv[2]);
  assert(test_type >= 0 && test_type <= 2);

  std::string output_file_name;
  if (4 == argc) 
    output_file_name = std::string(argv[3]);

  inagist_classifiers::ChannelManager cm;

  std::string text;
  if (0 == input_type) {
    while (getline(std::cin, text)) {
      if (text.compare("exit") == 0 || text.compare("quit") == 0) {
        break;
      }
    }
    return 0;
  }

  if (1 == input_type) {
    std::cout << "This feature is not implemented yet\n";
  }

  if (2 == input_type || 3 == input_type) {
    std::set<std::string> tweets;
    inagist_api::TwitterAPI tapi;
    if (tapi.GetPublicTimeLine(tweets) < 0) {
      std::cout << "Error: could not get trending tweets from inagist\n";
      return -1;
    }
    std::set<std::string>::iterator set_iter;
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      text = *set_iter;
    }
    tweets.clear();
  }

  cm.Clear();

  return 0;
}

