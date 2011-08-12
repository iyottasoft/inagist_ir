#include "channel_manager.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cassert>
#include "twitter_api.h"

int main(int argc, char* argv[]) {

  if (argc < 3 || argc > 4) {
    std::cout << "Usage: " << argv[0] << " \n\t<0/1/2, 0-interactive|1-file/2-tweet/3-manytweets> \n\t<channels_dictionary_file> \n\t[<output_file_name>]\n";
    return -1;
  }

  int input_type = atoi(argv[1]);
  assert((input_type >= 0 && input_type <= 3));
 
  std::string channels_dictionary_file = std::string(argv[2]);

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
      std::cerr << "Error: could not get trending tweets from inagist\n";
      return -1;
    }
    std::set<std::string> channels_set;
    std::set<std::string>::iterator channels_iter;
    std::set<std::string>::iterator tweets_iter;
    std::string text;
    for (tweets_iter = tweets.begin(); tweets_iter != tweets.end(); tweets_iter++) {
      text = *tweets_iter;
      if (cm.FindChannels(text, channels_set) < 0) {
        std::cerr << "ERROR: could not find channels\n";
      } else {
        for (channels_iter = channels_set.begin();
             channels_iter != channels_set.end();
             channels_iter++) {
          std::cout << *channels_iter << std::endl;
        }
      }
    }
    channels_set.clear();
    tweets.clear();
  }

  cm.Clear();

  return 0;
}

