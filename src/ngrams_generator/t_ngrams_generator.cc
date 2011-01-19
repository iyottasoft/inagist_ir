#include "ngrams_generator.h"
#include <iostream>
#include <set>
#include <cstdlib>
#include "twitter_api.h"
#include "twitter_searcher.h"

int main(int argc, char* argv[]) {

  if (argc > 3 || argc < 2) {
    std::cout << "Usage: " << argv[0] << " <0/1/2, 0-interactive/1-file/2-tweets> [<file_name>/[handle]]\n";
    return -1;
  }

  inagist_classifiers::NgramsGenerator ng;
  std::map<std::string, int> features_map;
  std::map<std::string, int>::iterator map_iter;
  std::string text;

  int test_type = 0;
  if (argc >= 2) {
    test_type = atoi(argv[1]);
  }

  if (0 == test_type) {
    while (getline(std::cin, text)) {
      if (ng.GetNgrams((unsigned char*) text.c_str(), text.length(), features_map) <= 0) {
        std::cout << "ERROR: could not find ngrams" << std::endl;
      } else {
        for (map_iter = features_map.begin(); map_iter != features_map.end(); map_iter++)
          std::cout << (*map_iter).first << " " << (*map_iter).second << std::endl;
      }
      features_map.clear();
    }
    return 0;
  }

  if (1 == test_type) {
    if (argc != 3) {
      std::cout << "ERROR: input file needed\n";
      return -1;
    } else {
      std::cout << "This feature is not implemented yet\n";
    }
  }

  if (2 == test_type) {
    std::set<std::string> tweets;
    if (argc == 3) {
      std::string str = std::string(argv[2]);
      if (str.compare("-i") == 0) {
        while (getline(std::cin, text)) {
          if (ng.GetNgramsFromTweet(text, features_map) <= 0) {
            std::cout << "ERROR: could not find ngrams" << std::endl;
          } else {
            for (map_iter = features_map.begin(); map_iter != features_map.end(); map_iter++)
              std::cout << (*map_iter).first << " " << (*map_iter).second << std::endl;
          }
          features_map.clear();
        }
        return 0;
      } else {
        std::cout << "This feature is not implemented yet\n";
      }
    } else {
      inagist_api::TwitterAPI tapi;
      if (tapi.GetPublicTimeLine(tweets) < 0) {
        std::cout << "Error: could not get trending tweets from inagist\n";
        return -1;
      }
      std::set<std::string>::iterator set_iter;
      std::string tweet;
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        tweet = *set_iter;
        std::cout << tweet << std::endl;
        if (ng.GetNgramsFromTweet(tweet, features_map) <= 0) {
          std::cout << "ERROR: could not find ngrams" << std::endl;
        } else {
          for (map_iter = features_map.begin(); map_iter != features_map.end(); map_iter++)
            std::cout << (*map_iter).first << " " << (*map_iter).second << std::endl;
        }
        features_map.clear();
      }
      tweets.clear();
    }
  }
  return 0;
}
