#include "ngrams_generator.h"
#include <iostream>
#include <set>
#include <map>
#include <cstdlib>
#include <cassert>
#include "twitter_api.h"
#include "twitter_searcher.h"
#include "string_utils.h"

inagist_classifiers::NgramsGenerator g_ng;

int GetNgrams(unsigned int test_type,
              std::string& text,
              std::set<std::string>& words_set,
              std::map<std::string, int>& features_map) {

  int ngrams_count = 0;
  std::map<std::string, int>::iterator map_iter;
  if (0 == test_type) {
    ngrams_count = g_ng.GetNgrams((unsigned char*) text.c_str(), text.length(), features_map);
  } else if (1 == test_type) {
    if (inagist_utils::Tokenize(text, words_set) < 0) {
      std::cout << "ERROR: could not tokenize text:\n" << text << std::endl;
    } else {
      ngrams_count = g_ng.GetNgramsFromWords(words_set, features_map);
    }
  } else if (2 == test_type) {
    ngrams_count = g_ng.GetAllNgrams(text, features_map);
  } else if (3 == test_type) {
    ngrams_count = g_ng.GetNgramsFromTweet(text, features_map);
  }
  if (ngrams_count > 0) {
    for (map_iter = features_map.begin(); map_iter != features_map.end(); map_iter++)
      std::cout << (*map_iter).first << " " << (*map_iter).second << std::endl;
    features_map.clear();
  } else if (ngrams_count < 0) {
    std::cout << "ERROR: could not find ngrams" << std::endl;
  } else {
    std::cout << "no ngrams found" << std::endl;
  }

  return ngrams_count;
}

int main(int argc, char* argv[]) {

  if (argc > 4 || argc < 3) {
    std::cout << "Usage: " << argv[0] << " \n\t<0/1/2/3, 0-interactive/1-file/2-tweet/3-many_tweets> \n\t<0/1/2, 0-normal/1-words/2-allgrams/3-tweet> \n\t[<input_file_name>/<handle>]\n";
    return -1;
  }

  std::map<std::string, int> features_map;
  std::string text;

  unsigned int input_type = atoi(argv[1]);
  assert((input_type >= 0 && input_type <= 3));
 
  unsigned int test_type = atoi(argv[2]);
  assert(test_type >= 0 && test_type <= 3);

  std::set<std::string> words_set;
  if (0 == input_type) {
    while (getline(std::cin, text)) {
      GetNgrams(test_type, text, words_set, features_map);
    }
    return 0;
  }

  if (1 == input_type) {
    if (argc != 4) {
      std::cout << "ERROR: input file needed\n";
      return -1;
    } else {
      std::cout << "This feature is not implemented yet\n";
    }
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
      std::cout << text << std::endl;
      if (GetNgrams(test_type, text, words_set, features_map) > 0)
        std::cout << text << std::endl;
      if (2 == input_type)
        break;
    }
    tweets.clear();
  }

  return 0;

}
