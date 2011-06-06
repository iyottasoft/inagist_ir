#include "test_utils.h"
#include <iostream>
#include <fstream>
#include <set>
#include <cassert>
#include <cstring>
#include <cstdlib>
#include "twitter_api.h"
#include "inagist_api.h"
#include "twitter_searcher.h"

namespace inagist_utils {

int GetInputText(const unsigned int &input_type,
                 const char* input_value,
                 std::set<std::string>& docs) {

  std::set<std::string>::iterator set_iter;
  std::ifstream ifs;
  std::string text;
  switch (input_type) {
    case 0:
      std::cout << "ERROR: interactive option is not handled in util";
      return -1;
    case 1:
      if (!input_value || strlen(input_value) < 4) {
        std::cerr << "ERROR: invalid input file name" << std::endl;
        return -1;
      }
      ifs.open(input_value);
      if (!ifs.is_open()) {
        std::cout << "ERROR: could not open input file: " << input_value << std::endl;
      }
      while (getline(ifs, text)) {
        docs.insert(text);
      }
      ifs.close();
      break;
    case 2:
      // fall through
    case 3:
      // get top tweets from twitter api
      if (input_value) {
        if (strlen(input_value) < 1) {
          std::cerr << "ERROR: invalid input\n";
          return -1;
        }
        inagist_api::TwitterSearcher ts;
        if (ts.GetTweetsFromUser(input_value, docs) < 0) {
          std::cout << "ERROR: could not search for tweets by " << input_value << std::endl;
          return -1;
        }
      } else {
        inagist_api::TwitterAPI tapi;
        if ((tapi.GetPublicTimeLine(docs)) < 0) {
          std::cout << "Error: could not get trending tweets from inagist\n";
          return -1;
        }
      }
      if ((2 == input_type) && (docs.size() > 1)) {
        set_iter = docs.begin();
        set_iter++;
        for (; set_iter != docs.end(); set_iter++)
          docs.erase(set_iter);
      }
      return docs.size();
      break;
    case 4:
      // get trends from inagist api
      if (!input_value || strlen(input_value) < 4) {
        std::cerr << "ERROR: invalid input or this feature has not been implemented" << std::endl;
        return -1;
      } else {
        inagist_api::InagistAPI ia;
        if ((ia.GetTrendingTweets(input_value, docs)) < 0) {
          std::cout << "ERROR: could not get trending tweets from inagist\n";
          return -1;
        }
      }
      return docs.size();
      break;
    default:
      break;
  }

  return 0;
}

} // namespace inagist_utils
