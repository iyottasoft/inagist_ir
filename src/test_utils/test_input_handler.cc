#include "test_input_handler.h"
#include <iostream>
#include <fstream>
#include <set>
#include <cassert>
#include <cstdlib>
#include "twitter_api.h"
#include "inagist_api.h"

namespace inagist_test_utils {

TestInputHandler::TestInputHandler() {
}

TestInputHandler::~TestInputHandler() {
}

int TestInputHandler::ReadArgs(int argc, char* argv[], TestInput& test_input) {

  if (1 == argc) {
    return 0;
  }

  if (argc >= 2) {
    std::string classifier_config_file = std::string(argv[1]);
    if (classifier_config_file.size() < 5) {
      std::cout << "ERROR: invalid config file\n";
      return -1;
    }
    test_input.config.assign(classifier_config_file);
  }

  if (argc >= 3) {
    std::string keytuples_config_file = std::string(argv[2]);
    if (keytuples_config_file.size() < 5) {
      std::cout << "ERROR: invalid config file\n";
      return -1;
    }
    test_input.keytuples_config.assign(keytuples_config_file);
  }

  unsigned int input_type = 0;
  if (argc >= 4) {
    input_type = atoi(argv[3]);
    assert(input_type >=0 && input_type <=4);
    test_input.input_type = input_type;
  }

  if (argc >=5) {
    unsigned int output_type = atoi(argv[4]);
    assert(output_type >=0 && output_type <=2);
    test_input.output_type = output_type;
  } else {
    test_input.output_type = 0;
  }

  if (argc >= 6) {
    unsigned int debug_level = atoi(argv[5]);
    assert(debug_level >=0 && debug_level <=5);
    test_input.debug_level = debug_level;
  } else {
    test_input.debug_level = 0;
  }

  std::string handle;
  std::string input_file_name;

  if (7 == argc) {
    if (input_type >= 2) {
      handle = std::string(argv[6]);
      test_input.input_value.assign(handle);
    } else {
      input_file_name = std::string(argv[6]);
      test_input.input_value.assign(input_file_name);
    }
  }

  return 0;
}

int TestInputHandler::Test(const TestInput& test_input) {

  std::string text;
  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;
  std::ifstream ifs;
  switch (test_input.input_type) {
    case 0:
      while (getline(std::cin, text)) {
        if (text.compare("exit") == 0 || text.compare("quit") == 0)
          break;
        TestFunction(text);
      }
      break;
    case 1:
      ifs.open(test_input.input_value.c_str());
      if (!ifs.is_open()) {
        std::cout << "ERROR: could not open input file: " << test_input.input_value << std::endl;
      }
      while (getline(ifs, text)) {
        tweets.insert(text);
      }
      ifs.close();
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        text.assign(*set_iter);
        TestFunction(text);
        text.clear();
      }
      break;
    case 2:
      // fall through
    case 3:
      // get top tweets from twitter api
      if (!test_input.input_value.empty()) {
        std::cout << "this feature has not been implemented yet\n";
        return -1;
      } else {
        inagist_api::TwitterAPI tapi;
        if ((tapi.GetPublicTimeLine(tweets)) < 0) {
          std::cout << "Error: could not get trending tweets from inagist\n";
          return -1;
        }
      }
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        text.assign(*set_iter);
        TestFunction(text);
        text.clear();
        if (2 == test_input.input_type)
          break;
      }
      break;
    case 4:
      if (!test_input.input_value.empty()) {
        inagist_api::InagistAPI ia;
        if ((ia.GetTrendingTweets(test_input.input_value, tweets)) < 0) {
          std::cout << "ERROR: could not get trending tweets from inagist\n";
          return -1;
        }
      } else {
        std::cout << "this feature has not been implemented yet\n";
        return -1;
      }
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        text.assign(*set_iter);
        TestFunction(text);
        text.clear();
      }
      // get tweets from inagist api
      break;
    default:
      break;
  }

  tweets.clear();
  return 0;
}

int TestInputHandler::PrintArgs(const TestInput& test_input) {

  std::cout << "config_file: " << test_input.config << std::endl;
  std::cout << "keytuples_config_file: " << test_input.keytuples_config << std::endl;
  std::cout << "input_type: " << test_input.input_type << std::endl;
  std::cout << "output_type: " << test_input.output_type << std::endl;
  std::cout << "debug_level: " << test_input.debug_level << std::endl;
  if (1 == test_input.input_type)
    std::cout << "handle: " << test_input.input_value << std::endl;
  else
    std::cout << "input_file_name: " << test_input.input_value << std::endl;

  return 0;
}

} // namespace inagist_utils
