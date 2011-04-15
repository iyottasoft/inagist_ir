#include "string_utils.h"
#include <iostream>
#include <string>
#include <cstring>
#include <set>
#include <cassert>
#include <cstdlib>
#include "twitter_api.h"
#include "twitter_searcher.h"

int main(int argc, char* argv[]) {

  if (argc != 3 && argc != 4) {
    std::cout << "Usage:\t" << argv[0] << " \n\t<input_type (0 - interactive, 1 - file_input, 2 - tweet, 3 - many_tweets) \n\t<test_type (0 - test_utils, 1 - tokenize, 2 - tolower)> \n\t<input/file_name/handle>\n";
    return -1;
  }

  int input_type = atoi(argv[1]);
  assert((input_type >= 0 && input_type <= 3));

  int test_type = atoi(argv[2]);
  assert((test_type >= 0 && test_type <= 2));

  //inagist_utils::StringUtils util;

  std::string text;
  std::set<std::string> tokens;
  std::set<std::string>::iterator token_iter;
  char buffer[1024];
  memset(buffer, '\0', 1024);
  if (0 == input_type) {
    while(getline(std::cin, text)) {
      if (text.compare("exit") == 0 || text.compare("quit") == 0) {
         break;
      }
      if (0 == test_type) {
        if (inagist_utils::TestUtils(text, text.length()) < 0) {
          std::cout << "Error: TestUtils failed\n";
          return -1;
        }
      } else if (1 == test_type) {
        if (inagist_utils::Tokenize(text, tokens) < 0) {
          std::cout << "ERROR: tokenize failed\n";
          break;
        }
        for (token_iter = tokens.begin(); token_iter != tokens.end(); token_iter++) {
          std::cout << *token_iter << "|" << std::endl;
        }
        tokens.clear();
      } else if (2 == test_type) {
        if (inagist_utils::ToLower(text.c_str(), buffer) < 0) {
          std::cout << "ERROR: ToLower failed\n" << std::endl;
        } else {
          std::cout << buffer << std::endl;
        }
      }
    }
  }

  std::set<std::string> tweets;
  int num_docs = 0;
  if (2 == input_type || 3 == input_type) {
    if (argc == 3) { 
      inagist_api::TwitterAPI twitter_api;
      num_docs = twitter_api.GetPublicTimeLine(tweets);
    } else if (argc == 4) {
      inagist_api::TwitterSearcher twitter_searcher;
      num_docs = twitter_searcher.GetTweetsFromUser(std::string(argv[3]), tweets);
    }
  }

  if (num_docs < 1) {
    std::cout << "no docs found\n";
    return -1;
  }

  std::set<std::string>::iterator tweet_iter;
  for (tweet_iter = tweets.begin(); tweet_iter != tweets.end(); tweet_iter++) {
    text = *tweet_iter;
    std::cout << text << std::endl;
    if (0 == test_type) {
      if (inagist_utils::TestUtils(text, text.length()) < 0) {
        std::cout << "Error: TestUtils failed\n";
        break;
      }
    } else if (1 == test_type) {
      if (inagist_utils::Tokenize(text, tokens) < 0) {
        std::cout << "ERROR: tokenize failed\n";
        break;
      }
      for (token_iter = tokens.begin(); token_iter != tokens.end(); token_iter++) {
        std::cout << *token_iter << "|" << std::endl;
      }
      tokens.clear();
    } else if (2 == test_type) {
      if (inagist_utils::ToLower(text.c_str(), buffer) < 0) {
        std::cout << "ERROR: ToLower failed\n" << std::endl;
      } else {
        std::cout << buffer << std::endl;
      }
    }
    if (2 == input_type)
      break;
  }
  tweets.clear();

  return 0;
}

