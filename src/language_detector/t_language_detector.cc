#include "language_detector.h"
#include "twitter_api.h"
#include "twitter_searcher.h"
#include <iostream>

int main(int argc, char* argv[]) {
  if (argc != 2 && argc != 3) {
    std::cout << "Usage: " << argv[0] << " [training_data_dir]\n";
    return -1;
  }

  std::string training_data_dir = std::string(argv[1]);
  inagist_classifiers::LanguageDetector ld;
  if (ld.Init(training_data_dir) < 0) {
    std::cout << "Error: could not initialize language detector\n";
    return -1;
  }

  std::set<std::string> tweets;
  int num_docs = 0;
  if (argc == 2) { 
    inagist_api::TwitterAPI twitter_api;
    num_docs = twitter_api.GetPublicTimeLine(tweets);
  } else {
    inagist_api::TwitterSearcher twitter_searcher;
    num_docs = twitter_searcher.GetTweetsFromUser(std::string(argv[2]), tweets);
  }

  if (num_docs < 1)
    return -1;

  std::set<std::string>::iterator set_iter;
  std::string tweet;
  std::string lang;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    tweet = *set_iter;
    std::cout << tweet << std::endl;
    if (ld.DetectLanguage(tweet, tweet.length(), lang) < 0) {
      std::cout << "Error: could not find language\n";
    }
    break;
  }

  return 0;
}

