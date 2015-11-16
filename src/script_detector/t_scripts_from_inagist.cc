#include <iostream>
#include <string>
#include <set>
#include "inagist_api.h"
#include "script_detector.h"

int main(int argc, char *argv[]) {

  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <handle>\n";
    return -1;
  }

  int num_docs = 0;
  std::set<std::string> tweets;
  if (argc == 2) {
    inagist_api::InagistAPI ia;
    if ((num_docs = ia.GetTrendingTweets(std::string(argv[1]), tweets)) < 0) {
      std::cout << "Error: could not get tweets form inagist\n";
    }
  } else {
    std::cout << "this case is not coded yet" << std::endl;
    return 0;
  }

  inagist_classifiers::ScriptDetector sd;
  std::set<std::string> scripts;
  std::set<std::string>::iterator tweets_iter;
  std::set<std::string>::iterator scripts_iter;
  std::string tweet;

  for (tweets_iter = tweets.begin(); tweets_iter != tweets.end(); tweets_iter++) {
    tweet = *tweets_iter;
    sd.Init();
    sd.DetectScript(tweet, scripts);
    std::cout << tweet << std::endl;
    for (scripts_iter = scripts.begin(); scripts_iter != scripts.end(); scripts_iter++)
      std::cout << *scripts_iter << std::endl;
    scripts.clear();
  }
  sd.Clear();
  std::cout << "Num Tweets: " << num_docs << std::endl;

  return 0;
}
