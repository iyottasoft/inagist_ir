#include <iostream>
#include <string>
#include <set>
#include "twitter_searcher.h"
#include "twitter_api.h"
#include "script_detector.h"

int main(int argc, char *argv[]) {

  if (argc > 2) {
    std::cout << "Usage: " << argv[0] << "<handle>\n";
    return -1;
  }

  std::set<std::string> tweets;
  if (argc == 1) {
    inagist_api::TwitterAPI tapi;
    if (tapi.GetPublicTimeLine(tweets) < 0) {
      std::cout << "Error: could not get twitter public timeline\n";
      return -1;
    }
  } else {
    inagist_api::TwitterSearcher ts;
    std::string url = std::string("http://search.twitter.com/search.json?q=from:") + std::string(argv[1]);
    if (ts.GetTweetsFromSearchUrl(url, tweets) < 0) {
      std::cout << "Error: could not get tweets from twitter search\n";
      return -1;
    }
  }

  std::set<std::string>::iterator tweets_iter;
  std::string tweet;
  inagist_classifiers::ScriptDetector sd;
  std::set<std::string> scripts;
  std::set<std::string>::iterator scripts_iter;
  int count = 0;
  std::string script;
  for (tweets_iter = tweets.begin(); tweets_iter != tweets.end(); tweets_iter++) {
    tweet = *tweets_iter;
    std::cout << tweet << std::endl;
    sd.Init();
    if (sd.DetectScript(tweet, scripts) < 0) {
      std::cout << "Error: could not find scripts\n";
    }
    scripts.clear();

    if ((count = sd.PrintScripts()) < 0)
      std::cout << "Error: could not print scripts map\n";

    if ((count = sd.GetMaxScript(script)) < 0)
      std::cout << "Error: could not get max script\n";
    else
      std::cout << "Script: " << script << std::endl;
    sd.Clear();
  }
  tweets.clear();

  return 0;
}
