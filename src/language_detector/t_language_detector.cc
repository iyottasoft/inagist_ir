#include "language_detector.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <set>
#include "twitter_api.h"
#include "twitter_searcher.h"

int main(int argc, char* argv[]) {

  if (argc != 2 && argc != 3) {
    std::cout << "Usage: " << argv[0] << " <config_file_name>\n";
    return -1;
  }

  std::string corpus_config_file_name = std::string(argv[1]);
  inagist_classifiers::LanguageDetector ld;
  if (ld.Init(corpus_config_file_name) < 0) {
    std::cout << "ERROR: could not initialize language detector\n";
    return -1;
  }

  std::set<std::string> lines;

  int num_docs = 0;
  if (argc == 2) { 
      inagist_api::TwitterAPI twitter_api;
      num_docs = twitter_api.GetPublicTimeLine(lines);
  } else {
      inagist_api::TwitterSearcher twitter_searcher;
      num_docs = twitter_searcher.GetTweetsFromUser(std::string(argv[2]), lines);
  }

  if (num_docs < 1)
    return -1;

  std::set<std::string>::iterator set_iter;
  std::string line;
  std::string lang;
  for (set_iter = lines.begin(); set_iter != lines.end(); set_iter++) {
    line = *set_iter;
    std::cout << line << std::endl;
    if (ld.DetectLanguage(line, line.length(), lang) < 0) {
      std::cout << "ERROR: could not find language\n";
    } else {
      std::cout << lang << std::endl;
    }
    break;
  }

  return 0;
}

