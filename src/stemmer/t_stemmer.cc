#include "stemmer.h"
#include <iostream>
#include <fstream>
#include <string>
#include <cstring>
#include <set>
#include "twitter_api.h"
#include "twitter_searcher.h"

int main(int argc, char* argv[]) {

  if (argc > 3) {
    std::cout << "Usage: " << argv[0] << " <nothing | -i | -f filename>\n";
    return -1;
  }

  std::string arguments(argv[0]);
  std::string::size_type loc = arguments.find("bin", 0);
  std::string root_dir;
  if (loc != std::string::npos) {
    loc-=1;
    root_dir.assign(argv[0], loc);
  }
  std::string stopwords_file = root_dir + "/data/static_data/stopwords.txt";
  std::string dictionary_file = root_dir + "/data/static_data/dictionary.txt";
  std::string stemmer_dictionary_file = root_dir + "/data/static_data/stemmer_dictionary.txt";

  inagist_search::Stemmer stemmer;
  if (stemmer.Init(stopwords_file.c_str(), dictionary_file.c_str(), stemmer_dictionary_file.c_str()) < 0) {
    std::cout << "ERROR: could not initialize dictionaries\n";
    return -1;
  }

  std::set<std::string> stems;
  std::set<std::string>::iterator stems_iter;

  std::string text;
  if (argc == 2 && (strcmp(argv[1], "-i") == 0)) {
    while(getline(std::cin, text)) {
      if (text.compare("exit") == 0)
        return 0;
      if ((text.length() > 1) && stemmer.Stem(text, stems) < 0) {
        std::cout << "Error: stemming failed\n";
        return -1;
      }
      for (stems_iter = stems.begin(); stems_iter != stems.end(); stems_iter++)
         std::cout << *stems_iter << std::endl;
      stems.clear();
    }
  }

  std::set<std::string> tweets;
  int num_docs = 0;
  if (argc == 3 && (strcmp(argv[1], "-f") == 0)) {
    std::string input_file_name = std::string(argv[2]);
    std::ifstream ifs(input_file_name.c_str());
    if (!ifs) {
      std::cout << "ERROR: could not open " << input_file_name << std::endl;
      return -1;
    }
    std::string line;
    while (getline(ifs,line)) {
      tweets.insert(line);
      num_docs++;
    }
    ifs.close();
  } else {
    if (argc == 1) { 
      inagist_api::TwitterAPI twitter_api;
      num_docs = twitter_api.GetPublicTimeLine(tweets);
    } else {
      inagist_api::TwitterSearcher twitter_searcher;
      num_docs = twitter_searcher.GetTweetsFromUser(std::string(argv[1]), tweets);
    }
  }

  if (num_docs < 1) {
    std::cout << "no docs found\n";
    return -1;
  }

  std::set<std::string>::iterator set_iter;
  std::string tweet;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    tweet = *set_iter;
    if (stemmer.Stem(tweet, stems) < 0) {
      std::cout << "Error: stemming failed\n";
      break;
    }
    if (!stems.empty())
      std::cout << tweet << std::endl;
    for (stems_iter = stems.begin(); stems_iter != stems.end(); stems_iter++)
       std::cout << *stems_iter << std::endl;
    stems.clear();
  }
  tweets.clear();

  return 0;
}

