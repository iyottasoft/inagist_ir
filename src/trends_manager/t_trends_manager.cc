#include <iostream>
#include <cstring>
#include <set>
#include "trends_manager.h"
#include "twitter_api.h"
#include "twitter_searcher.h"

#define T_MAX_BUFFER_LEN 1024 

inagist_trends::TrendsManager g_tm;

int GetKeyTuples(const std::string& text) {
  std::string safe_status;
  std::string script;
  std::string lang;
  std::set<std::string> keywords;
  std::set<std::string> keyphrases;
  std::set<std::string> hashtags;
  if (g_tm.GetKeyTuples(text, safe_status, script, lang,
                     keywords, keyphrases, hashtags) < 0) {
    std::cout << "ERROR: could not get keywords\n";
  } else {
    std::cout << "safe status: " << safe_status \
              << "script: " << script \
              << "lang: " << lang << std::endl;
    std::set<std::string>::iterator set_iter;
    std::cout << "keywords:\n";
    for (set_iter = keywords.begin(); set_iter != keywords.end(); set_iter++) {
      std::cout << *set_iter << std::endl;
    }
    keywords.clear();
    std::cout << "keyphrases:\n";
    for (set_iter = keyphrases.begin(); set_iter != keyphrases.end(); set_iter++) {
      std::cout << *set_iter << std::endl;
    }
    keyphrases.clear();
    std::cout << "hashtags:\n";
    for (set_iter = hashtags.begin(); set_iter != hashtags.end(); set_iter++) {
      std::cout << *set_iter << std::endl;
    }
    hashtags.clear();
  }
  return 0;
}

int main(int argc, char *argv[]) {

  if (argc > 2) {
    std::cout << "Usage: " << argv[0] << " < -i for interactive | user_name >" << std::endl;
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
  std::string unsafe_dictionary_file = root_dir + "/data/static_data/unsafe_dictionary.txt";

  // these two are currently not used
  std::string lang_detect_config_file = root_dir + "/configs/language_detection.config";
  std::string channels_dictionary_file = root_dir + "/data/static_data/channels_dictionary.txt";

  if (g_tm.Init(stopwords_file.c_str(),
           dictionary_file.c_str(),
           unsafe_dictionary_file.c_str(),
           lang_detect_config_file.c_str(),
           channels_dictionary_file.c_str()) < 0) {
    std::cerr << "ERROR: could not initialize trends manager\n";
    return -1;
  }

  std::set<std::string> tweets;
  std::string line;
  if (argc == 2) { 
    if (strcmp(argv[1], "-i") == 0) {
      while (getline(std::cin, line)) {
        if (line.compare("quit") == 0)
          break;
        if (GetKeyTuples(line) < 0) {
          std::cerr << "ERROR: could not get keytuples\n";
        }
      }
    }
  } else if (argc == 3) {
    if (strcmp(argv[1], "-f") == 0) {
      std::string file_name = std::string(argv[2]);
      std::ifstream ifs(file_name.c_str());
      if (ifs.is_open()) {
        while (getline(ifs, line)) {
          if (line.length() > 1)
            tweets.insert(line);
        }
      } else {
        std::cout << "could not open file " << file_name << std::endl;
        return -1;
      }
      ifs.close();
    }
  }

  int num_docs = 0;
  if (argc == 1) { 
    inagist_api::TwitterAPI twitter_api;
    num_docs = twitter_api.GetPublicTimeLine(tweets);
  } else {
    inagist_api::TwitterSearcher twitter_searcher;
    num_docs = twitter_searcher.GetTweetsFromUser(std::string(argv[1]), tweets);
  }

  std::set<std::string>::iterator set_iter;
  std::string tweet;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    tweet = *set_iter;
    if (GetKeyTuples(line) < 0) {
      std::cerr << "ERROR: could not get keytuples\n";
    }
  }
  tweets.clear();

  return 0;
}
