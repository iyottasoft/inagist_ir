#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include "keywords_extract.h"
#include "keywords_manager.h"
#include "inagist_api.h"

int main(int argc, char *argv[]) {

  if (argc > 2) {
    std::cout << "Usage: " << argv[0] << " <handle>\n";
    return -1;
  }

  std::string bin_location = std::string(argv[0]);
  std::string::size_type loc = bin_location.find("bin", 0);
  std::string root_dir;
  if (loc == std::string::npos) {
    std::cout << "ERROR: could not find bin location\n" << std::endl;
    return -1;
  } else {
    root_dir = std::string(bin_location, 0, loc);
  }

  std::string data_dir = root_dir + "data/";
  std::string stopwords_file = data_dir + "static_data/stopwords.txt";
  std::string dictionary_file = data_dir + "static_data/dictionary.txt";
  std::string unsafe_dictionary_file = data_dir + "static_data/unsafe_dictionary.txt";
  std::string lang_detect_config_file = root_dir + "configs/language_detection.config";
  std::string channels_dictionary_file = data_dir + "static_data/channels_dictionary.txt";
  std::string input_file = data_dir + "tweets.txt";
  std::string output_file = data_dir + "static_data/output.txt";

  inagist_trends::KeywordsExtract ke;
  if (ke.Init(stopwords_file.c_str(),
              dictionary_file.c_str(),
              unsafe_dictionary_file.c_str(),
              lang_detect_config_file.c_str(),
              channels_dictionary_file.c_str(),
              NULL,
              input_file.c_str(),
              output_file.c_str()) < 0) {
    std::cerr << "ERROR: couldn't initialize KeywordsExtract\n";
    return -1; 
  }

  // get top tweets from inagist api
  std::set<std::string> tweets;
  int num_docs = 0;
  if (argc == 2) {
    inagist_api::InagistAPI ia;
    if ((num_docs = ia.GetTrendingTweets(std::string(argv[1]), tweets)) < 0) {
      std::cout << "ERROR: could not get trending tweets from inagist\n";
      return -1;
    }
  } else {
    std::cout << "this feature has not been implemented yet\n";
    return -1;
  }

  char buffer[1024];
  std::string safe_status;
  std::string script;
  std::set<std::string> keywords_set;
  std::set<std::string> hashtags_set;
  std::set<std::string> keyphrases_set;

  std::set<std::string>::iterator set_iter;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    strcpy(buffer, (char *) (*set_iter).c_str());
    std::cout << buffer << std::endl;
    ke.GetKeywords(buffer, safe_status, script, keywords_set, hashtags_set, keyphrases_set);
    std::cout << safe_status << std::endl;
    std::cout << script << std::endl;
    ke.PrintKeywords(keywords_set);
    keywords_set.clear();
    ke.PrintKeywords(hashtags_set);
    hashtags_set.clear();
    ke.PrintKeywords(keyphrases_set);
    keyphrases_set.clear();
    memset(buffer, 0, 1024);
  }
  tweets.clear();
  std::cout << "Num tweets: " << num_docs << std::endl;

  return 0;
}
