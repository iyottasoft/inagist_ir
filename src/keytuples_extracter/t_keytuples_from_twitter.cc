#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include "keytuples_extracter.h"
#include "keywords_manager.h"
#include "twitter_api.h"
#include "twitter_searcher.h"

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

  // get top tweets from twitter api
  std::set<std::string> tweets;
  int num_docs = 0;
  if (argc == 1) {
    inagist_api::TwitterAPI tapi;
    if ((num_docs = tapi.GetPublicTimeLine(tweets)) < 0) {
      std::cout << "Error: could not get trending tweets from inagist\n";
      return -1;
    }
  } else {
    std::cout << "this feature has not been implemented yet\n";
    return -1;
  }

  std::string data_dir = root_dir + "data/";
  std::string stopwords_file = data_dir + "static_data/stopwords.txt";
  std::string dictionary_file = data_dir + "static_data/dictionary.txt";
  std::string unsafe_dictionary_file = data_dir + "static_data/unsafe_dictionary.txt";
  std::string lang_detect_config_file = root_dir + "configs/language_detection.config";
  std::string channels_dictionary_file = data_dir + "static_data/channels_dictionary.txt";
  std::string input_file = data_dir + "tweets.txt";
  std::string output_file = data_dir + "static_data/output.txt";

  inagist_trends::KeyTuplesExtracter ke;
  if (ke.Init(stopwords_file.c_str(),
              dictionary_file.c_str(),
              unsafe_dictionary_file.c_str(),
              lang_detect_config_file.c_str(),
              channels_dictionary_file.c_str(),
              NULL,
              input_file.c_str(),
              output_file.c_str()) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
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
    ke.GetKeyTuples(buffer, safe_status, script, keywords_set, hashtags_set, keyphrases_set);
    std::cout << "safe_status: " << safe_status << std::endl;
    std::cout << "script: " << script << std::endl;
    std::cout << "keywords: ";
    ke.PrintKeywords(keywords_set);
    std::cout << std::endl;
    std::cout << "hashtags: ";
    ke.PrintKeywords(hashtags_set);
    std::cout << std::endl;
    hashtags_set.clear();
    std::cout << "keyphrases: ";
    ke.PrintKeywords(keyphrases_set);
    std::cout << std::endl;
    keyphrases_set.clear();
    keywords_set.clear();
    memset(buffer, 0, 1024);
  }
  tweets.clear();
  std::cout << "Num tweets: " << num_docs << std::endl;

  return 0;
}
