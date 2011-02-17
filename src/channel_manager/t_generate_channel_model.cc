#include "channel_manager.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include "twitter_api.h"
#include "keywords_extract.h"

#define MAX_TEST_BUFFER_LEN 1024
int main(int argc, char* argv[]) {

  // TODO (balaji) use getopt for heaven's sake!
  if (argc < 4 || argc > 6) {
    std::cout << "Usage: " << argv[0] << " \n\t<0/1/2, 0-interactive|1-file/2-tweet/3-manytweets> \n\t<0/1/2, 0-create|1-test|2-clean> \n\t<config_file_path> \n\t[<input_file_path>]\n\t[<output_file_path>]\n";
    return -1;
  }

  // config file has the input files for keywords extract
  unsigned int input_type = atoi(argv[1]);
  assert((input_type >= 0 && input_type <= 3));
 
  unsigned int test_type = atoi(argv[2]);
  assert(test_type >= 0 && test_type <= 2);

  std::string config_file_path = std::string(argv[3]);
  std::ifstream config_fs(config_file_path.c_str());
  if (!config_fs.is_open()) {
    std::cout << "ERROR: cannot open config file " << config_file_path << std::endl;
  }
  std::string handles_file_path;
  std::string output_tweets_file_path;
  std::string output_corpus_file_path;
  std::string stopwords_file_path;
  std::string dictionary_file_path;
  std::string unsafe_dictionary_file_path;
  std::string lang_detect_config_file_path; 
  std::string line;
  std::string key;
  std::string value;
  std::string::size_type loc;
  int line_count = 0;
  std::string channel;
  while (getline(config_fs, line)) {
    line_count++;
    //std::cout << line << std::endl;
    loc = line.find("=", 0);
    if (loc == std::string::npos) {
      std::cout << "ERROR: invalid config file entry\n";
      break;
    }
    key.assign(line.c_str(), loc);
    value.assign(line.c_str(), loc+1, (line.length()-loc-1));
    if (key.compare(0, 4, "channel") == 0) {
      channel = value;
    } else if (key.compare(0, 7, "handles") == 0) {
      handles_file_path = value;
    } else if (key.compare(0, 6, "corpus") == 0) {
      output_corpus_file_path = value;
    } else if (key.compare(0, 6, "tweets") == 0) {
      output_tweets_file_path = value;
    } else if (key.compare(0, 9, "stopwords") == 0) {
      stopwords_file_path = value;
    } else if (key.compare(0, 10, "dictionary") == 0) {
      dictionary_file_path = value;
    } else if (key.compare(0, 11, "unsafewords") == 0) {
      unsafe_dictionary_file_path = value;
    } else if (key.compare(0, 11, "langdconfig") == 0) {
      lang_detect_config_file_path = value;
    }
  }
  config_fs.close();

  std::string output_file_path;
  std::string input_file_path;
  if (5 == argc) {
    if (1 == input_type) {
      input_file_path = std::string(argv[4]);
    } else {
      output_file_path = std::string(argv[4]);
    }
  } else if (6 == argc) {
      input_file_path = std::string(argv[4]);
      output_file_path = std::string(argv[5]);
  }

  inagist_classifiers::ChannelManager cm;
  inagist_trends::KeywordsExtract ke;
  if (ke.Init(stopwords_file_path.c_str(),
              dictionary_file_path.c_str(),
              unsafe_dictionary_file_path.c_str(),
              NULL,
              lang_detect_config_file_path.c_str()) < 0) {
    std::cout << "ERROR: could not initialize keywords extract\n";
  }

  std::string text;
  if (0 == input_type) {
    while (getline(std::cin, text)) {
      if (text.compare("exit") == 0 || text.compare("quit") == 0) {
        break;
      }
    }
    return 0;
  }

  if (1 == input_type) {
    if (argc < 5) {
      std::cout << "ERROR: input file needed\n";
      return -1;
    }
    std::ifstream ifs(input_file_path.c_str());
    if (!ifs.is_open()) {
      std::cout << "ERROR: could not open input file ";
      std::cout << input_file_path << std::endl;
    } else {
      std::string line;
      unsigned int num_docs = 0;
      char str[MAX_TEST_BUFFER_LEN];
      memset(str, '\0', MAX_TEST_BUFFER_LEN);
      std::string safe_status;
      std::string lang;
      std::set<std::string> keywords_set;
      std::set<std::string> hashtags_set;
      std::set<std::string> keyphrases_set;
      while (getline(ifs, line)) {
        num_docs++;
        strcpy(str, line.c_str());
        std::cout << line << std::endl;
        if (ke.GetKeywords(str, safe_status, lang,
                           keywords_set, hashtags_set, keyphrases_set) < 0) {
          std::cout << "ERROR: could not get keywords for ...\n";
          std::cout << line << std::endl;
        }
        std::set<std::string>::iterator set_iter;
        std::cout << "keywords:\n";
        for (set_iter = keywords_set.begin(); set_iter != keywords_set.end(); set_iter++) {
          std::cout << *set_iter << std::endl;
        }
        std::cout << "keyphrases:\n";
        for (set_iter = keyphrases_set.begin(); set_iter != keyphrases_set.end(); set_iter++) {
          std::cout << *set_iter << std::endl;
        }
        std::cout << "hashtags:\n";
        for (set_iter = hashtags_set.begin(); set_iter != hashtags_set.end(); set_iter++) {
          std::cout << *set_iter << std::endl;
        }
      }
    }
  }

  if (2 == input_type || 3 == input_type) {
    std::set<std::string> tweets;
    inagist_api::TwitterAPI tapi;
    if (tapi.GetPublicTimeLine(tweets) < 0) {
      std::cout << "Error: could not get trending tweets from inagist\n";
      return -1;
    }
    std::set<std::string>::iterator set_iter;
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      text = *set_iter;
    }
    tweets.clear();
  }

  cm.Clear();

  return 0;
}

