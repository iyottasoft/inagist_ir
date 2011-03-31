#include <iostream>
#include <cstring>
#include <cstdlib>
#include <cassert>
#include <string>
#include <set>
#include "keytuples_extracter.h"
#include "keywords_manager.h"
#include "twitter_api.h"
#include "twitter_searcher.h"
#include "inagist_api.h"

inagist_trends::KeyTuplesExtracter g_ke;

int GetKeyTuples(std::string text) {

  char buffer[1024];
  std::string safe_status;
  std::string script;
  std::set<std::string> keywords_set;
  std::set<std::string> hashtags_set;
  std::set<std::string> keyphrases_set;

  strcpy(buffer, text.c_str()); 
  if (g_ke.GetKeyTuples(buffer, safe_status, script, keywords_set, hashtags_set, keyphrases_set) < 0) {
    std::cout << "ERROR: could not get keytuples\n";
    return -1;
  }

  if (script.compare(0,2,"en") != 0)
    return 0;

  std::cout << std::endl << buffer << std::endl;
  memset(buffer, 0, 1024);
  std::cout << "safe_status: " << safe_status << std::endl;
  std::cout << "script: " << script << std::endl;
  if (keywords_set.size() > 0) {
    std::cout << "keywords:\n";
    g_ke.PrintKeywords(keywords_set);
    keywords_set.clear();
  }
  if (hashtags_set.size() > 0) {
    std::cout << "hashtags:\n";
    g_ke.PrintKeywords(hashtags_set);
    hashtags_set.clear();
  }
  if (keyphrases_set.size() > 0) {
    std::cout << "keyphrases:\n";
    g_ke.PrintKeywords(keyphrases_set);
    keyphrases_set.clear();
  }

  return 0;
}

int main(int argc, char *argv[]) {

  if (argc < 3 || argc > 5) {
    std::cout << "Usage: " << argv[0] << "\n\t<config_file_name>\n\t<0/1/2, 0-interactive, 1-file, 2-tweet, 3-many tweets, 4-inagist>\n\t<debug_level>\n\t[<file>/<handle>]\n";
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

  std::string keytuples_config_file = std::string(argv[1]);
  if (keytuples_config_file.size() < 5) {
    std::cout << "ERROR: invalid config file\n";
    return -1;
  }

  unsigned int input_type = atoi(argv[2]);
  assert(input_type >=0 && input_type <=4);

  // initialize keytuples extracter
  if (g_ke.Init(keytuples_config_file) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
    return -1; 
  }

  unsigned int debug_level = 0;
  if (argc >= 4)
    debug_level = atoi(argv[3]);

  std::string input_file_name;
  std::string handle;

  if (5 == argc) {
    if (1==input_type) {
      input_file_name = std::string(argv[4]);
    } else if (input_type >= 2) {
      handle = std::string(argv[4]);
    }
  }

  std::string text;
  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;
  switch (input_type) {
    case 0:
      while (getline(std::cin, text)) {
        if (text.compare("exit") == 0 || text.compare("quit") == 0)
          break;
        GetKeyTuples(text);
      }
      break;
    case 1:
      std::cout << "this feature has not been implemented yet\n";
      break;
    case 2:
      // fall through
    case 3:
      // get top tweets from twitter api
      if (argc == 4) {
        inagist_api::TwitterAPI tapi;
        if ((tapi.GetPublicTimeLine(tweets)) < 0) {
          std::cout << "Error: could not get trending tweets from inagist\n";
          return -1;
        }
      } else {
        std::cout << "this feature has not been implemented yet\n";
        return -1;
      }
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        GetKeyTuples(*set_iter);
        if (2 == input_type)
          break;
      }
      break;
    case 4:
      if (argc == 5) {
        inagist_api::InagistAPI ia;
        if ((ia.GetTrendingTweets(std::string(argv[4]), tweets)) < 0) {
          std::cout << "ERROR: could not get trending tweets from inagist\n";
          return -1;
        }
      } else {
        std::cout << "this feature has not been implemented yet\n";
        return -1;
      }
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        GetKeyTuples(*set_iter);
      }
      // get tweets from inagist api
      break;
    default:
      break;
  }

  tweets.clear();

  return 0;
}
