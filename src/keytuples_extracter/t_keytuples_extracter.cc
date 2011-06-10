#include <iostream>
#include <cstring>
#include <cstdlib>
#include <cassert>
#include <string>
#include <set>
#include "keytuples_extracter.h"
#include "test_utils.h"

inagist_trends::KeyTuplesExtracter g_ke;

int GetKeyTuples(std::string text) {

  char buffer[1024];
  std::string safe_status;
  std::string script;
  std::set<std::string> keywords_set;
  std::set<std::string> hashtags_set;
  std::set<std::string> keyphrases_set;
  std::set<std::string> lang_words_set;
  std::set<std::string> text_class_words_set;

  strcpy(buffer, text.c_str()); 
  if (g_ke.GetKeyTuples(buffer, safe_status, script
#ifndef KEYWORDS_DISABLED
                        , keywords_set
#endif
#ifdef HASHTAGS_ENABLED
                        , hashtags_set
#endif
#ifdef KEYPHRASE_ENABLED
                        , keyphrases_set
#endif
#ifdef LANG_WORDS_ENABLED
                        , lang_words_set
#endif
#ifdef TEXT_CLASS_WORDS_ENABLED
                        , text_class_words_set
#endif
                       ) < 0) {
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
  if (lang_words_set.size() > 0) {
    std::cout << "lang_words:\n";
    g_ke.PrintKeywords(lang_words_set);
    lang_words_set.clear();
  }
  if (text_class_words_set.size() > 0) {
    std::cout << "text_class_words:\n";
    g_ke.PrintKeywords(text_class_words_set);
    text_class_words_set.clear();
  }

  return 0;
}

int main(int argc, char *argv[]) {

  if (argc < 3 || argc > 4) {
    std::cout << "Usage: " << argv[0] << "\n\t<config_file_name>\n\t<0/1/2, 0-interactive, 1-file, 2-tweet, 3-many tweets, 4-inagist>\n\t[<file>/<handle>]\n";
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
  const char* input_value = NULL;

  // initialize keytuples extracter
  if (g_ke.Init(keytuples_config_file) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
    return -1; 
  }

  if (5 == argc) {
    input_value = argv[4];
  }

  std::string text;
  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;

  if (0 == input_type) {
    while (getline(std::cin, text)) {
      if (text.compare("exit") == 0 || text.compare("quit") == 0)
        break;
      GetKeyTuples(text);
    }
  } else {
    if (inagist_utils::GetInputText(input_type, input_value, tweets) < 0) {
      std::cerr << "ERROR: could not input texts\n";
      return -1;
    }
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      GetKeyTuples(*set_iter);
      if (2 == input_type)
        break;
    }
  }

  tweets.clear();

  return 0;
}
