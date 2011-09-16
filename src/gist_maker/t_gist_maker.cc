#include <iostream>
#include <cstring>
#include <cstdlib>
#include <cassert>
#include <string>
#include <set>
#include "gist_maker.h"
#include "test_utils.h"

inagist::GistMaker g_gm;

int MakeGist(std::string text) {

  char buffer[1024];
  std::string safe_status;
  std::string script;
  std::set<std::string> named_entities_set;
  std::set<std::string> keywords_set;
  std::set<std::string> keyphrases_set;
  std::string language;
  std::set<std::string> text_classes_set;
  std::set<std::string> locations_set;
  //std::string intent;
  int intent_valence=0;
  //std::string sentiment;
  int sentiment_valence=0;

  strcpy(buffer, text.c_str()); 
  std::cout << std::endl << buffer << std::endl;

  int gist_elements_count = 0;
  if ((gist_elements_count = g_gm.MakeGist(buffer, safe_status, script
#ifdef NAMED_ENTITIES_ENABLED
                        , named_entities_set
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                        , keywords_set
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                        , keyphrases_set
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                        , language 
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                        , text_classes_set
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                        , locations_set
#endif // LOCATION_ENABLED
#ifdef INTENT_ENABLED
                        //, intent
                        , intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                        //, sentiment
                        , sentiment_valence
#endif // SENTIMENT_ENABLED
                       )) < 0) {
    std::cout << "ERROR: could not get gist\n";
    return -1;
  }

  memset(buffer, 0, 1024);

  if (!safe_status.empty()) {
    std::cout << "safe_status: " << safe_status << std::endl;
  }
  if (!script.empty()) {
    std::cout << "script: " << script << std::endl;
  }
#ifdef NAMED_ENTITIES_ENABLED
  if (named_entities_set.size() > 0) {
    std::cout << "named_entities:\n";
    g_gm.PrintKeywords(named_entities_set);
    named_entities_set.clear();
  }
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
  if (keywords_set.size() > 0) {
    std::cout << "keywords:\n";
    g_gm.PrintKeywords(keywords_set);
    keywords_set.clear();
  }
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
  if (keyphrases_set.size() > 0) {
    std::cout << "keyphrases:\n";
    g_gm.PrintKeywords(keyphrases_set);
    keyphrases_set.clear();
  }
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
  if (!language.empty()) {
    std::cout << "language: " << language << std::endl;
  }
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  if (text_classes_set.size() > 0) {
    std::cout << "text_classes:\n";
    g_gm.PrintKeywords(text_classes_set);
    text_classes_set.clear();
  }
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
  if (locations_set.size() > 0) {
    std::cout << "locations:\n";
    g_gm.PrintKeywords(locations_set);
    locations_set.clear();
  }
#endif // LOCATION_ENABLED
#ifdef INTENT_ENABLED
  //if (!intent.empty()) {
    //std::cout << "intent: " << intent << std::endl;
  //}
  if (intent_valence != 0) {
    std::cout << "intent: " << intent_valence << std::endl;
  }
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  //if (!sentiment.empty()) {
    //std::cout << "sentiment: " << sentiment << std::endl;
  //}
  if (sentiment_valence != 0) {
    std::cout << "sentiment: " << sentiment_valence << std::endl;
  }
#endif // SENTIMENT_ENABLED

  return 0;
}

int main(int argc, char *argv[]) {

  if (argc < 3 || argc > 4) {
    std::cout << "Usage: " << argv[0] << "\n\t<config_file_name>\n\t<0-5, 0-interactive, 1-file, 2-tweet, 3-many tweets, 4-inagist, 5-twitter search>\n\t[file_name/<handle/query>]\n";
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

  std::string gist_maker_config_file = std::string(argv[1]);
  if (gist_maker_config_file.size() < 5) {
    std::cout << "ERROR: invalid config file\n";
    return -1;
  }

  int input_type = atoi(argv[2]);
  assert(input_type >=0 && input_type <=5);
  const char* input_value = NULL;

  if (4 == argc) {
    input_value = argv[3];
  }

  std::string text;
  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;

  if (0 == input_type) {
    // initialize keytuples extracter
    if (g_gm.Init(gist_maker_config_file) < 0) {
      std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
      return -1; 
    } else {
      std::cout << "INFO: init done\n";
    }

    while (getline(std::cin, text)) {
      if (text.compare("exit") == 0 || text.compare("quit") == 0)
        break;
      MakeGist(text);
    }
  } else {
    if (inagist_utils::GetInputText(input_type, input_value, tweets) < 0) {
      std::cerr << "ERROR: could not input texts\n";
      return -1;
    }

    if (!tweets.empty()) {
      // initialize keytuples extracter
      if (g_gm.Init(gist_maker_config_file) < 0) {
        std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
        return -1; 
      }

      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        MakeGist(*set_iter);
        if (2 == input_type)
          break;
      }
      tweets.clear();
    }
  }

  return 0;
}
