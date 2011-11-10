#include "profiler.h"
#include <iostream>
#include <string>
#include <cstdlib>
#include <set>

int PrintSet(std::set<std::string>& string_set, std::string label) {

  std::cout << label << ": ";
  std::set<std::string>::iterator set_iter;
  for (set_iter = string_set.begin();
       set_iter != string_set.end();
       set_iter++) {
    std::cout << *set_iter << " | ";
  }
  string_set.clear();
  std::cout << std::endl << std::endl;

  return 0;
}

int main(int argc, char* argv[]) {

  if (argc < 2 || argc > 3) {
    std::cerr << "Usage: " << argv[0] \
              << " <gist_maker_config> [debug_level]\n";
    return -1;
  }

  std::string gist_maker_config = std::string(argv[1]);
  unsigned int debug_level = 0;
  if (argc == 3) {
    debug_level = atoi(argv[2]);
  }

  inagist_dashboard::Profiler p;

  std::cout << "Initializing gist maker. please wait ..." << std::endl;
  if (p.Init(gist_maker_config.c_str()) < 0) {
    std::cerr << "ERROR: could not initialize profiler\n";
    return -1;
  }
  std::cout << "done!" << std::endl;

  if (argc == 3) {
    if (p.SetDebugLevel(debug_level) < 0) {
      std::cerr << "ERROR: could not set debug level for profiler\n";
    } else {
      if (debug_level > 1) {
        std::cout << "INFO: setting debug level to " << debug_level << std::endl;
      }
    }
  }

  std::set<std::string> locations;
  std::set<std::string> self_languages;
  std::set<std::string> self_text_classes;
#ifdef LOCATION_ENABLED
  std::set<std::string> self_location_classes;
#endif // LOCATION_ENABLED
  // std::map<std::string, std::string> self_text_class_contributors_map;
  std::set<std::string> self_text_class_contributors;
  std::set<std::string> others_languages;
  std::set<std::string> others_text_classes;
#ifdef LOCATION_ENABLED
  std::set<std::string> others_location_classes;
#endif // LOCATION_ENABLED
  //std::map<std::string, std::string> others_text_class_contributors_map;
  std::set<std::string> others_text_class_contributors;
  std::set<std::string> recommendations;
  std::string profile_name;
  std::string twitter_handle;

  std::cout << "Enter twitter handle:\n";
  while (getline(std::cin, twitter_handle)) {
    if (!twitter_handle.compare("quit") || !twitter_handle.compare("exit")) {
      break;
    }
    if (p.Profile(twitter_handle,
                locations
#ifdef LANG_ENABLED
                , self_languages
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                , self_text_classes
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                , self_location_classes
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                , self_text_class_contributors
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
                , others_languages
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                , others_text_classes
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                , others_location_classes
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                , others_text_class_contributors
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef RECSYS_ENABLED
                , recommendations
#endif // RECSYS_ENABLED
                , profile_name) < 0) {
      std::cerr << "ERROR: could not generate profile for " \
                << twitter_handle << std::endl;
      return -1;
    }

    std::cout << "twitter_handle: " << twitter_handle << std::endl;
    PrintSet(locations, "locations");
    PrintSet(self_languages, "self_languages");
    PrintSet(self_text_classes, "self_text_classes");
    PrintSet(self_text_class_contributors, "self_text_class_contributors");
#ifdef LOCATION_ENABLED
    PrintSet(self_location_classes, "self_location_classes");
#endif // LOCATION_ENABLED

    PrintSet(others_languages, "others_languages");
    PrintSet(others_text_classes, "others_text_classes");
    PrintSet(others_text_class_contributors, "others_text_class_contributors");
#ifdef LOCATION_ENABLED
    PrintSet(others_location_classes, "others_location_classes");
#endif // LOCATION_ENABLED
#ifdef RECSYS_ENABLED
    PrintSet(recommendations, "recommendations");
#endif // RECSYS_ENABLED
  }

  return 0;
}

