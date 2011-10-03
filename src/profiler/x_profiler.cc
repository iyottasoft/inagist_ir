#include "profiler.h"
#include <iostream>
#include <string>
#include <cstdlib>
#include <set>

int main(int argc, char* argv[]) {

  if (argc < 5 || argc > 7) {
    std::cerr << "Usage: " << argv[0] \
              << " <keytuples_config> <lang_detect_config> <classifier_config> <twitter_handle> [debug_level] [profile_name]\n";
    return -1;
  }

  std::string keytuples_config = std::string(argv[1]);
  std::string language_detection_config = std::string(argv[2]);
  std::string text_classifier_config = std::string(argv[3]);
  std::string twitter_handle = std::string(argv[4]);
  unsigned int debug_level = 0;
  if (argc == 6) {
    debug_level = atoi(argv[5]);
  }
  std::string profile_name;
  if (argc == 7) {
    profile_name = std::string(argv[6]);
  }

  inagist_dashboard::Profiler p;

  if (p.Init(keytuples_config.c_str(),
             language_detection_config.c_str(),
             text_classifier_config.c_str()) < 0) {
    std::cerr << "ERROR: could not initialize profiler\n";
    return -1;
  }

  if (p.SetDebugLevel(debug_level) < 0) {
    std::cerr << "ERROR: could not set debug level for profiler\n";
  } else {
    std::cout << "INFO: setting debug level to " << debug_level << std::endl;
  }

  std::set<std::string> locations;
  std::set<std::string> self_languages;
  std::set<std::string> self_text_classes;
  std::set<std::string> self_sub_classes;
  std::map<std::string, std::string> self_text_class_contributors_map;
  std::set<std::string> others_languages;
  std::set<std::string> others_text_classes;
  std::set<std::string> others_sub_classes;
  std::map<std::string, std::string> others_text_class_contributors_map;
  int intent_valence=0;
  int sentiment_valence=0;
  std::set<std::string> recommendations;
  if (p.Profile(twitter_handle,
                locations,
                self_languages,
                self_text_classes,
                self_sub_classes,
                self_text_class_contributors_map,
                others_languages,
                others_text_classes,
                others_sub_classes,
                others_text_class_contributors_map,
                intent_valence,
                sentiment_valence,
                recommendations,
                profile_name) < 0) {
    std::cerr << "ERROR: could not generate profile for " \
              << twitter_handle << std::endl;
    return -1;
  }

  std::cout << "twitter_handle: " << twitter_handle << std::endl;

  std::cout << "locations:";
  std::set<std::string>::iterator location_iter;
  for (location_iter = locations.begin();
       location_iter != locations.end();
       location_iter++) {
    std::cout << " " << *location_iter; 
  }
  locations.clear();
  std::cout << std::endl;

  std::cout << "self_languages:";
  std::set<std::string>::iterator language_iter;
  for (language_iter = self_languages.begin();
       language_iter != self_languages.end();
       language_iter++) {
    std::cout << " " << *language_iter; 
  }
  self_languages.clear();
  std::cout << std::endl;

  std::cout << "self_text_classes:";
  std::set<std::string>::iterator text_classes_iter;
  for (text_classes_iter = self_text_classes.begin();
       text_classes_iter != self_text_classes.end();
       text_classes_iter++) {
    std::cout << " " << *text_classes_iter; 
  }
  self_text_classes.clear();
  std::cout << std::endl;

  std::cout << "self_sub_classes:";
  std::set<std::string>::iterator sub_classes_iter;
  for (sub_classes_iter = self_sub_classes.begin();
       sub_classes_iter != self_sub_classes.end();
       sub_classes_iter++) {
    std::cout << " " << *sub_classes_iter; 
  }
  self_sub_classes.clear();
  std::cout << std::endl;

  std::cout << "self_text_class_contributors:";
  std::map<std::string, std::string>::iterator map_iter;
  for (map_iter = self_text_class_contributors_map.begin();
       map_iter != self_text_class_contributors_map.end();
       map_iter++) {
    std::cout << map_iter->first << " : " << map_iter->second << " | ";
  }
  self_text_class_contributors_map.clear();
  std::cout << std::endl;

  std::cout << "others_languages:";
  for (language_iter = others_languages.begin();
       language_iter != others_languages.end();
       language_iter++) {
    std::cout << " " << *language_iter; 
  }
  others_languages.clear();
  std::cout << std::endl;

  std::cout << "others_text_classes:";
  for (text_classes_iter = others_text_classes.begin();
       text_classes_iter != others_text_classes.end();
       text_classes_iter++) {
    std::cout << " " << *text_classes_iter; 
  }
  others_text_classes.clear();
  std::cout << std::endl;

  std::cout << "others_sub_classes:";
  for (sub_classes_iter = others_sub_classes.begin();
       sub_classes_iter != others_sub_classes.end();
       sub_classes_iter++) {
    std::cout << " " << *sub_classes_iter; 
  }
  others_sub_classes.clear();
  std::cout << std::endl;

  std::cout << "others_text_class_contributors:";
  for (map_iter = others_text_class_contributors_map.begin();
       map_iter != others_text_class_contributors_map.end();
       map_iter++) {
    std::cout << map_iter->first << " : " << map_iter->second << " | ";
  }
  others_text_class_contributors_map.clear();
  std::cout << std::endl;

  return 0;
}

