#include "profiler.h"
#include <iostream>
#include <string>
#include <set>

int main(int argc, char* argv[]) {

  if (argc != 6) {
    std::cerr << "Usage: " << argv[0] \
              << " <keytuples_config> <lang_detect_config> <classifier_config> <twitter_handle> <profile_name>\n";
    return -1;
  }

  std::string keytuples_config = std::string(argv[1]);
  std::string language_detection_config = std::string(argv[2]);
  std::string text_classifier_config = std::string(argv[3]);
  std::string twitter_handle = std::string(argv[4]);
  std::string profile_name = std::string(argv[5]);

  inagist_dashboard::Profiler p;

  if (p.Init(keytuples_config.c_str(),
             language_detection_config.c_str(),
             text_classifier_config.c_str()) < 0) {
    std::cout << "ERROR: could not initialize profiler\n";
    return -1;
  }

  std::set<std::string> locations;
  std::set<std::string> languages;
  std::set<std::string> text_classes;
  std::set<std::string> sub_classes;
  std::map<std::string, std::string> text_class_contributors_map;
  std::string sentiment;
  if (p.Profile(twitter_handle,
                locations, languages, text_classes, sub_classes,
                text_class_contributors_map, sentiment, profile_name) < 0) {
    std::cerr << "ERROR: could not generate profile for " \
              << twitter_handle << std::endl;
    return -1;
  }

  std::cout << "twitter_handle: " << twitter_handle << std::endl;

  std::cout << "languages:";
  std::set<std::string>::iterator language_iter;
  for (language_iter = languages.begin(); language_iter != languages.end(); language_iter++) {
    std::cout << " " << *language_iter; 
  }
  languages.clear();
  std::cout << std::endl;

  std::cout << "text_classes:";
  std::set<std::string>::iterator text_classes_iter;
  for (text_classes_iter = text_classes.begin(); text_classes_iter != text_classes.end(); text_classes_iter++) {
    std::cout << " " << *text_classes_iter; 
  }
  text_classes.clear();
  std::cout << std::endl;

  std::cout << "sub_classes:";
  std::set<std::string>::iterator sub_classes_iter;
  for (sub_classes_iter = sub_classes.begin(); sub_classes_iter != sub_classes.end(); sub_classes_iter++) {
    std::cout << " " << *sub_classes_iter; 
  }
  sub_classes.clear();
  std::cout << std::endl;
  std::cout << "text_class_contributors:";
  std::map<std::string, std::string>::iterator map_iter;
  for (map_iter = text_class_contributors_map.begin();
       map_iter != text_class_contributors_map.end();
       map_iter++) {
    std::cout << map_iter->first << " : " << map_iter->second << " | ";
  }
  std::cout << std::endl;

  return 0;
}

