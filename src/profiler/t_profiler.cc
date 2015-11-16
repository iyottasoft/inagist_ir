#include "profiler.h"
#include <iostream>
#include <string>
#include <cstring>
#include <cstdlib>
#include <set>

#define ULTIMATE_BUFFER_LEN 10240
#define MAX_BUFFER_LEN       1024
#define MAX_NAME_LEN          255
#define MAX_LIST_LEN          255
#define MAX_CLASS_NAME        255

int PrintSet(std::set<std::string>& string_set, std::string label) {

  if (string_set.empty())
    return 0;

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

  if (argc < 3 || argc > 4) {
    std::cerr << "Usage: " << argv[0] \
              << " <gist_maker_config> <input_type 0-handle, 1-doc> [debug_level]\n";
    return -1;
  }

  std::string gist_maker_config = std::string(argv[1]);
  unsigned int input_type = atoi(argv[2]);
  unsigned int debug_level = 0;
  if (argc == 4) {
    debug_level = atoi(argv[3]);
  }

  inagist_dashboard::Profiler p;
  inagist_dashboard::Profile *profile;

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
  std::string input_string;

  if (input_type == 0) { 
    std::cout << "Enter twitter handle:\n";
  } else {
    std::cout << "Enter text:\n";
    p.CreateProfile(profile);
  }

  char self_languages_buffer[MAX_LIST_LEN];
  self_languages_buffer[0] = '\0';
  unsigned int self_languages_len = 0;
  unsigned int self_languages_count = 0;

  char self_text_classes_buffer[MAX_BUFFER_LEN];
  self_text_classes_buffer[0] = '\0';
  unsigned int self_text_classes_len = 0;
  unsigned int self_text_classes_count = 0;

#ifdef LOCATION_ENABLED
  char self_location_classes_buffer[MAX_BUFFER_LEN];
  self_location_classes_buffer[0] = '\0';
  unsigned int self_location_classes_len = 0;
  unsigned int self_location_classes_count = 0;
#endif // LOCATION_ENABLED

  unsigned char self_text_class_contributors_buffer[ULTIMATE_BUFFER_LEN];
  self_text_class_contributors_buffer[0] = '\0';
  unsigned int self_text_class_contributors_len = 0;
  unsigned int self_text_class_contributors_count = 0;

  unsigned char text_buffer[MAX_BUFFER_LEN];
  text_buffer[0] = '\0';
  unsigned int text_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_len = 0;

  while (getline(std::cin, input_string)) {

    if (!input_string.compare("quit") || !input_string.compare("exit")) {
      break;
    }

    if (input_type == 1) {
      if (!input_string.compare("create")) {
        if (p.GetProfile(profile,
                   self_languages_buffer, MAX_LIST_LEN,
                   self_languages_len, self_languages_count,
                   self_text_classes_buffer, MAX_BUFFER_LEN,
                   self_text_classes_len, self_text_classes_count,
#ifdef LOCATION_ENABLED
                   self_location_classes_buffer, MAX_BUFFER_LEN,
                   self_location_classes_len, self_location_classes_count,
#endif // LOCATION_ENABLED
                   self_text_class_contributors_buffer, ULTIMATE_BUFFER_LEN,
                   self_text_class_contributors_len, self_text_class_contributors_count
                  ) < 0) {
          std::cerr << "ERROR: could not get profile\n";
        }
        if (self_languages_len > 0)
          std::cout << self_languages_buffer << std::endl;
        if (self_text_classes_len > 0)
          std::cout << self_text_classes_buffer << std::endl;
        if (self_text_class_contributors_len > 0)
          std::cout << self_text_class_contributors_buffer << std::endl;
      } else {
        strcpy((char*) text_buffer, input_string.c_str());
        text_len = input_string.length();
        p.AddTextToProfile(text_buffer, text_buffer_len, text_len, profile);
      }
      std::cout << "Enter text:\n";
    } else if (input_type == 0) {
      if (p.GetProfile(input_string,
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
                  << input_string << std::endl;
        return -1;
      }

      std::cout << "handle: " << input_string << std::endl;
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
      std::cout << "Enter twitter handle:\n";
    }
  }

  if (input_type == 1) {
    p.DeleteProfile(profile);
  }

  return 0;
}

