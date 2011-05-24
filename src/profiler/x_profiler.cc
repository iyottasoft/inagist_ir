#include "profiler.h"
#include <iostream>
#include <string>

int main(int argc, char* argv[]) {

  if (argc != 6) {
    std::cerr << "Usage: " << argv[0] \
              << " <keytuples_config> <classifier_config> <lang_detect_config> <twitter_handle> <profile_name>\n";
    return -1;
  }

  std::string keytuples_config = std::string(argv[1]);
  std::string text_classifier_config = std::string(argv[2]);
  std::string language_detection_config = std::string(argv[3]);
  std::string twitter_handle = std::string(argv[4]);
  std::string profile_name = std::string(argv[5]);

  inagist_dashboard::Profiler p;

  if (p.Init(keytuples_config,
             text_classifier_config,
             language_detection_config) < 0) {
    std::cout << "ERROR: could not initialize profiler\n";
    return -1;
  }

  std::string dominant_class;
  if (p.Profile(twitter_handle, dominant_class, profile_name) < 0) {
    std::cerr << "ERROR: could not generate profile for " \
              << twitter_handle << std::endl;
    return -1;
  }

  std::cout << twitter_handle << " : " << dominant_class << std::endl;

  return 0;
}

