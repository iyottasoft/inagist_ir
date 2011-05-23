#include "profiler.h"
#include <iostream>
#include <string>

namespace inagist_dashboard {

int main(int argc, char* argv[]) {

  if (argc != 4) {
    std::cerr << "Usage: " << argv[0] << " <user_name> <classifier_config> <profile_name>\n";
    return -1;
  }

  std::string user_name = std::string(argv[1]);
  std::string profile_name = std::string(argv[2]);
  Profiler p;
  if (p.GenerateProfile(user_name, profile_name) < 0) {
    std::cerr << "ERROR: could not generate profile for " << user_name << std::endl;
    return -1;
  }

  std::string dominant_class;
  if (p.Classify(classifier_config, profile_name, dominant_class) < 0) {
    std::cerr << "ERROR: could not classify " << user_name << std::endl;
    return 0;
  }

  std::cout << user_name << " : " << dominant_class << std::endl;

  return 0;
}

} // namespace inagist_dashboard
