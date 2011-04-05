#include "keytuples_config.h"
#include <iostream>
#include <fstream>

namespace inagist_trends {

KeyTuplesConfig::KeyTuplesConfig() {
}

KeyTuplesConfig::~KeyTuplesConfig() {
}

int KeyTuplesConfig::Clear(Config& config) {
  config.stopwords_file.clear();
  config.dictionary_file.clear();
  config.unsafe_dictionary_file.clear();
  config.lang_detect_config_file.clear();
  config.stemmer_dictionary_file.clear();
  return 0;
}

int KeyTuplesConfig::Read(const char* config_file_name, Config& config) {

  if (!config_file_name) {
    std::cerr << "ERROR: invalid input. can't read config\n";
    return -1;
  }

  std::ifstream ifs(config_file_name);
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open config file " << config_file_name << std::endl;
    return -1;
  } else {
    std::string line;
    std::string key;
    std::string value;
    std::string::size_type loc;
    while (getline(ifs, line)) {
      //std::cout << line << std::endl;
      if (line.length() < 1)
        continue;
      loc = line.find("=", 0);
      if (loc == std::string::npos) {
        std::cout << "ERROR: invalid config file entry in line: " << line << std::endl;
        break;
      }
      key.assign(line.c_str(), loc);
      value.assign(line.c_str(), loc+1, (line.length()-loc-1));
      //std::cout << key << std::endl;
      //std::cout << value << std::endl;
      if (key.compare(0, 9, "stopwords") == 0) {
        config.stopwords_file = value;
      } else if (key.compare(0, 10, "dictionary") == 0) {
        config.dictionary_file = value;
      } else if (key.compare(0, 17, "unsafe_dictionary") == 0) {
        config.unsafe_dictionary_file = value;
      } else if (key.compare(0, 18, "lang_detect_config") == 0) {
        config.lang_detect_config_file = value;
      } else if (key.compare(0, 18, "stemmer_dictionary") == 0) {
        config.stemmer_dictionary_file = value;
      }
    }
    ifs.close();
  }

  return 0;
}

} // namespace inagist_classifiers
