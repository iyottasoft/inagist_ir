#include "config_reader.h"
#include <iostream>
#include <fstream>

namespace inagist_classifiers {

ConfigReader::ConfigReader() {
}

ConfigReader::~ConfigReader() {
}

int ConfigReader::Clear(Config& config) {
  config.test_data_file.clear();
  config.freqs_file.clear();
  if (!config.classes.empty()) {
    config.classes.clear();
  }
  return 0;
}

int ConfigReader::Read(const char* config_file_name, Config& config) {

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
    int line_count = 0;
    std::string name;
    std::string handles_file_name;
    std::string tweets_file_name;
    std::string corpus_file_name;
    std::string training_data_file_name;
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
      if (key.compare(0, 8, "testdata") == 0) {
        config.test_data_file = value;
      } else if (key.compare(0, 11, "frequencies") == 0) {
        config.freqs_file = value;
      } else {
        line_count++;
        if (key.compare(0, 5, "class") == 0) {
          name = value;
        } else if (key.compare(0, 7, "handles") == 0) {
          handles_file_name = value;
        } else if (key.compare(0, 6, "corpus") == 0) {
          corpus_file_name = value;
        } else if (key.compare(0, 6, "tweets") == 0) {
          tweets_file_name = value;
        } else if (key.compare(0, 12, "trainingdata") == 0) {
          training_data_file_name = value;
        }
        if (line_count == 5) {
          ClassStruct class_struct;
          class_struct.name = name;
          class_struct.training_data_file = training_data_file_name;
          class_struct.handles_file = handles_file_name;
          class_struct.corpus_file = corpus_file_name;
          class_struct.tweets_file = tweets_file_name;
          config.classes.insert(class_struct);
          line_count = 0;
        }
      }
    }
    ifs.close();
  }

  return 0;
}

} // namespace inagist_classifiers
