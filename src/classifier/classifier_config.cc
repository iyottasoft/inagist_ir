#include "classifier_config.h"
#include <iostream>
#include <fstream>

namespace inagist_classifiers {

ClassifierConfig::ClassifierConfig() {
}

ClassifierConfig::~ClassifierConfig() {
}

int ClassifierConfig::Clear(Config& config) {
  config.class_freqs_file.clear();
  config.test_freqs_file.clear();
  if (!config.classes.empty()) {
    config.classes.clear();
  }
  return 0;
}

int ClassifierConfig::Read(const char* config_file, Config& config) {

  if (!config_file) {
    std::cerr << "ERROR: invalid input. can't read config\n";
    return -1;
  }

  std::ifstream ifs(config_file);
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open config file " << config_file << std::endl;
    return -1;
  } else {
    std::string line;
    std::string key;
    std::string value;
    std::string::size_type loc;
    int line_count = 0;
    ClassStruct class_struct;
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
      if (key.compare(0, 17, "class_frequencies") == 0) {
        config.class_freqs_file = value;
      } else if (key.compare(0, 16, "test_frequencies") == 0) {
        config.test_freqs_file = value;
      } else {
        line_count++;
        if (key.compare(0, 10, "class_name") == 0) {
          class_struct.name = value;
        } else if (key.compare(0, 11, "class_label") == 0) {
          class_struct.label = value;
        } else if (key.compare(0, 10, "class_data") == 0) {
          class_struct.class_data_file = value;
        } else if (key.compare(0, 12, "testing_data") == 0) {
          class_struct.testing_data_file = value;
        } else if (key.compare(0, 13, "training_data") == 0) {
          class_struct.training_data_file = value;
        } else if (key.compare(0, 7, "handles") == 0) {
          class_struct.handles_file = value;
        } else if (key.compare(0, 6, "corpus") == 0) {
          class_struct.corpus_file = value;
        } else if (key.compare(0, 6, "tweets") == 0) {
          class_struct.tweets_file = value;
        } else if (key.compare(0, 4, "seed") == 0) {
          class_struct.seed_file = value;
        }
        if (line_count == 9) {
          config.classes.insert(class_struct);
          line_count = 0;
          class_struct.clear();
        }
      }
    }
    ifs.close();
  }

  return 0;
}

int ClassifierConfig::LoadClassLabelsMap(Config& config,
                                         std::map<std::string, std::string>& class_labels_map) {
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    class_labels_map[config.iter->name] = config.iter->label;
  }
  return 0;
}


} // namespace inagist_classifiers
