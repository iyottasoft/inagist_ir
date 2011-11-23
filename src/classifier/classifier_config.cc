#include "classifier_config.h"
#include <iostream>
#include <fstream>
#include <sys/stat.h>

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
    unsigned int line_count = 0;
    unsigned int class_count = 0;
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
      value.assign(line.c_str(), loc+1, (line.length()-loc-1)); // loc is 0 indexed, hence sub another 1
      //std::cout << key << std::endl;
      //std::cout << value << std::endl;
      if (key.compare(0, 17, "class_frequencies") == 0) {
        config.class_freqs_file = value;
      } else if (key.compare(0, 16, "test_frequencies") == 0) {
        config.test_freqs_file = value;
      } else if (key.compare(0, 11, "num_classes") == 0) {
        config.num_classes = atoi(value.c_str());
      } else {
        line_count++;
        if (key.compare(0, 10, "class_name") == 0) {
          class_struct.name = value;
          loc = key.find(".", 0);
          if (loc == std::string::npos) {
            std::cout << "ERROR: invalid config file entry in line: " << line << std::endl;
          }
          value.assign(key.c_str(), loc+1, (key.length()-loc-1));
          class_struct.number = value;
        } else if (key.compare(0, 11, "class_label") == 0) {
          class_struct.label = value;
        } else if (key.compare(0, 10, "class_data") == 0) {
          class_struct.class_data_file = value;
        } else if (key.compare(0, 12, "testing_data") == 0) {
          class_struct.testing_data_file = value;
        } else if (key.compare(0, 18, "testing_timestamps") == 0) {
          class_struct.testing_timestamps_file = value;
        } else if (key.compare(0, 14, "testing_corpus") == 0) {
          class_struct.testing_corpus_file = value;
        } else if (key.compare(0, 14, "testing_tweets") == 0) {
          class_struct.testing_tweets_file = value;
        } else if (key.compare(0, 13, "training_data") == 0) {
          class_struct.training_data_file = value;
        } else if (key.compare(0, 16, "training_handles") == 0) {
          class_struct.training_handles_file = value;
        } else if (key.compare(0, 19, "training_timestamps") == 0) {
          class_struct.training_timestamps_file = value;
        } else if (key.compare(0, 15, "training_corpus") == 0) {
          class_struct.training_corpus_file = value;
        } else if (key.compare(0, 15, "training_tweets") == 0) {
          class_struct.training_tweets_file = value;
        } else if (key.compare(0, 4, "seed") == 0) {
          struct stat stat_struct;
          if (0 == stat(value.c_str(), &stat_struct)) {
            class_struct.seed_file = value;
          }
        }
        if (line_count == 13) {
          config.classes.insert(class_struct);
          line_count = 0;
          class_struct.Clear();
          class_count++;
          if (class_count >= config.num_classes) {
            break;
          }
        }
      }
    }
    ifs.close();
    if (class_count != config.num_classes) {
      std::cerr << "WARNING: corrupt config file? invalid class_count." << std::endl;
    }
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

int ClassifierConfig::LoadClassNumbersMap(Config& config,
                                          std::map<std::string, std::string>& class_numbers_map) {
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    class_numbers_map[config.iter->name] = config.iter->number;
  }
  return 0;
}

int ClassifierConfig::Write(Config& config, const char* config_file_name) {

  if (config.classes.empty()) {
    std::cerr << "ERROR: invalid config\n";
    return -1;
  }

  std::ofstream ofs(config_file_name);
  if (!ofs.is_open()) {
    std::cerr << "ERROR: could not write to file: " << config_file_name << std::endl;
    return -1;
  }

  ofs << "class_frequencies=" << config.class_freqs_file << std::endl; 
  ofs << "test_frequencies=" << config.test_freqs_file << std::endl;
  ofs << "num_classes=" << config.num_classes << std::endl;
  unsigned int count = 0;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    ofs << "class_name." << count << "=" << config.iter->name << std::endl;
    ofs << "class_label." << count << "=" << config.iter->label << std::endl;
    ofs << "class_data." << count << "=" << config.iter->class_data_file << std::endl;
    ofs << "testing_data." << count << "=" << config.iter->testing_data_file << std::endl;
    ofs << "testing_timestamps." << count << "=" << config.iter->testing_timestamps_file << std::endl;
    ofs << "testing_corpus." << count << "=" << config.iter->testing_corpus_file << std::endl;
    ofs << "testing_tweets." << count << "=" << config.iter->testing_tweets_file << std::endl; 
    ofs << "training_data." << count << "=" << config.iter->training_data_file << std::endl;
    ofs << "training_handles." << count << "=" << config.iter->training_handles_file << std::endl;
    ofs << "training_timestamps." << count << "=" << config.iter->training_timestamps_file << std::endl;
    ofs << "training_corpus." << count << "=" << config.iter->training_corpus_file << std::endl;
    ofs << "training_tweets." << count << "=" << config.iter->training_tweets_file << std::endl; 
    ofs << "seed." << count << "=" << config.iter->seed_file << std::endl;
    count++;
  }
  ofs.close();

  return 1;
}

} // namespace inagist_classifiers
