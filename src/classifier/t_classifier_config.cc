#include <iostream>
#include "classifier_config.h"

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <config_file_name>\n";
    return -1;
  }

  std::string config_file_name = std::string(argv[1]);
  inagist_classifiers::Config config;
  if (inagist_classifiers::ClassifierConfig::Read(config_file_name.c_str(), config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
    return -1;
  }

  std::cout << config.test_data_file << std::endl;
  std::cout << config.freqs_file << std::endl;
  if (config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " << config_file_name << std::endl;
  } else {
    std::cout << config.classes.size() << " classes" << std::endl;
    for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
      std::cout << config.iter->name << std::endl;
    }
  }
  inagist_classifiers::ClassifierConfig::Clear(config);

  // TODO (balaji) write code here to read the test data file and ensure that
  // all classes have test data. if not send a message. all check for the all_classes entry
  // and if the sum of individual classes is equal to the all_class value

  return 0;
}
