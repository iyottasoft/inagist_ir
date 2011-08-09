#include "classifier_config.h"

int main(int argc, char* argv[]) {

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input_config_file> <output_config_file>" << std::endl;
    return -1;
  }

  const char* input_config_file = argv[1];
  const char* output_config_file = argv[2];

  inagist_classifiers::Config config;
  if (inagist_classifiers::ClassifierConfig::Read(input_config_file, config) < 0) {
    std::cerr << "ERROR: could not read config from file: " << input_config_file << std::endl;
    return -1;
  }

  if (inagist_classifiers::ClassifierConfig::Write(config, output_config_file) < 0) {
    std::cerr << "ERROR: could not write config to file: " << output_config_file << std::endl;
    return -1;
  }

  return 0;
}
