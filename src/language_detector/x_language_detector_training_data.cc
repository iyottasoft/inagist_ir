#include "language_detector.h"
#include <iostream>
#include <cstring>
#include <cstdlib>

using namespace inagist_classifiers;

int main(int argc, char* argv[]) {

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <config_file_name> <keytuples_config>\n";
    return -1;
  }

  std::string classifier_config = argv[1];
  std::string keytuples_config = argv[2];

  inagist_classifiers::LanguageDetector ld;

  int my_argc = 1;
  char* my_argv[1];
  char* temp_location = (char*) malloc(255);
  my_argv[0] = temp_location;
  memset(temp_location, '\0', 255);
  strcpy(temp_location, keytuples_config.c_str());
  if (ld.InitDependencies(my_argc, (char**) my_argv) < 0) {
    std::cerr << "ERROR: could not init keytuples extracter" \
              << " for training. config_file: " << keytuples_config << std::endl;
    ld.Clear();
    return -1;
  }
  free(temp_location);

  if (ld.GetTrainingData(classifier_config.c_str()) < 0) {
    std::cout << "ERROR: could not get training data for lang detection\n";
  }

  ld.Clear();

  return 0;
}

