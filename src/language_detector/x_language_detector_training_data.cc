#include "language_detector.h"
#include <iostream>

using namespace inagist_classifiers;

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <config_file_name>\n";
    return -1;
  }

  std::string config_file_name = argv[1];
  LanguageDetector* ld = (LanguageDetector*) new LanguageDetector();

  if (ld->GetTrainingData(config_file_name.c_str()) < 0) {
    std::cout << "ERROR: could not get training data for lang detection\n";
  }

  ld->Clear();
  delete ld;

  return 0;
}

