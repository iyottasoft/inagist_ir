#include "text_classifier.h"
#include <iostream>

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <config_file_name>\n";
    return -1;
  }

  std::string config_file_name = argv[1];
  inagist_classifiers::TextClassifier tc;

  if (tc.GetTrainingData(config_file_name.c_str()) < 0) {
    std::cout << "ERROR: could not get training data for lang detection\n";
  }

  tc.Clear();

  return 0;
}

