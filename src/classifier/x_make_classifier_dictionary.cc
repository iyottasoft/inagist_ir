#include <iostream>
#include "classifier.h"

int main(int argc, char* argv[]) {

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <classifier_config> <dictionary_file>\n";
    return -1;
  }

  const char* classifier_config = argv[1];
  const char* classifier_dictionary_file = argv[2];

  inagist_classifiers::Classifier classifier;
  if (classifier.Init(classifier_config) < 0) {
    std::cerr << "ERROR: could not initialize text classifier\n";
    return -1;
  }

  if (classifier.MakeDictionary(classifier_dictionary_file) < 0) {
    std::cerr << "ERROR: could not make classifier dictionary\n";
  }

  return 0;
}
