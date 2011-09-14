#include <iostream>
#include "language_classifier.h"

int main(int argc, char* argv[]) {

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <classifier_config> <lang_prior_freqs_file>\n";
    return -1;
  }

  const char* classifier_config = argv[1];
  const char* classifier_prior_freqs_file = argv[2];

  inagist_classifiers::LanguageClassifier lc;
  if (lc.Init(classifier_config) < 0) {
    std::cerr << "ERROR: could not initialize text classifier\n";
    return -1;
  }

  if (lc.MakePriorFreqsFile(classifier_prior_freqs_file) < 0) {
    std::cerr << "ERROR: could not make classifier dictionary\n";
  }

  return 0;
}
