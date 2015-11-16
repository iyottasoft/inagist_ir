#include "naive_bayes_classifier.h"
#include <iostream>
#include <cstdlib>

int main(int argc, char* argv[]) {

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <classifier_config> " \
              << "<class_prior_freqs_file> <0/1 - 0-training, 1-testing>\n";
    return -1;
  }

  const char* classifier_config = argv[1];
  const char* class_prior_freqs_file = argv[2];
  unsigned int input_type = atoi(argv[3]);
  bool train_not_test;
  switch (input_type) {
    case 0:
      train_not_test = true;
      break;
    case 1:
      train_not_test = false;
      break;
    default:
      return -1;
      break;
  }

  inagist_classifiers::NaiveBayesClassifier nbc;
  bool ignore_history;
  if (nbc.PrepareNaiveBayes(classifier_config, train_not_test, ignore_history=true) < 0) {
    std::cerr << "ERROR: could not initialize text classifier\n";
    return -1;
  }

  if (nbc.MakePriorProbabilitiesFile(class_prior_freqs_file) < 0) {
    std::cerr << "ERROR: could not make classifier dictionary\n";
  }

  if (nbc.Clear() < 0) {
    std::cerr << "ERROR: could not clear member data in naive bayes classifier\n";
    return -1;
  }

  return 0;
}
