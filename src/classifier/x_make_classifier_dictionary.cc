#include <iostream>
#include <cstdlib>
#include "classifier.h"

int main(int argc, char* argv[]) {

  if (argc < 3 || argc > 4) {
    std::cout << "Usage: " << argv[0] << "\n\t<classifier_config>\n\t<dictionary_file>\n\t[corpus_type 0-model,1-training,2-testing]\n";
    return -1;
  }

  const char* classifier_config = argv[1];
  const char* classifier_dictionary_file = argv[2];
  unsigned int corpus_type = 0;
  if (argc == 4) {
    corpus_type = atoi(argv[3]);
  }

  inagist_classifiers::Classifier classifier;
  if (classifier.Init(classifier_config, true, corpus_type) < 0) {
    std::cerr << "ERROR: could not initialize text classifier\n";
    return -1;
  }

  if (classifier.MakeDictionary(classifier_dictionary_file) < 0) {
    std::cerr << "ERROR: could not make classifier dictionary\n";
  }

  return 0;
}
