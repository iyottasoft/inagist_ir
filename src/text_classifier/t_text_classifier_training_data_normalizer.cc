#include "text_classifier.h"

int main(int argc, char* argv[]) {

  if (argc < 2) {
    std::cout << "Usage: " << argv[0] << " <classifier_config>\n";
    return -1;
  }

  inagist_classifiers::TextClassifier text_classifier;
  text_classifier.Init(argv[1]);
  text_classifier.NormalizeFrequencies();

  return 0;
}
