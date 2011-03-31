#include "text_classifier.h"
#include <iostream>

int main(int argc, char* argv[]) {

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <keytuples_config> <handle>\n";
    return -1;
  }

  std::string keytuples_config = argv[1];
  std::string handle = argv[2];

  inagist_classifiers::TextClassifier tc;

  if (tc.InitTraining(keytuples_config.c_str()) < 0) {
    std::cerr << "ERROR: could not init keytuples extracter" \
              << " for training. config_file: " << keytuples_config << std::endl;
    return -1;
  }

  inagist_classifiers::Corpus corpus;
  if (tc.GetTrainingData(handle, corpus) < 0) {
    std::cerr << "ERROR: could not get training data for lang detection\n";
  }

  inagist_classifiers::CorpusManager::PrintCorpus(corpus);

  tc.Clear();

  return 0;
}

