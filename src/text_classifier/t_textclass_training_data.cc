#include "text_classifier.h"
#include <iostream>

int main(int argc, char* argv[]) {

  if (argc != 4) {
    std::cout << "Usage: " << argv[0] << " <keytuples_config> <dictionary_override> <handle>\n";
    return -1;
  }

  std::string keytuples_config = argv[1];
  std::string dictionary_file = argv[2];
  std::string handle = argv[3];

  inagist_classifiers::TextClassifier tc;

  if (tc.InitTraining(keytuples_config.c_str()) < 0) {
    std::cerr << "ERROR: could not init keytuples extracter" \
              << " for training. config_file: " << keytuples_config << std::endl;
    return -1;
  }

  if (tc.LoadKeyTuplesDictionary(dictionary_file.c_str()) < 0) {
    tc.Clear();
    return -1;
  }

  inagist_classifiers::Corpus corpus;
  int count = 0;
  if ((count = tc.GetTrainingData(handle, corpus)) < 0) {
    std::cerr << "ERROR: could not get training data for lang detection\n";
  } else {
    std::cout << "Corpus of size " << count << " generated for " << handle << std::endl;
  }

  inagist_classifiers::CorpusManager::PrintCorpus(corpus);

  tc.Clear();

  return 0;
}

