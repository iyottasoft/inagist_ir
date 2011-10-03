#include "corpus_manager.h"
#include <iostream>
#include <cstdlib>

int main(int argc, char* argv[]) {

/*
  if (argc != 3 && argc != 5) {
    std::cout << "Usage: " << argv[0] << " <input_file_name> <0/1, 0-create/1-read> [<0/1> 0-create from tweet handles/1-text] [output_file_name]\n";
    return -1;
  }
*/

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input_file_name> <0/1, 0-lookup/1-print corpus>\n";
    return -1;
  }

  std::string input_file_name = std::string(argv[1]);
  // create corpus or read from supplied corpus
  int test_type = atoi(argv[2]);
  if (test_type != 0 && test_type != 1) {
    std::cout << "ERROR: invalid test type " << test_type << std::endl;
    return -1;
  }

  if (0 == test_type) {
    inagist_classifiers::Corpus corpus;
    if (inagist_classifiers::CorpusManager::LoadCorpus(input_file_name, corpus) < 0) {
      std::cout << "ERROR: could not load corpus with file: " << input_file_name << std::endl;
      return -1;
    }
    std::string line;
    double freq = 0;
    while (getline(std::cin, line)) {
      if (line.compare("exit") == 0)
        break;
      if ((freq = inagist_classifiers::CorpusManager::LookUp(corpus, line)) < 0)
        std::cout << "ERROR: could not get frequency for " << line << std::endl;
      if (freq == 0)
        std::cout << line << " not found\n";
      else
        std::cout << line << " = " << freq << std::endl;
    }
    inagist_classifiers::CorpusManager::ClearCorpus(corpus);
    return 0;
  }

  if (1 == test_type) {
    inagist_classifiers::Corpus corpus;
    inagist_classifiers::CorpusManager::LoadCorpus(input_file_name, corpus);
    inagist_classifiers::CorpusManager::PrintCorpus(corpus);
  }

  return 0;
}

