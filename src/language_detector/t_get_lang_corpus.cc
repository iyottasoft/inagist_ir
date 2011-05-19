#include <iostream>
#include <cstdlib>
#include <cstring>
#include "test_utils.h"
#include "language_detector.h"
#include "corpus_manager.h"

using namespace inagist_classifiers;

int main(int argc, char* argv[]) {

  if (argc < 4 || argc > 5) {
    std::cout << "Usage: " << argv[0] << " <classifier_config> <keytuples_config> <input_type> <input_value>\n";
    return -1;
  }

  std::string classifier_config = std::string(argv[1]);
  std::string keytuples_config = std::string(argv[2]);
  unsigned int input_type = atoi(argv[3]);
  const char* input_value = NULL;
  if (5 == argc) {
    input_value = argv[4];
  }

  LanguageDetector* m_ld = (LanguageDetector *) new LanguageDetector();

  // initialize dependencies
  int my_argc = 1;
  char* my_argv[1];
  char* temp_location = (char*) malloc(255);
  my_argv[0] = temp_location;
  memset(temp_location, '\0', 255);
  strcpy(temp_location, keytuples_config.c_str());
  if (m_ld->InitDependencies(my_argc, (char**) my_argv) < 0) {
    std::cerr << "ERROR: could not init keytuples extracter" \
              << " for training. config_file: " << keytuples_config << std::endl;
    return -1;
  }
  free(temp_location);

  Corpus corpus;

  std::string doc;
  std::set<std::string> docs;
  if (0 == input_type) {
    // interactive
    while (getline(std::cin, doc)) {
      if (doc.compare("quit") == 0)
        break;
      m_ld->GetCorpus(doc, corpus);
      if (!corpus.empty()) {
        CorpusManager::PrintCorpus(corpus);
        std::cout << doc << std::endl;
      }
    }
  } else {
    if (inagist_utils::GetInputText(input_type,
                                    input_value,
                                    docs) < 0) {
      std::cerr << "ERROR: could not get test input\n";
      docs.clear();
    }
  }

  std::set<std::string>::iterator doc_iter;
  for (doc_iter = docs.begin(); doc_iter != docs.end(); doc_iter++) {
    doc = *doc_iter;
    std::cout << doc << std::endl;
    m_ld->GetCorpus(doc, corpus);
    if (!corpus.empty()) {
      CorpusManager::PrintCorpus(corpus);
      std::cout << doc << std::endl;
    }
  }
  docs.clear();
  input_value = NULL;

  m_ld->ClearDependencies();
  m_ld->Clear();

  return 0;
}
