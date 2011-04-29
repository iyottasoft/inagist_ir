#include <iostream>
#include <cstdlib>
#include <cstring>
#include "test_input_handler.h"
#include "language_detector.h"
#include "corpus_manager.h"

using namespace inagist_test_utils;
using namespace inagist_classifiers;

class TestGetCorpus : public TestInputHandler {
 public:
  TestGetCorpus() {
    m_ld = (LanguageDetector *) new LanguageDetector();
  }
  ~TestGetCorpus() {
    m_ld->ClearDependencies();
    m_ld->Clear();
    delete m_ld;
  }
  int TestFunction(std::string &text);
  int Init(const std::string& keytuples_config);
 private:
  LanguageDetector* m_ld;
};

int TestGetCorpus::Init(const std::string& keytuples_config) {

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

  return 0;
}

int TestGetCorpus::TestFunction(std::string &text) {
  Corpus corpus;
  std::cout << text << std::endl;
  m_ld->GetCorpus(text, corpus);
  if (!corpus.empty()) {
    CorpusManager::PrintCorpus(corpus);
    std::cout << text << std::endl;
  }
  return 0;
}

int main(int argc, char* argv[]) {

  if (argc != 4) {
    std::cout << "Usage: " << argv[0] << " <classifier_config> <keytuples_config> <input_type>\n";
    return -1;
  }

  TestGetCorpus* tgc = (TestGetCorpus *) new TestGetCorpus();

  TestInput test_input;

  std::string keytuples_config = std::string(argv[2]);
  tgc->Init(keytuples_config);

  // the following 3 functions are in TestInputHandler
  tgc->ReadArgs(argc, argv, test_input);
  tgc->PrintArgs(test_input);
  tgc->Test(test_input); // this calls virtual TestFunction above
  delete tgc;
  tgc = (TestGetCorpus *) NULL;

  return 0;
}
