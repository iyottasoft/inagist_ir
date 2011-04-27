#include <iostream>
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
    delete m_ld;
  }
  int TestFunction(std::string &text);
 private:
  LanguageDetector* m_ld;
};

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
  tgc->ReadArgs(argc, argv, test_input);
  tgc->PrintArgs(test_input);
  tgc->Test(test_input);
  delete tgc;
  tgc = (TestGetCorpus *) NULL;

  return 0;
}
