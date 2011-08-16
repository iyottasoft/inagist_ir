#include <iostream>
#include <cstring>
#include <cstdlib>
#include <set>
#include "gist_maker.h"
#include "twitter_api.h"
#include "twitter_searcher.h"
#include "test_utils.h"

#define T_MAX_BUFFER_LEN 1024 

int main(int argc, char *argv[]) {

  if (argc < 5 || argc > 6) {
    std::cout << "Usage: " << argv[0] << " <keytuples_config> <lang_config> " \
              << "<text_classifier_config> <input_type> [input_value]\n";
    return -1;
  }

  std::string keytuples_extracter_config = std::string(argv[1]);
  std::string language_detector_config = std::string(argv[2]);
  std::string text_classifier_config = std::string(argv[3]);

  unsigned int input_type = atoi(argv[4]);
  const char* input_value = NULL;
  if (6 == argc) {
    input_value = argv[5];
  }
  unsigned int stress_count = 1;
  if (6 == input_type) {
    stress_count = atoi(input_value);
    if (stress_count < 1 || stress_count > 10000) {
      std::cerr << "ERROR: valid value for stress_count is between 1 and 10k" << std::endl;
    }
    input_type = 3;
    input_value = NULL;
  }

  inagist::GistMaker gm;

  if (gm.Init(keytuples_extracter_config.c_str()
#ifdef LANG_ENABLED
              , language_detector_config.c_str()
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              , text_classifier_config.c_str()
#endif // TEXT_CLASSIFICATION_ENABLED
             ) < 0) {
    std::cerr << "ERROR: could not initialize gist maker\n";
    return -1;
  }

  std::string doc;
  std::set<std::string> docs;
  if (0 == input_type) {
    // interactive
    while (getline(std::cin, doc)) {
      if (doc.compare("quit") == 0)
        break;
      if (gm.GetGist(doc) < 0) {
        std::cerr << "ERROR: coult not get gist for doc:" << doc << std::endl;
      }
    }
    return 0;
  }

  for (unsigned int i=0; i < stress_count; i++) {
    if (inagist_utils::GetInputText(input_type,
                                    input_value,
                                    docs) < 0) {
      std::cerr << "ERROR: could not get test input\n";
      return -1;
    }

    if (docs.empty()) {
      std::cerr << "WARNING: no input text found\n";
      return 0;
    }

    std::set<std::string>::iterator doc_iter;
    for (doc_iter = docs.begin(); doc_iter != docs.end(); doc_iter++) {
      doc = *doc_iter;
      std::cout << std::endl;
      std::cout << doc << std::endl;
      if (gm.GetGist(doc) < 0) {
        std::cerr << "ERROR: could not get gist for doc:" << doc << std::endl;
      }
    }
    docs.clear();
  }
  input_value = NULL;

  return 0;
}
