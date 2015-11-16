#include <iostream>
#include <cstring>
#include <cstdlib>
#include <set>
#include "trends_manager.h"
#include "test_utils.h"

#define T_MAX_BUFFER_LEN 1024 

int main(int argc, char *argv[]) {

  if (argc < 3 || argc > 4) {
    std::cout << "Usage: " << argv[0] << " <keytuples_config> <input_type> [input_value]" << std::endl;
    return -1;
  }

  const char* keytuples_config_file = argv[1];
  unsigned int input_type = atoi(argv[2]);
  char* input_value = NULL;
  if (4 == argc) {
    input_value = argv[3];
  }

  inagist_trends::TrendsManager tm;

  if (tm.Init(keytuples_config_file) < 0) {
    std::cerr << "ERROR: could not initialize trends manager\n";
    return -1;
  }

  std::set<std::string> trends;
  std::set<std::string>::iterator trends_iter;

  std::string doc;
  std::set<std::string> docs;
  if (0 == input_type) {
    // interactive
    while (getline(std::cin, doc)) {
      if (doc.compare("quit") == 0)
        break;
      if (tm.GetTrends(doc, trends) < 0) {
        std::cerr << "ERROR: coult not get gist for doc:" << doc << std::endl;
      }
      for (trends_iter = trends.begin(); trends_iter != trends.end(); trends_iter++) {
        std::cout << *trends_iter << std::endl;
      }
      trends.clear();
    }
  } else {
    if (inagist_utils::GetInputText(input_type,
                                    input_value,
                                    docs) < 0) {
      std::cerr << "ERROR: could not get test input\n";
      return -1;
    }
  }

  std::set<std::string>::iterator doc_iter;
  for (doc_iter = docs.begin(); doc_iter != docs.end(); doc_iter++) {
    doc = *doc_iter;
    std::cout << doc << std::endl;
    if (tm.GetTrends(doc, trends) < 0) {
      std::cerr << "ERROR: could not get gist for doc:" << doc << std::endl;
    }
    for (trends_iter = trends.begin(); trends_iter != trends.end(); trends_iter++) {
      std::cout << *trends_iter << std::endl;
    }
    trends.clear();
  }
  docs.clear();
  input_value = NULL;

  return 0;
}
