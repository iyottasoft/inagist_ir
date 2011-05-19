#include "test_utils.h"
#include <iostream>
#include <cstdlib>

int main(int argc, char* argv[]) {

  if (argc < 2 || argc > 3) {
    std::cerr << "Usage: " << argv[0] << " <input_type> [input_value]\n";
    return -1;
  }

  unsigned int input_type = atoi(argv[1]);
  const char* input_value = NULL;
  if (3 == argc) {
    input_value = argv[2];
  }

  std::set<std::string> docs;
  if (inagist_utils::GetInputText(input_type, input_value, docs) < 0) {
    std::cerr << "ERROR: could not input text.\n" << std::endl;
    return -1;
  }

  std::set<std::string>::iterator docs_iter;
  for (docs_iter = docs.begin(); docs_iter != docs.end(); docs_iter++) {
    std::cout << *docs_iter << std::endl;
  }
  docs.clear();

  return 0;
}
