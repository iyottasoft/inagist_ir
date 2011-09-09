#include "amazon_api.h"
#include <iostream>
#include <cstring>

int main(int argc, char* argv[]) {

  if (argc < 4) {
    std::cout << "Usage: " << argv[0] << " <python_file> <class_name> <function_name>\n";
    return -1;
  }

  const char* module_name = argv[1];
  const char* class_name = argv[2];
  const char* function_name = argv[3];

  inagist_api::AmazonAPI aapi;

  if (aapi.Init(module_name, class_name) < 0) {
    std::cerr << "ERROR: could not initialize amazon api\n";
    return -1;
  }

  if (strcmp(function_name, "item_search") == 0) {
    if (aapi.ItemSearch() < 0) {
      std::cerr << "ERROR: could not search the amazon api\n";
      return -1;
    }
  }

  return 0;
}

