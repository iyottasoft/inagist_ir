#include "dictionary_map.h"
#include <iostream>

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <dictionary_map_file>\n";
    return -1;
  }

  inagist_utils::DictionaryMap dictionary_map;
  if (dictionary_map.Load(argv[1]) < 0) {
    std::cout << "Error: could not load dictionary_map\n";
    return -1;
  }

  std::string key;
  std::string value;
  while(getline(std::cin, key)) {
    if (key.compare("exit") == 0)
      break;
    if (key.compare("print") == 0) {
      if (dictionary_map.Print() < 0)
        std::cout << "Error: could not print dictionary_map\n";
    } else {
      if (dictionary_map.Find(key, value) == 1)
        std::cout << key << " = " << value << std::endl;
    }
  }

  return 0;
}

