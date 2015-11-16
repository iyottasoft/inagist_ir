#include "dictionary_set.h"
#include <iostream>

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <dictionary_set_file>\n";
    return -1;
  }

  inagist_utils::DictionarySet dictionary_set;
  if (dictionary_set.Load(argv[1]) < 0) {
    std::cout << "Error: could not load dictionary_set\n";
    return -1;
  }

  std::string word;
  while(getline(std::cin, word)) {
    if (word.compare("exit") == 0)
      break;
    if (word.compare("print") == 0) {
      if (dictionary_set.Print() < 0)
        std::cout << "Error: could not print dictionary_set\n";
    } else {
      if (dictionary_set.Find((unsigned char*) word.c_str()) == 1)
        std::cout << word << " : present in this dictionary_set\n";
    }
  }

  return 0;
}

