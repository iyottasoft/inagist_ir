#include "dictionary.h"
#include <iostream>

int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <dictionary_file>\n";
    return -1;
  }

  inagist_utils::Dictionary dictionary;
  if (dictionary.Load(argv[1]) < 0) {
    std::cout << "Error: could not load dictionary\n";
    return -1;
  }

  std::string word;
  while(getline(std::cin, word)) {
    if (word.compare("exit") == 0)
      break;
    if (word.compare("print") == 0) {
      if (dictionary.Print() < 0)
        std::cout << "Error: could not print dictionary\n";
    } else {
      if (dictionary.Find((unsigned char*) word.c_str()) == 1)
        std::cout << word << " : present in this dictionary\n";
    }
  }

  return 0;
}

