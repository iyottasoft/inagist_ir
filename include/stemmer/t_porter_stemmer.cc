#include <iostream>
#include <string>
#include <cstring>
#include "porter_stemmer.h"

int main(int argc, char * argv[])
{

  if (argc != 2) {
    std::cout << "Usage: " + std::string(argv[0]) + " <word>" << std::endl;
    return -1;
  }

  inagist_stemmer::init_stemmer();

  char out_word[1024];
  int out_len = 0;
  int in_len = strlen(argv[1]);
  out_len = inagist_stemmer::stem(argv[1], in_len, out_word);
  if (out_len > 0 && out_len != in_len) 
    std::cout << out_word << std::endl;

  inagist_stemmer::free_stemmer();

  return 0;
}

