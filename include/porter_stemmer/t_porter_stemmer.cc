#include <iostream>
#include <string>
#include <cstring>
#include "porter_stemmer.h"

int main(int argc, char * argv[])
{

  if (argc != 2 && argc != 3) {
    std::cout << "Usage: " + std::string(argv[0]) + " [<word> | <-f file_name>]\n";
    return -1;
  }

  porter_stemmer::init_stemmer();

  if (argc == 2) {
    char out_word[1024];
    int out_len = 0;
    int in_len = strlen(argv[1]);
    out_len = porter_stemmer::stem(argv[1], in_len, out_word);
    if (out_len > 0 && out_len != in_len) 
      std::cout << out_word << std::endl;
  } else if (argc == 3) {
    if (porter_stemmer::stemfile(argv[2]) < 0)
      std::cout << "Error: couldn't stem from file\n";
  }

  porter_stemmer::free_stemmer();

  return 0;
}

