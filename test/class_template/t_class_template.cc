#include "class_template.h"
#include <iostream>

int main(int argc, char* argv[]) {
  if (argc < 1) {
    std::cout << "Usage: " << argv[0] << " \n";
    return -1;
  }

  inagist_test::ClassTemplate ct;

  return 0;
}

