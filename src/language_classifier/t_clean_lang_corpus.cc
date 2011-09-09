#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cstdlib>
#include <cstring>
#include "corpus_manager.h"
#include "classifier_config.h"

//#define TEST_DEBUG 1

int main(int argc, char* argv[]) {

  if (argc < 4 || argc > 5) {
    std::cout << "Usage: " << argv[0] << " \n\t<0/1 input_type, 0-corpus_file/1-config_file> \n\t<file_name> <output_type> \n\t<output_prefix>\n";
    return -1;
  }

  int input_type = atoi(argv[1]);
  if (input_type != 0 && input_type != 1) {
    std::cout << "ERROR: invalid input type\n";
    return -1;
  }

  std::string file_name = std::string(argv[2]);

  if (0 == input_type) {
    if (file_name.find(".config") != std::string::npos) {
      std::cout << "WARNING: config file given as input for corpus file? " << file_name << ". will be wiped out. check and rename!" << std::endl;
      return -1;
    }
  }

  int output_type = atoi(argv[3]);
  if (output_type != 0 && output_type != 1) {
    std::cout << "ERROR: invalid output type\n";
    return -1;
  }

  const char* output_prefix;
  std::string output_prefix_str;

  if (argc == 5) {
    output_prefix = argv[4];
    if (strcmp(output_prefix, "none") == 0) {
      std::cout << "WARNING: input files will be replaced\n";
    }
    output_prefix_str = std::string(output_prefix);
  } else {
    output_prefix_str = "clean_corpus_";
  }

  clean_corpus(input_type, file_name, output_prefix_str);

  return 0;
}
