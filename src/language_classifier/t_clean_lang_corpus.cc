#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cstdlib>
#include <cstring>
#include "language_classifier.h"

//#define TEST_DEBUG 1

int main(int argc, char* argv[]) {

  if (argc < 7 || argc > 8) {
    std::cout << "Usage: " << argv[0] << " \n\t<classifier_config> \n\t<keytuples_config> \n\t<clean_type>\n\t<0/1 input_type, 0-corpus_file/1-all_corpus_files>\n\t <0/1 output_type, 0-stdout, 1-output_file> \n\t<corpus_file_name or some dummy name> \n\t[<output_suffix> (\"none\" forces replacement)]\n";
    return -1;
  }

  const char* classifier_config = argv[1];
  const char* keytuples_config = argv[2];

  unsigned int clean_type = atoi(argv[3]);
  if (clean_type > 3) {
    std::cout << "ERROR: invalid clean type\n";
    return -1;
  }

  unsigned int input_type = atoi(argv[4]);
  if (input_type != 0 && input_type != 1) {
    std::cout << "ERROR: invalid input type\n";
    return -1;
  }

  int output_type = atoi(argv[5]);
  if (output_type != 0 && output_type != 1) {
    std::cout << "ERROR: invalid output type\n";
    return -1;
  }

  std::string file_name;
  if (0 == input_type) {
    file_name = std::string(argv[6]);
  } else if (1 == input_type) {
    file_name = std::string(classifier_config);
  }

  if (0 == input_type) {
    if (file_name.find(".config") != std::string::npos) {
      std::cout << "WARNING: config file given as input for corpus file? " << file_name << ". will be wiped out. check and rename!" << std::endl;
      return -1;
    }
  }

  const char* output_suffix;
  std::string output_suffix_str;

  if (argc == 8) {
    output_suffix = argv[7];
    if (strcmp(output_suffix, "none") == 0) {
      std::cout << "WARNING: input files will be replaced\n";
    }
    output_suffix_str = std::string(output_suffix);
  } else {
    output_suffix_str = "clean_corpus";
  }

  inagist_classifiers::LanguageClassifier lc;

  if (lc.Init(classifier_config) < 0) {
    std::cout << "ERROR: could not initialize text classifier\n";
    return -1;
  }

  int my_argc = 1;
  char* my_argv[1];
  char* temp_location = (char *) malloc(255);
  my_argv[0] = temp_location; 
  memset(temp_location, '\0', 255);
  strcpy(temp_location, keytuples_config); 
  if (lc.InitDependencies(my_argc, (char **) my_argv) < 0) {
    std::cerr << "ERROR: could not initialize dependencies for text classifier\n";
    return -1;
  }
  free(my_argv[0]);

  lc.CleanCorpus(input_type, file_name, output_suffix_str, clean_type);

  lc.ClearDependencies();

  return 0;
}
