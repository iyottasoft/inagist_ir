#include "language_detector.h"
#include <iostream>
#include <cstdlib>
#include <cstring>

using namespace inagist_classifiers;

int main(int argc, char* argv[]) {

  if (argc < 5 || argc > 7) {
    std::cout << "Usage: " << argv[0] << " <classifier_config> <keytuples_config> <input_type> <output_type> [class_name] [output_file/input_file] [input_file/handle]\n";
    std::cout << "input_type:\n\t0 - twitter timeline\n\t1 - random selection of handles from training sources\n\t2 - all training sources (all handles)\n\t3 - given class(random handles)\n\t4 - use input file\n\t5 - tweets from handle\noutput_type:\n\t0 - stdout\n\t1 - class frequency file\n\t2 - html version\n";
    return -1;
  }

  const char* config_file = argv[1];
  const char* keytuples_config_file = argv[2];
  unsigned int input_type = atoi(argv[3]);
  unsigned int output_type = atoi(argv[4]);
  const char* input_file = NULL;
  const char* output_file = NULL;
  const char* class_name = NULL;
  const char* input_handle = NULL;

  if (8 == argc) {
    if (4 == input_type)
      input_file = argv[7];
    else if (5 == input_type)
      input_handle = argv[7];
    output_file = argv[6];
    if (3 == input_type || 4 == input_type)
      class_name = argv[5];
  }
  if (7 == argc) {
    if (input_type == 4)
      input_file = argv[6];
    else if (input_type == 5)
      input_handle = argv[6];
    else if (output_type > 0)
      output_file = argv[6];
    if (3 == input_type || 4 == input_type)
      class_name = argv[5];
  }
  if (6 == argc) {
    if (3 == input_type || 4 == input_type) {
      class_name = argv[5];
    } else {
      output_file = argv[5];
    }
  }

#ifdef TEST_DEBUG
  std::cout << "config_file: " << config_file << std::endl;
  std::cout << "keytuples_config: " << keytuples_config_file << std::endl;
  std::cout << "input_type: " << input_type << std::endl;
  std::cout << "output_type: " << output_type << std::endl;
  std::cout << "class_name: ";
  if (class_name)
    std::cout << class_name;
  std::cout << std::endl;
  std::cout << "input_file: ";
  if (input_file)
    std::cout << input_file;
  std::cout << std::endl;
  std::cout << "output_file: ";
  if (output_file)
    std::cout << output_file;
  std::cout << std::endl;
  std::cout << "input_handle: ";
  if (input_handle)
    std::cout << input_handle;
  std::cout << std::endl;
#endif

  const char* input_value = NULL;
  if (input_file)
    input_value = input_file;

  LanguageDetector *ld = (LanguageDetector*) new LanguageDetector();
  
  bool ignore_history = true;
  if (ld->Init(config_file, ignore_history) < 0) {
    std::cerr << "ERROR: could not initialize language detector\n";
    return -1;
  }

  int my_argc = 1;
  char* my_argv[1];
  char* temp_location = (char *) malloc(255);
  my_argv[0] = temp_location; 
  memset(temp_location, '\0', 255);
  strcpy(temp_location, keytuples_config_file);
  if (ld->InitDependencies(my_argc, (char **) my_argv) < 0) {
    std::cerr << "ERROR: could not initialize dependencies for language detection\n";
    return -1;
  }
  free(my_argv[0]);

  int ret_val = 0;
  // Twitter timeline is input and output to stdout. hence (0,0)
  if (ld->GetTestData(input_type, input_value, class_name, output_type, output_file) < 0) {
    std::cerr << "ERROR: could not get test data\n";
    ret_val = -1;
  }

  try {
    if (ld->ClearDependencies() < 0) {
      std::cerr << "ERROR: could not clear dependencies\n";
      ret_val = -1;
    }

    if (ld->Clear() < 0) {
      std::cerr << "ERROR: could not clear language detector\n";
      ret_val = -1;
    }

    delete ld;
  } catch (...) {
    std::cerr << "Exception raised\n";
    return -1;
  }

  return ret_val;
}
