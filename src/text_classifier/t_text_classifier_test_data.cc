#include "text_classifier.h"
#include <iostream>
#include <cstring>
#include <cstdlib>

using namespace inagist_classifiers;

//#define TEST_DEBUG 1

int main(int argc, char* argv[]) {

  if (argc < 5 || argc > 8) {
    std::cout << "Usage: " << argv[0] << " <classifier_config> <keytuples_config> <input_type> <output_type>\n";
    std::cout << "\t[debug_value]\n\t[input file/handle/class]\n\t[output file/expected_class]\n";
    std::cout << "input_type:\n\t0-9 - regular testing options\n\t10 - random selection of handles from training sources\n\t11 - all training sources (all handles)\n\t12 - given class (random handles)\n\t13 - use training files\noutput_type:\n\t0 - stdout\n\t1 - class frequency file\n\t2 - html version\n\t3 - all test data files (prod version)\n";
    return -1;
  }

  const char* classifier_config_file = argv[1];
  const char* keytuples_config_file = argv[2];
  unsigned int input_type = atoi(argv[3]); 
  unsigned int output_type = atoi(argv[4]); 
  unsigned int debug_level = 0;
  const char* input_value = NULL;
  const char* output_value = NULL;
  if (argc == 6) {
    debug_level = atoi(argv[5]);
  }
  if (argc == 7) {
    input_value = argv[6];
  }
  if (argc == 8) {
    output_value = argv[7];
  }

#ifdef TEST_DEBUG
  std::cout << "config_file: ";
  if (classifier_config_file)
    std::cout << classifier_config_file;
  std::cout << std::endl;

  std::cout << "keytuples_config: ";
  if (keytuples_config_file)
    std::cout << keytuples_config_file;
  std::cout << std::endl;

  std::cout << "input_type: " << input_type << std::endl;
  std::cout << "output_type: " << output_type << std::endl;

  std::cout << "input_value: ";
  if (input_value)
    std::cout << input_value;
  std::cout << std::endl;

  std::cout << "output_value: ";
  if (output_value)
    std::cout << output_value;
  std::cout << std::endl;
#endif

  TextClassifier *tc = (TextClassifier *) new TextClassifier();

  bool ignore_history = true;
  if (tc->Init(classifier_config_file, ignore_history) < 0) {
    std::cerr << "ERROR: could not initialize text classifier\n";
    return -1;
  }

  int my_argc = 1;
  char* my_argv[1];
  char* temp_location = (char *) malloc(255);
  my_argv[0] = temp_location; 
  memset(temp_location, '\0', 255);
  strcpy(temp_location, keytuples_config_file);
  if (tc->InitDependencies(my_argc, (char **) my_argv) < 0) {
    std::cerr << "ERROR: could not initialize dependencies for text classifier\n";
    return -1;
  }
  free(my_argv[0]);

  if (debug_level > 0) {
    tc->SetDebugLevel(debug_level);
  }

  int ret_val = 0;
  if (tc->GetTestData(input_type, input_value, output_type, output_value) < 0) {
    std::cerr << "ERROR: could not get test data\n";
    ret_val = -1;
  }

  try {
    if (tc->ClearDependencies() < 0) {
      std::cerr << "ERROR: could not clear dependencies\n";
      ret_val = -1;
    }

    if (tc->Clear() < 0) {
      std::cerr << "ERROR: could not clear text classifier\n";
      ret_val = -1;
    }

    delete tc;
  } catch (...) {
    std::cerr << "Exception raised\n";
    return -1;
  }

  return ret_val;
}
