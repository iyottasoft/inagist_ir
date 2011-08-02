#include "text_classifier.h"
#include <iostream>
#include <cstring>
#include <cstdlib>

using namespace inagist_classifiers;

#define TEST_DEBUG 1

int main(int argc, char* argv[]) {

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <classifier_config> <keytuples_config>\n";
    return -1;
  }

  const char* classifier_config = argv[1];
  const char* keytuples_config = argv[2];

#ifdef TEST_DEBUG
  std::cout << "classifier_config: ";
  if (classifier_config)
    std::cout << classifier_config;
  std::cout << std::endl;

  std::cout << "keytuples_config: ";
  if (keytuples_config)
    std::cout << keytuples_config;
  std::cout << std::endl;
#endif

  TextClassifier *tc = (TextClassifier *) new TextClassifier();

  bool ignore_history = true;
  if (tc->Init(classifier_config, ignore_history) < 0) {
    std::cerr << "ERROR: could not initialize text classifier\n";
    return -1;
  }

  int my_argc = 1;
  char* my_argv[1];
  char* temp_location = (char *) malloc(255);
  my_argv[0] = temp_location; 
  memset(temp_location, '\0', 255);
  strcpy(temp_location, keytuples_config);
  if (tc->InitDependencies(my_argc, (char **) my_argv) < 0) {
    std::cerr << "ERROR: could not initialize dependencies for text classifier\n";
    return -1;
  }
  free(my_argv[0]);

  int ret_val = 0;
  unsigned int input_type = 0;
  const char* input_file = NULL;
  const char* input_handle = NULL;
  std::string expected_class_name;
  unsigned int output_type = 3;
  const char* output_file = NULL;
  if (tc->GetTestData(input_type, input_file, input_handle, expected_class_name, output_type, output_file) < 0) {
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
