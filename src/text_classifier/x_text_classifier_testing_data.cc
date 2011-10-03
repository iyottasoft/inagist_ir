#include "text_classifier.h"
#include <iostream>
#include <cstring>
#include <cstdlib>

int main(int argc, char* argv[]) {

  if (argc != 4) {
    std::cout << "Usage: " << argv[0] << " <classifier_config> <keytuples_config> <dictionary_override>\n";
    return -1;
  }

  std::string classifier_config = argv[1];
  std::string keytuples_config = argv[2];
  std::string dictionary_file = argv[3];

  inagist_classifiers::TextClassifier tc;

  int my_argc = 1;
  char* my_argv[1];
  char* temp_location = (char*) malloc(255);
  my_argv[0] = temp_location;
  memset(temp_location, '\0', 255);
  strcpy(temp_location, keytuples_config.c_str());
  if (tc.InitDependencies(my_argc, (char**) my_argv) < 0) {
    std::cerr << "ERROR: could not init keytuples extracter" \
              << " for training. config_file: " << keytuples_config << std::endl;
    return -1;
  }
  free(temp_location);

  if (tc.LoadKeyTuplesDictionary(dictionary_file.c_str()) < 0) {
    tc.Clear();
    return -1;
  }

  bool train_not_test;
  if (tc.GetData(train_not_test=true, classifier_config.c_str()) < 0) {
    std::cerr << "ERROR: could not get training data for lang detection\n";
    tc.Clear();
    return -1;
  }

  tc.Clear();

  return 0;
}

