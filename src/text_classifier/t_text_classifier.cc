#include "text_classifier.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <set>
#include "twitter_api.h"
#include "twitter_searcher.h"
#include "test_utils.h"

inagist_classifiers::TextClassifier g_tc;

int Classify(std::string text) {

  std::string text_class;
  std::string top_classes;
  unsigned int top_classes_count;
  inagist_classifiers::Corpus test_corpus;
  std::cout << "\ntext: " << text << std::endl;
  if (g_tc.Classify(text, text.length(),
                  text_class,
                  top_classes, top_classes_count
#ifdef CLASSIFIER_DATA_TESTING_ENABLED
                  , test_corpus
#endif // CLASSIFIER_DATA_TESTING_ENABLED
#ifdef CLASS_CONTRIBUTORS_ENABLED
                  , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                 ) < 0) {
    std::cerr << "ERROR: could not find text class: " << text << std::endl;
  } else {
    std::cout << "text_class: " << text_class << std::endl;
    std::cout << "top_classes: " << top_classes << std::endl;
  }
  return 0;
}

int main(int argc, char* argv[]) {

  if (argc < 4 || argc > 6) {
    std::cout << "Usage: " << argv[0] << " \n\t<classifier_config> \n\t<keytuples_config> \n\t<0/1/2, 0-interactive/1-file/2-tweets/3-many tweets> \n\t[<debug_level>] \n\t[<input_file_name>/[handle]]\n";
    return -1;
  }

  const char* classifier_config = argv[1];
  const char* keytuples_config = argv[2];

  int input_type = atoi(argv[3]);

  unsigned int debug_level = 0;
  if (argc >= 5) {
    debug_level = atoi(argv[4]);
  }

  const char* input_value = NULL;
  if (argc == 6) {
    input_value = argv[5];
  }

  if (g_tc.Init(classifier_config) < 0) {
    std::cout << "ERROR: could not initialize text classifier\n";
    return -1;
  }

  int my_argc = 1;
  char* my_argv[1];
  char* temp_location = (char *) malloc(255);
  my_argv[0] = temp_location; 
  memset(temp_location, '\0', 255);
  strcpy(temp_location, keytuples_config); 
  if (g_tc.InitDependencies(my_argc, (char **) my_argv) < 0) {
    std::cerr << "ERROR: could not initialize dependencies for text classifier\n";
    return -1;
  }
  free(my_argv[0]);

  g_tc.SetDebugLevel(debug_level);

  std::string text;
  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;

#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED

  if (0 == input_type) {
    while (getline(std::cin, text)) {
      if (text.compare("exit") == 0 || text.compare("quit") == 0)
        break;
      Classify(text);
    }
  } else {
    if (inagist_utils::GetInputText(input_type, input_value, tweets) < 0) {
      std::cerr << "ERROR: could not input texts\n";
      return -1;
    }
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      Classify(*set_iter);
      if (2 == input_type)
        break;
    }
  }

  tweets.clear();

  g_tc.ClearDependencies();

  return 0;
}

