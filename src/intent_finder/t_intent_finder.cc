#include <iostream>
#include <cstring>
#include <cstdlib>
#include <string>
#include <set>
#include "keytuples_extracter.h"
#include "intent_finder.h"
#include "test_utils.h"

inagist_classifiers::IntentFinder g_if;

int FindIntent(std::string text) {

  unsigned char text_buffer[1024];
  unsigned int text_buffer_len = 1024;
  char intent_buffer[255];
  int intent_buffer_len = 255;

  strcpy((char *) text_buffer, text.c_str()); 
  unsigned int text_len = text.length();
  int ret_val = 0;
  if ((ret_val = g_if.FindIntent(text_buffer, text_buffer_len, text_len,
                                 intent_buffer, intent_buffer_len)) < 0) {
    std::cout << "ERROR: could not get sentiment\n";
    return -1;
  }

  if (ret_val < 1)
    return 0;

  std::cout << std::endl << text_buffer << std::endl;
  text_buffer[0] = '\0';

  std::cout << "intent: " ;
  if (strlen(intent_buffer) > 0)
    std::cout << intent_buffer;
  std::cout << std::endl;

  return 0;
}

int main(int argc, char *argv[]) {

  if (argc < 3 || argc > 4) {
    std::cout << "Usage: " << argv[0] << "\n\t<keytuples_config_file_name>\n\t<0/1/2, 0-interactive, 1-file, 2-tweet, 3-many tweets, 4-inagist, 5-search, 6-stress_test, 7-followers>\n\t[<file>/<handle>/stress_count]\n";
    return -1;
  }

  std::string keytuples_config_file = std::string(argv[1]);
  if (keytuples_config_file.size() < 5) {
    std::cout << "ERROR: invalid config file\n";
    return -1;
  }

  unsigned int input_type = atoi(argv[2]);
  const char* input_value = NULL;

  // initialize keytuples extracter
  if (g_if.Init(keytuples_config_file.c_str()) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
    return -1; 
  }

  if (4 == argc) {
    input_value = argv[3];
  }

  std::string text;
  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;

  if (0 == input_type) {
    while (getline(std::cin, text)) {
      if (text.compare("exit") == 0 || text.compare("quit") == 0)
        break;
      FindIntent(text);
    }
  } else {
    if (inagist_utils::GetInputText(input_type, input_value, tweets) < 0) {
      std::cerr << "ERROR: could not input texts\n";
      return -1;
    }
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      FindIntent(*set_iter);
      if (2 == input_type)
        break;
    }
  }

  tweets.clear();
  g_if.Clear();

  return 0;
}
