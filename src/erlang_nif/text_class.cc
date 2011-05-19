/* langd.cc */

#ifdef _CPLUSPLUS
#include <set>
#include <string>
#include <cstring>
#include <iostream>
#else
#include <string.h>
#endif

#include "text_class.h"
#include "text_classifier.h"

inagist_classifiers::TextClassifier g_text_classifier;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitTextClassifier(const char* config_file) {

  if (!config_file)
    return -1;

  if (g_text_classifier.Init(std::string(config_file)) < 0)
    return -1;

  return 0;
}

// keywords and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int ClassifyText(const char* text, const unsigned int text_len,
                 char* text_class_buffer, const unsigned int text_class_buffer_len) {
#ifdef LD_DEBUG
  std::cout << text << std::endl;
#endif
  std::string text_class;
  int ret_value = 0;
  if ((ret_value = g_text_classifier.Classify(std::string(text), text_len, text_class)) < 0) {
    strcpy(text_class_buffer, "ERR");
    return -1;
  }

  unsigned int len = text_class.length();
  if (len > 1) {
    strcpy(text_class_buffer, text_class.c_str());
  } else if (len < 1) {
    strcpy(text_class_buffer, "ERR");
    return -1;
  } else if (len == 0) {
    strcpy(text_class_buffer, "XX");
    return 0;
  }

  return ret_value;
}

