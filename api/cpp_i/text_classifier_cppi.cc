/* text_classifier_cppi.cc */

#ifdef _CPLUSPLUS
#include <set>
#include <string>
#include <cstring>
#include <iostream>
#else
#include <string.h>
#endif

#include "text_classifier.h"
#include "text_classifier_cppi.h"

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

// named_entities and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int ClassifyText(const char* text, const unsigned int text_len,
                 char* text_classes_buffer, const unsigned int text_classes_buffer_len,
                 unsigned int* text_classes_len_ptr, unsigned int* text_classes_count_ptr) {

#ifdef LD_DEBUG
  std::cout << text << std::endl;
#endif

  std::string text_class;
  std::string top_classes;
  unsigned int top_classes_count = 0;

  int ret_value = g_text_classifier.Classify(std::string(text), text_len,
                                         text_class, top_classes, top_classes_count);

  *text_classes_len_ptr = top_classes.length();
  *text_classes_count_ptr = top_classes_count;

  return ret_value;
}

