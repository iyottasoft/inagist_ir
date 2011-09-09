/* language_classifier_cppi.cc */

#ifdef _CPLUSPLUS
#include <set>
#include <string>
#include <cstring>
#include <iostream>
#else
#include <string.h>
#endif

#include "language_classifier.h"
#include "language_classifier_cppi.h"

#define LC_DEBUG 3

inagist_classifiers::LanguageClassifier g_language_classifier;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitLanguageClassifier(const char* config_file) {

  if (!config_file)
    return -1;

  if (g_language_classifier.Init(std::string(config_file)) < 0)
    return -1;

  return 0;
}

// named_entities and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int CallClassify(const char* tweet, const unsigned int tweet_len,
                 char* lang_buffer, const unsigned int lang_buffer_len) {
#ifdef LC_DEBUG
  std::cout << tweet << std::endl;
#endif // LC_DEBUG
  std::string lang;
  std::string top_classes;
  unsigned int top_classes_count = 0;
  int ret_value = 0;
  if ((ret_value = g_language_classifier.Classify(std::string(tweet), tweet_len,
                                                lang, top_classes, top_classes_count)) < 0) {
#ifdef LC_DEBUG
    std::cerr << "ERROR: could not classify language\n";
#endif // LC_DEBUG
    return -1;
  }

  unsigned int len = lang.length();
  if (len > 1) {
    strcpy(lang_buffer, lang.c_str());
  } else if (len < 1) {
    strcpy(lang_buffer, "RR");
    return -1;
  } else if (len == 0) {
    strcpy(lang_buffer, "XX");
    return 0;
  }

  return ret_value;
}

