/* langd.cc */

#ifdef _CPLUSPLUS
#include <set>
#include <string>
#include <cstring>
#include <iostream>
#else
#include <string.h>
#endif

#include "language_detector.h"

inagist_classifiers::LanguageDetector g_language_detector;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitLanguageDetector(const char* config_file) {

  if (!config_file)
    return -1;

  if (g_language_detector.Init(std::string(config_file)) < 0)
    return -1;

  return 0;
}

// keywords and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int DetectLanguage(const char* tweet, const unsigned int tweet_len,
                   char* lang_buffer, const unsigned int lang_buffer_len) {
#ifdef LD_DEBUG
  std::cout << tweet << std::endl;
#endif
  std::string lang;
  int ret_value = 0;
  if ((ret_value = g_language_detector.DetectLanguage(std::string(tweet), tweet_len, lang)) < 0) {
    strcpy(lang_buffer, "ERR");
    return -1;
  }

  unsigned int len = lang.length();
  if (len > 1) {
    strcpy(lang_buffer, lang.c_str());
  } else if (len < 1) {
    strcpy(lang_buffer, "ERR");
    return -1;
  } else if (len == 0) {
    strcpy(lang_buffer, "XX");
    return 0;
  }

  return ret_value;
}

