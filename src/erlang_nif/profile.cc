/* profile.cc */

#ifdef _CPLUSPLUS
#include <iostream>
#include <string>
#include <cstring>
#endif

#include "profile.h"
#include "profiler.h"

inagist_dashboard::Profiler g_profiler;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitProfiler(const char* keytuples_extracter_config,
                 const char* text_classifier_config,
                 const char* language_detection_config) {

  if (!keytuples_extracter_config || !text_classifier_config || !language_detection_config)
    return -1;

  if (g_profiler.Init(keytuples_extracter_config,
                      text_classifier_config,
                      language_detection_config) < 0) {
    return -1;
  }

  return 0;
}

// class_names are returned
#ifdef _CPLUSPLUS
extern "C"
#endif
int Profile(const char* twitter_handle,
            unsigned int twitter_handle_len,
            char* class_names,
            unsigned int* class_names_len_ptr,
            unsigned int* class_names_count_ptr,
            const char* profile_name) {

  if (!twitter_handle || !class_names) {
    std::cerr << "ERROR: invalid inputs\n";
    return -1;
  }

  *class_names_len_ptr = 0;
  *class_names_count_ptr = 0;
  class_names[0] = '\0';

  std::string twitter_handle_str = std::string(twitter_handle);
  std::string class_names_str;
  //std::string profile_name_str = std::string(profile_name);
  std::string profile_name_str = std::string("/tmp/1");
  int ret_value = 0;
  if ((ret_value = g_profiler.Profile(twitter_handle_str,
                         class_names_str,
                         profile_name_str)) < 0) {
    std::cerr << "ERROR: could not profile twitter handle: " \
              << twitter_handle_str << std::endl;
    return -1;
  }

  if ((*class_names_len_ptr = class_names_str.length()) > 0) {
    strcpy(class_names, class_names_str.c_str());
    strcpy(class_names + *class_names_len_ptr, "|");
    *class_names_len_ptr += 1;
    *class_names_count_ptr = 1;
  }

  return ret_value;
}

