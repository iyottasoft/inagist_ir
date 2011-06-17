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
                 const char* language_detection_config,
                 const char* text_classifier_config,
                 const char* sentiment_analyser_config) {

  if (!keytuples_extracter_config ||
      !language_detection_config ||
      !text_classifier_config ||
      !sentiment_analyser_config) {
    return -1;
  }

  if (g_profiler.Init(keytuples_extracter_config,
                      language_detection_config,
                      text_classifier_config,
                      sentiment_analyser_config) < 0) {
    return -1;
  }

  return 0;
}

// text_classes are returned
#ifdef _CPLUSPLUS
extern "C"
#endif
int GetProfile(const char* twitter_handle, unsigned int twitter_handle_len,
            unsigned char* locations_buffer, unsigned int locations_buffer_len,
            unsigned int* locations_len_ptr, unsigned int* locations_count_ptr,
            char* languages_buffer, unsigned int languages_buffer_len,
            unsigned int* languages_len_ptr, unsigned int* languages_count_ptr,
            char* text_classes_buffer, unsigned int text_classes_buffer_len,
            unsigned int* text_classes_len_ptr, unsigned int* text_classes_count_ptr,
            char* sub_classes_buffer, unsigned int sub_classes_buffer_len,
            unsigned int* sub_classes_len_ptr, unsigned int* sub_classes_count_ptr,
            unsigned char* text_class_contributors_buffer, unsigned int text_class_contributors_buffer_len,
            unsigned int* text_class_contributors_len_ptr, unsigned int* text_class_contributors_count_ptr,
            char* sentiment_buffer, unsigned int sentiment_buffer_len,
            const char* profile_name)  {

  if (!twitter_handle ||
      !locations_buffer||
      !languages_buffer ||
      !text_classes_buffer ||
      !sub_classes_buffer ||
      !text_class_contributors_buffer ||
      !sentiment_buffer) {
    std::cerr << "ERROR: invalid inputs\n";
    return -1;
  }

  *locations_len_ptr = 0;
  *locations_count_ptr = 0;
  locations_buffer[0] = '\0';
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;

  *languages_len_ptr = 0;
  *languages_count_ptr = 0;
  languages_buffer[0] = '\0';
  unsigned int languages_len = 0;
  unsigned int languages_count = 0;

  *text_classes_len_ptr = 0;
  *text_classes_count_ptr = 0;
  text_classes_buffer[0] = '\0';
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;

  *sub_classes_len_ptr = 0;
  *sub_classes_count_ptr = 0;
  sub_classes_buffer[0] = '\0';
  unsigned int sub_classes_len = 0;
  unsigned int sub_classes_count = 0;

  *text_class_contributors_len_ptr = 0;
  *text_class_contributors_count_ptr = 0;
  text_class_contributors_buffer[0] = '\0';
  unsigned int text_class_contributors_len = 0;
  unsigned int text_class_contributors_count = 0;

  sentiment_buffer[0] = '\0';

  int ret_value = 0;
  if ((ret_value = g_profiler.Profile(twitter_handle, twitter_handle_len,
                                      locations_buffer, locations_buffer_len,
                                      locations_len, locations_count,
                                      languages_buffer, languages_buffer_len,
                                      languages_len, languages_count,
                                      text_classes_buffer, text_classes_buffer_len,
                                      text_classes_len, text_classes_count,
                                      sub_classes_buffer, sub_classes_buffer_len,
                                      sub_classes_len, sub_classes_count,
                                      text_class_contributors_buffer, text_class_contributors_buffer_len,
                                      text_class_contributors_len, text_class_contributors_count,
                                      sentiment_buffer, sentiment_buffer_len,
                                      profile_name)) < 0) {
    std::cerr << "ERROR: could not profile twitter handle: " \
              << twitter_handle << std::endl;
    return -1;
  }

  *locations_len_ptr = locations_len;
  *locations_count_ptr = locations_count;
  *languages_len_ptr = languages_len;
  *languages_count_ptr = languages_count;
  *text_classes_len_ptr = text_classes_len;
  *text_classes_count_ptr = text_classes_count;
  *sub_classes_len_ptr = sub_classes_len;
  *sub_classes_count_ptr = sub_classes_count;
  *text_class_contributors_len_ptr = text_class_contributors_len;
  *text_class_contributors_count_ptr = text_class_contributors_count;

  return ret_value;
}

// just copy pasting from above. this function is meant for testing

// text_classes are returned
#ifdef _CPLUSPLUS
extern "C"
#endif
int GetProfileFromFile(const char* docs_file_name, unsigned int docs_file_name_len,
            unsigned char* locations_buffer, unsigned int locations_buffer_len,
            unsigned int* locations_len_ptr, unsigned int* locations_count_ptr,
            char* languages_buffer, unsigned int languages_buffer_len,
            unsigned int* languages_len_ptr, unsigned int* languages_count_ptr,
            char* text_classes_buffer, unsigned int text_classes_buffer_len,
            unsigned int* text_classes_len_ptr, unsigned int* text_classes_count_ptr,
            char* sub_classes_buffer, unsigned int sub_classes_buffer_len,
            unsigned int* sub_classes_len_ptr, unsigned int* sub_classes_count_ptr,
            unsigned char* text_class_contributors_buffer, unsigned int text_class_contributors_buffer_len,
            unsigned int* text_class_contributors_len_ptr, unsigned int* text_class_contributors_count_ptr,
            char* sentiment_buffer, unsigned int sentiment_buffer_len,
            const char* profile_name)  {

  if (!docs_file_name ||
      !locations_buffer ||
      !languages_buffer ||
      !text_classes_buffer ||
      !sub_classes_buffer ||
      !text_class_contributors_buffer ||
      !sentiment_buffer) {
    std::cerr << "ERROR: invalid inputs\n";
    return -1;
  }

  *locations_len_ptr = 0;
  *locations_count_ptr = 0;
  locations_buffer[0] = '\0';
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;

  *languages_len_ptr = 0;
  *languages_count_ptr = 0;
  languages_buffer[0] = '\0';
  unsigned int languages_len = 0;
  unsigned int languages_count = 0;

  *text_classes_len_ptr = 0;
  *text_classes_count_ptr = 0;
  text_classes_buffer[0] = '\0';
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;

  *sub_classes_len_ptr = 0;
  *sub_classes_count_ptr = 0;
  sub_classes_buffer[0] = '\0';
  unsigned int sub_classes_len = 0;
  unsigned int sub_classes_count = 0;

  *text_class_contributors_len_ptr = 0;
  *text_class_contributors_count_ptr = 0;
  text_class_contributors_buffer[0] = '\0';
  unsigned int text_class_contributors_len = 0;
  unsigned int text_class_contributors_count = 0;

  sentiment_buffer[0] = '\0';

  int ret_value = 0;
  if ((ret_value = g_profiler.ProfileFromFile(docs_file_name, docs_file_name_len,
                                      locations_buffer, locations_buffer_len,
                                      locations_len, locations_count,
                                      languages_buffer, languages_buffer_len,
                                      languages_len, languages_count,
                                      text_classes_buffer, text_classes_buffer_len,
                                      text_classes_len, text_classes_count,
                                      sub_classes_buffer, sub_classes_buffer_len,
                                      sub_classes_len, sub_classes_count,
                                      text_class_contributors_buffer, text_class_contributors_buffer_len,
                                      text_class_contributors_len, text_class_contributors_count,
                                      sentiment_buffer, sentiment_buffer_len,
                                      profile_name)) < 0) {
    std::cerr << "ERROR: could not profile twitter handle: " \
              << docs_file_name << std::endl;
    return -1;
  }

  *locations_len_ptr = locations_len;
  *locations_count_ptr = locations_count;
  *languages_len_ptr = languages_len;
  *languages_count_ptr = languages_count;
  *text_classes_len_ptr = text_classes_len;
  *text_classes_count_ptr = text_classes_count;
  *sub_classes_len_ptr = sub_classes_len;
  *sub_classes_count_ptr = sub_classes_count;
  *text_class_contributors_len_ptr = text_class_contributors_len;
  *text_class_contributors_count_ptr = text_class_contributors_count;

  return ret_value;
}

