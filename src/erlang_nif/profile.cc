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
                 const char* text_classifier_config) {

  if (!keytuples_extracter_config ||
      !language_detection_config ||
      !text_classifier_config) {
    return -1;
  }

  if (g_profiler.Init(keytuples_extracter_config,
                      language_detection_config,
                      text_classifier_config) < 0) {
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
      char* self_languages_buffer, unsigned int self_languages_buffer_len,
      unsigned int* self_languages_len_ptr, unsigned int* self_languages_count_ptr,
      char* self_text_classes_buffer, unsigned int self_text_classes_buffer_len,
      unsigned int* self_text_classes_len_ptr, unsigned int* self_text_classes_count_ptr,
      char* self_sub_classes_buffer, unsigned int self_sub_classes_buffer_len,
      unsigned int* self_sub_classes_len_ptr, unsigned int* self_sub_classes_count_ptr,
      unsigned char* self_text_class_contributors_buffer, unsigned int self_text_class_contributors_buffer_len,
      unsigned int* self_text_class_contributors_len_ptr, unsigned int* self_text_class_contributors_count_ptr,
      char* others_languages_buffer, unsigned int others_languages_buffer_len,
      unsigned int* others_languages_len_ptr, unsigned int* others_languages_count_ptr,
      char* others_text_classes_buffer, unsigned int others_text_classes_buffer_len,
      unsigned int* others_text_classes_len_ptr, unsigned int* others_text_classes_count_ptr,
      char* others_sub_classes_buffer, unsigned int others_sub_classes_buffer_len,
      unsigned int* others_sub_classes_len_ptr, unsigned int* others_sub_classes_count_ptr,
      unsigned char* others_text_class_contributors_buffer, unsigned int others_text_class_contributors_buffer_len,
      unsigned int* others_text_class_contributors_len_ptr, unsigned int* others_text_class_contributors_count_ptr,
      char* intent_buffer, unsigned int intent_buffer_len,
      char* sentiment_buffer, unsigned int sentiment_buffer_len,
      unsigned char* recommendations_buffer, unsigned int recommendations_buffer_len,
      unsigned int* recommendations_len_ptr, unsigned int* recommendations_count_ptr,
      const char* profile_name)  {

  if (!twitter_handle ||
      !locations_buffer||
      !self_languages_buffer ||
      !self_text_classes_buffer ||
      !self_sub_classes_buffer ||
      !self_text_class_contributors_buffer ||
      !others_languages_buffer ||
      !others_text_classes_buffer ||
      !others_sub_classes_buffer ||
      !others_text_class_contributors_buffer ||
      !recommendations_buffer||
      !intent_buffer ||
      !sentiment_buffer) {
    std::cerr << "ERROR: invalid inputs\n";
    return -1;
  }

  *locations_len_ptr = 0;
  *locations_count_ptr = 0;
  locations_buffer[0] = '\0';
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;

  *self_languages_len_ptr = 0;
  *self_languages_count_ptr = 0;
  self_languages_buffer[0] = '\0';
  unsigned int self_languages_len = 0;
  unsigned int self_languages_count = 0;

  *self_text_classes_len_ptr = 0;
  *self_text_classes_count_ptr = 0;
  self_text_classes_buffer[0] = '\0';
  unsigned int self_text_classes_len = 0;
  unsigned int self_text_classes_count = 0;

  *self_sub_classes_len_ptr = 0;
  *self_sub_classes_count_ptr = 0;
  self_sub_classes_buffer[0] = '\0';
  unsigned int self_sub_classes_len = 0;
  unsigned int self_sub_classes_count = 0;

  *self_text_class_contributors_len_ptr = 0;
  *self_text_class_contributors_count_ptr = 0;
  self_text_class_contributors_buffer[0] = '\0';
  unsigned int self_text_class_contributors_len = 0;
  unsigned int self_text_class_contributors_count = 0;

  *others_languages_len_ptr = 0;
  *others_languages_count_ptr = 0;
  others_languages_buffer[0] = '\0';
  unsigned int others_languages_len = 0;
  unsigned int others_languages_count = 0;

  *others_text_classes_len_ptr = 0;
  *others_text_classes_count_ptr = 0;
  others_text_classes_buffer[0] = '\0';
  unsigned int others_text_classes_len = 0;
  unsigned int others_text_classes_count = 0;

  *others_sub_classes_len_ptr = 0;
  *others_sub_classes_count_ptr = 0;
  others_sub_classes_buffer[0] = '\0';
  unsigned int others_sub_classes_len = 0;
  unsigned int others_sub_classes_count = 0;

  *others_text_class_contributors_len_ptr = 0;
  *others_text_class_contributors_count_ptr = 0;
  others_text_class_contributors_buffer[0] = '\0';
  unsigned int others_text_class_contributors_len = 0;
  unsigned int others_text_class_contributors_count = 0;

  intent_buffer[0] = '\0';
  sentiment_buffer[0] = '\0';

  *recommendations_len_ptr = 0;
  *recommendations_count_ptr = 0;
  recommendations_buffer[0] = '\0';
  unsigned int recommendations_len = 0;
  unsigned int recommendations_count = 0;

  int ret_value = 0;
  if ((ret_value = g_profiler.Profile(twitter_handle, twitter_handle_len,
                     locations_buffer, locations_buffer_len,
                     locations_len, locations_count,
                     self_languages_buffer, self_languages_buffer_len,
                     self_languages_len, self_languages_count,
                     self_text_classes_buffer, self_text_classes_buffer_len,
                     self_text_classes_len, self_text_classes_count,
                     self_sub_classes_buffer, self_sub_classes_buffer_len,
                     self_sub_classes_len, self_sub_classes_count,
                     self_text_class_contributors_buffer, self_text_class_contributors_buffer_len,
                     self_text_class_contributors_len, self_text_class_contributors_count,
                     others_languages_buffer, others_languages_buffer_len,
                     others_languages_len, others_languages_count,
                     others_text_classes_buffer, others_text_classes_buffer_len,
                     others_text_classes_len, others_text_classes_count,
                     others_sub_classes_buffer, others_sub_classes_buffer_len,
                     others_sub_classes_len, others_sub_classes_count,
                     others_text_class_contributors_buffer, others_text_class_contributors_buffer_len,
                     others_text_class_contributors_len, others_text_class_contributors_count,
                     intent_buffer, intent_buffer_len,
                     sentiment_buffer, sentiment_buffer_len,
                     recommendations_buffer, recommendations_buffer_len,
                     recommendations_len, recommendations_count,
                     profile_name)) < 0) {
    std::cerr << "ERROR: could not profile twitter handle: " \
              << twitter_handle << std::endl;
    return -1;
  }

  *locations_len_ptr = locations_len;
  *locations_count_ptr = locations_count;
  *self_languages_len_ptr = self_languages_len;
  *self_languages_count_ptr = self_languages_count;
  *self_text_classes_len_ptr = self_text_classes_len;
  *self_text_classes_count_ptr = self_text_classes_count;
  *self_sub_classes_len_ptr = self_sub_classes_len;
  *self_sub_classes_count_ptr = self_sub_classes_count;
  *self_text_class_contributors_len_ptr = self_text_class_contributors_len;
  *self_text_class_contributors_count_ptr = self_text_class_contributors_count;
  *others_languages_len_ptr = others_languages_len;
  *others_languages_count_ptr = others_languages_count;
  *others_text_classes_len_ptr = others_text_classes_len;
  *others_text_classes_count_ptr = others_text_classes_count;
  *others_sub_classes_len_ptr = others_sub_classes_len;
  *others_sub_classes_count_ptr = others_sub_classes_count;
  *others_text_class_contributors_len_ptr = others_text_class_contributors_len;
  *others_text_class_contributors_count_ptr = others_text_class_contributors_count;
  *recommendations_len_ptr = recommendations_len;
  *recommendations_count_ptr = recommendations_count;

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
            char* self_languages_buffer, unsigned int self_languages_buffer_len,
            unsigned int* self_languages_len_ptr, unsigned int* self_languages_count_ptr,
            char* self_text_classes_buffer, unsigned int self_text_classes_buffer_len,
            unsigned int* self_text_classes_len_ptr, unsigned int* self_text_classes_count_ptr,
            char* self_sub_classes_buffer, unsigned int self_sub_classes_buffer_len,
            unsigned int* self_sub_classes_len_ptr, unsigned int* self_sub_classes_count_ptr,
            unsigned char* self_text_class_contributors_buffer, unsigned int self_text_class_contributors_buffer_len,
            unsigned int* self_text_class_contributors_len_ptr, unsigned int* self_text_class_contributors_count_ptr,
            char* intent_buffer, unsigned int intent_buffer_len,
            char* sentiment_buffer, unsigned int sentiment_buffer_len,
            unsigned char* recommendations_buffer, unsigned int recommendations_buffer_len,
            unsigned int* recommendations_len_ptr, unsigned int* recommendations_count_ptr,
            const char* profile_name)  {

  if (!docs_file_name ||
      !locations_buffer ||
      !self_languages_buffer ||
      !self_text_classes_buffer ||
      !self_sub_classes_buffer ||
      !self_text_class_contributors_buffer ||
      !intent_buffer ||
      !sentiment_buffer) {
    std::cerr << "ERROR: invalid inputs\n";
    return -1;
  }

  *locations_len_ptr = 0;
  *locations_count_ptr = 0;
  locations_buffer[0] = '\0';
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;

  *self_languages_len_ptr = 0;
  *self_languages_count_ptr = 0;
  self_languages_buffer[0] = '\0';
  unsigned int self_languages_len = 0;
  unsigned int self_languages_count = 0;

  *self_text_classes_len_ptr = 0;
  *self_text_classes_count_ptr = 0;
  self_text_classes_buffer[0] = '\0';
  unsigned int self_text_classes_len = 0;
  unsigned int self_text_classes_count = 0;

  *self_sub_classes_len_ptr = 0;
  *self_sub_classes_count_ptr = 0;
  self_sub_classes_buffer[0] = '\0';
  unsigned int self_sub_classes_len = 0;
  unsigned int self_sub_classes_count = 0;

  *self_text_class_contributors_len_ptr = 0;
  *self_text_class_contributors_count_ptr = 0;
  self_text_class_contributors_buffer[0] = '\0';
  unsigned int self_text_class_contributors_len = 0;
  unsigned int self_text_class_contributors_count = 0;

  intent_buffer[0] = '\0';
  sentiment_buffer[0] = '\0';

  *recommendations_len_ptr = 0;
  *recommendations_count_ptr = 0;
  recommendations_buffer[0] = '\0';
  unsigned int recommendations_len = 0;
  unsigned int recommendations_count = 0;


  int ret_value = 0;
  if ((ret_value = g_profiler.ProfileFromFile(docs_file_name, docs_file_name_len,
                                      locations_buffer, locations_buffer_len,
                                      locations_len, locations_count,
                                      self_languages_buffer, self_languages_buffer_len,
                                      self_languages_len, self_languages_count,
                                      self_text_classes_buffer, self_text_classes_buffer_len,
                                      self_text_classes_len, self_text_classes_count,
                                      self_sub_classes_buffer, self_sub_classes_buffer_len,
                                      self_sub_classes_len, self_sub_classes_count,
                                      self_text_class_contributors_buffer, self_text_class_contributors_buffer_len,
                                      self_text_class_contributors_len, self_text_class_contributors_count,
                                      intent_buffer, intent_buffer_len,
                                      sentiment_buffer, sentiment_buffer_len,
                                      recommendations_buffer, recommendations_buffer_len,
                                      recommendations_len, recommendations_count,
                                      profile_name)) < 0) {
    std::cerr << "ERROR: could not profile twitter handle: " \
              << docs_file_name << std::endl;
    return -1;
  }

  *locations_len_ptr = locations_len;
  *locations_count_ptr = locations_count;
  *self_languages_len_ptr = self_languages_len;
  *self_languages_count_ptr = self_languages_count;
  *self_text_classes_len_ptr = self_text_classes_len;
  *self_text_classes_count_ptr = self_text_classes_count;
  *self_sub_classes_len_ptr = self_sub_classes_len;
  *self_sub_classes_count_ptr = self_sub_classes_count;
  *self_text_class_contributors_len_ptr = self_text_class_contributors_len;
  *self_text_class_contributors_count_ptr = self_text_class_contributors_count;
  *recommendations_len_ptr = recommendations_len;
  *recommendations_count_ptr = recommendations_count;

  return ret_value;
}

