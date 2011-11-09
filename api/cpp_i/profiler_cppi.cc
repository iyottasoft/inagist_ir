/* profile.cc */

#ifdef _CPLUSPLUS
#include <iostream>
#include <string>
#include <cstring>
#endif

#include "profiler_cppi.h"
#include "profiler.h"

inagist_dashboard::Profiler g_profiler;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitProfiler(const char* gist_maker_config) {

  if (!gist_maker_config) {
    std::cerr << "ERROR: invalid gist maker config file\n";
    return -1;
  }

  if (g_profiler.Init(gist_maker_config) < 0) {
    std::cerr << "ERROR: could not initialize gist maker" << std::endl;
    return -1;
  }

  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int SetProfilerDebugLevel(unsigned int debug_level) {
  g_profiler.SetDebugLevel(debug_level);
  return 0;
}

// text_classes are returned
#ifdef _CPLUSPLUS
extern "C"
#endif
int GetProfile(const char* twitter_handle, unsigned int twitter_handle_len,
      unsigned char* locations_buffer, unsigned int locations_buffer_len,
      unsigned int* locations_len_ptr, unsigned int* locations_count_ptr
#ifdef LANG_ENABLED
      , char* self_languages_buffer, unsigned int self_languages_buffer_len,
      unsigned int* self_languages_len_ptr, unsigned int* self_languages_count_ptr
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , char* self_text_classes_buffer, unsigned int self_text_classes_buffer_len,
      unsigned int* self_text_classes_len_ptr, unsigned int* self_text_classes_count_ptr
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
      , char* self_location_classes_buffer, unsigned int self_location_classes_buffer_len,
      unsigned int* self_location_classes_len_ptr, unsigned int* self_location_classes_count_ptr
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , unsigned char* self_text_class_contributors_buffer, unsigned int self_text_class_contributors_buffer_len,
      unsigned int* self_text_class_contributors_len_ptr, unsigned int* self_text_class_contributors_count_ptr
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
      , char* others_languages_buffer, unsigned int others_languages_buffer_len,
      unsigned int* others_languages_len_ptr, unsigned int* others_languages_count_ptr
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , char* others_text_classes_buffer, unsigned int others_text_classes_buffer_len,
      unsigned int* others_text_classes_len_ptr, unsigned int* others_text_classes_count_ptr
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
      , char* others_location_classes_buffer, unsigned int others_location_classes_buffer_len,
      unsigned int* others_location_classes_len_ptr, unsigned int* others_location_classes_count_ptr
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , unsigned char* others_text_class_contributors_buffer, unsigned int others_text_class_contributors_buffer_len,
      unsigned int* others_text_class_contributors_len_ptr, unsigned int* others_text_class_contributors_count_ptr
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef RECSYS_ENABLED
      , unsigned char* recommendations_buffer, unsigned int recommendations_buffer_len,
      unsigned int* recommendations_len_ptr, unsigned int* recommendations_count_ptr
#endif // RECSYS_ENABLED
      , const char* profile_name)  {


  if (!twitter_handle || twitter_handle_len < 1) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: invalid input twitter handle\n";
#endif
    return -1;
  }

  if (!locations_buffer
#ifdef LANG_ENABLED
      || !self_languages_buffer
      || !others_languages_buffer
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      || !self_text_classes_buffer
      || !self_text_class_contributors_buffer
      || !others_text_classes_buffer
      || !others_text_class_contributors_buffer
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
      || !self_location_classes_buffer
      || !others_location_classes_buffer
#endif // LOCATION_ENABLED
#ifdef RECSYS_ENABLED
      || !recommendations_buffer
#endif // RECSYS_ENABLED
     ) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: invalid output buffer(s) for profiling\n";
#endif
    return -1;
  }

  *locations_len_ptr = 0;
  *locations_count_ptr = 0;
  locations_buffer[0] = '\0';
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;

#ifdef LANG_ENABLED
  *self_languages_len_ptr = 0;
  *self_languages_count_ptr = 0;
  self_languages_buffer[0] = '\0';
  unsigned int self_languages_len = 0;
  unsigned int self_languages_count = 0;
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  *self_text_classes_len_ptr = 0;
  *self_text_classes_count_ptr = 0;
  self_text_classes_buffer[0] = '\0';
  unsigned int self_text_classes_len = 0;
  unsigned int self_text_classes_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LOCATION_ENABLED
  *self_location_classes_len_ptr = 0;
  *self_location_classes_count_ptr = 0;
  self_location_classes_buffer[0] = '\0';
  unsigned int self_location_classes_len = 0;
  unsigned int self_location_classes_count = 0;
#endif // LOCATION_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  *self_text_class_contributors_len_ptr = 0;
  *self_text_class_contributors_count_ptr = 0;
  self_text_class_contributors_buffer[0] = '\0';
  unsigned int self_text_class_contributors_len = 0;
  unsigned int self_text_class_contributors_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LANG_ENABLED
  *others_languages_len_ptr = 0;
  *others_languages_count_ptr = 0;
  others_languages_buffer[0] = '\0';
  unsigned int others_languages_len = 0;
  unsigned int others_languages_count = 0;
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  *others_text_classes_len_ptr = 0;
  *others_text_classes_count_ptr = 0;
  others_text_classes_buffer[0] = '\0';
  unsigned int others_text_classes_len = 0;
  unsigned int others_text_classes_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LOCATION_ENABLED
  *others_location_classes_len_ptr = 0;
  *others_location_classes_count_ptr = 0;
  others_location_classes_buffer[0] = '\0';
  unsigned int others_location_classes_len = 0;
  unsigned int others_location_classes_count = 0;
#endif // LOCATION_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  *others_text_class_contributors_len_ptr = 0;
  *others_text_class_contributors_count_ptr = 0;
  others_text_class_contributors_buffer[0] = '\0';
  unsigned int others_text_class_contributors_len = 0;
  unsigned int others_text_class_contributors_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef RECSYS_ENABLED
  *recommendations_len_ptr = 0;
  *recommendations_count_ptr = 0;
  recommendations_buffer[0] = '\0';
  unsigned int recommendations_len = 0;
  unsigned int recommendations_count = 0;
#endif // RECSYS_ENABLED

  int ret_value = 0;
  if ((ret_value = g_profiler.Profile(twitter_handle, twitter_handle_len,
                     locations_buffer, locations_buffer_len,
                     locations_len, locations_count
#ifdef LANG_ENABLED
                     , self_languages_buffer, self_languages_buffer_len,
                     self_languages_len, self_languages_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                     , self_text_classes_buffer, self_text_classes_buffer_len,
                     self_text_classes_len, self_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                     , self_location_classes_buffer, self_location_classes_buffer_len,
                     self_location_classes_len, self_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                     , self_text_class_contributors_buffer, self_text_class_contributors_buffer_len,
                     self_text_class_contributors_len, self_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
                     , others_languages_buffer, others_languages_buffer_len,
                     others_languages_len, others_languages_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                     , others_text_classes_buffer, others_text_classes_buffer_len,
                     others_text_classes_len, others_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                     , others_location_classes_buffer, others_location_classes_buffer_len,
                     others_location_classes_len, others_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                     , others_text_class_contributors_buffer, others_text_class_contributors_buffer_len,
                     others_text_class_contributors_len, others_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef RECSYS_ENABLED
                     , recommendations_buffer, recommendations_buffer_len,
                     recommendations_len, recommendations_count
#endif // RECSYS_ENABLED
                     , profile_name)) < 0) {
    std::cerr << "ERROR: could not profile twitter handle: " \
              << twitter_handle << std::endl;
    return -1;
  }

  *locations_len_ptr = locations_len;
  *locations_count_ptr = locations_count;
#ifdef LANG_ENABLED
  *self_languages_len_ptr = self_languages_len;
  *self_languages_count_ptr = self_languages_count;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  *self_text_classes_len_ptr = self_text_classes_len;
  *self_text_classes_count_ptr = self_text_classes_count;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
  *self_location_classes_len_ptr = self_location_classes_len;
  *self_location_classes_count_ptr = self_location_classes_count;
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  *self_text_class_contributors_len_ptr = self_text_class_contributors_len;
  *self_text_class_contributors_count_ptr = self_text_class_contributors_count;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
  *others_languages_len_ptr = others_languages_len;
  *others_languages_count_ptr = others_languages_count;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  *others_text_classes_len_ptr = others_text_classes_len;
  *others_text_classes_count_ptr = others_text_classes_count;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
  *others_location_classes_len_ptr = others_location_classes_len;
  *others_location_classes_count_ptr = others_location_classes_count;
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  *others_text_class_contributors_len_ptr = others_text_class_contributors_len;
  *others_text_class_contributors_count_ptr = others_text_class_contributors_count;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef RECSYS_ENABLED
  *recommendations_len_ptr = recommendations_len;
  *recommendations_count_ptr = recommendations_count;
#endif // RECSYS_ENABLED

  return ret_value;
}
