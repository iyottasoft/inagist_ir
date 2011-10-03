/* profile.h */

#ifndef _INAGIST_DASHBOARD_PROFILE_ERL_INTERFACE_H_
#define _INAGIST_DASHBOARD_PROFILE_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitProfiler(const char* keytuples_extracter_config,
                 const char* language_detection_config,
                 const char* text_classifier_config);

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
      int* intent_valence_ptr,
      int* sentiment_valence_ptr,
      unsigned char* recommendations_buffer, unsigned int recommendations_buffer_len,
      unsigned int* recommendations_len_ptr, unsigned int* recommendations_count_ptr,
      const char* profile_name);

#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_DASHBOARD_PROFILE_ERL_INTERFACE_H_
