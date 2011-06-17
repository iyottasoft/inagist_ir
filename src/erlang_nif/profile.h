/* profile.h */

#ifndef _INAGIST_DASHBOARD_PROFILE_ERL_INTERFACE_H_
#define _INAGIST_DASHBOARD_PROFILE_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitProfiler(const char* keytuples_extracter_config,
                 const char* language_detection_config,
                 const char* text_classifier_config,
                 const char* sentiment_analyser_config);

int GetProfile(const char* twitter_handle, unsigned int twitter_handle_len,
            unsigned char* locations, unsigned int locations_buffer_len,
            unsigned int* locations_len_ptr, unsigned int* locations_count_ptr,
            char* languages, unsigned int languages_buffer_len,
            unsigned int* languages_len_ptr, unsigned int* languages_count_ptr,
            char* text_classes_buffer, unsigned int text_classes_buffer_len,
            unsigned int* text_classes_len_ptr, unsigned int* text_classes_count_ptr,
            char* sub_classes_buffer, unsigned int sub_classes_buffer_len,
            unsigned int* sub_classes_len_ptr, unsigned int* sub_classes_count_ptr,
            unsigned char* text_class_contributors_buffer, unsigned int text_class_contributors_buffer_len,
            unsigned int* text_class_contributors_len_ptr, unsigned int* text_class_contributors_count_ptr,
            char* sentiment_buffer, unsigned int sentiment_buffer_len,
            const char* profile_name);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_DASHBOARD_PROFILE_ERL_INTERFACE_H_
