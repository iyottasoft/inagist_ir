/* profile.h */

#ifndef _INAGIST_DASHBOARD_PROFILE_ERL_INTERFACE_H_
#define _INAGIST_DASHBOARD_PROFILE_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitProfiler(const char* keytuples_extracter_config,
                 const char* text_classifier_config,
                 const char* language_detection_config);

int Profile(const char* twitter_handle_str,
            unsigned int twitter_handle_len,
            char* dominant_class_str,
            unsigned int* dominant_class_len_ptr,
            unsigned int* dominant_class_count_ptr,
            const char* profile_name_str=NULL); // this is a file name!
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_DASHBOARD_PROFILE_ERL_INTERFACE_H_
