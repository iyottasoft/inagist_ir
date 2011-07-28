/* gist.h */

#ifndef _INAGIST_ERLANG_API_GIST_ERL_INTERFACE_H_
#define _INAGIST_ERLANG_API_GIST_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
#ifdef _CPLUSPLUS
int Init(const char* keytuples_extracter_config_file,
         const char* language_detection_config_file,
         const char* text_classification_config_file);

int InaGist(unsigned char* text_buffer, const unsigned int text_buffer_len,
            const unsigned int text_len,
            char* safe_status_buffer, const unsigned int safe_status_buffer_len,
            char* script_buffer, const unsigned int script_buffer_len
            , char* lang_class_buffer, const unsigned int lang_class_buffer_len
            , unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
            unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr
            , unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
            unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr
            , unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
            unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr
            , char* top_text_classes_buffer, const unsigned int top_text_classes_buffer_len,
            unsigned int* top_text_classes_len_ptr, unsigned int* top_text_classes_count_ptr
            , char* intent_buffer, const unsigned int intent_buffer_len
            , char* sentiment_buffer, const unsigned int sentiment_buffer_len
           );
}
#endif

#endif // _INAGIST_ERLANG_API_GIST_ERL_INTERFACE_H_
