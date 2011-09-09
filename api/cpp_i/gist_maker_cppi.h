/* gist_maker_cppi.h */

#ifndef _INAGIST_ERLANG_API_GIST_ERL_INTERFACE_H_
#define _INAGIST_ERLANG_API_GIST_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitGistMaker(const char* gist_maker_config_file);

int CallMakeGist(unsigned char* text_buffer, const unsigned int text_buffer_len,
            const unsigned int text_len,
            char* safe_status_buffer, const unsigned int safe_status_buffer_len,
            char* script_buffer, const unsigned int script_buffer_len
#ifdef LANG_ENABLED
            , char* lang_class_buffer, const unsigned int lang_class_buffer_len
#endif // LANG_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
            , unsigned char* named_entities_buffer, const unsigned int named_entities_buffer_len,
            unsigned int* named_entities_len_ptr, unsigned int* named_entities_count_ptr
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
            , unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
            unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
            , unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
            unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
            , char* top_text_classes_buffer, const unsigned int top_text_classes_buffer_len,
            unsigned int* top_text_classes_len_ptr, unsigned int* top_text_classes_count_ptr
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
            , char* intent_buffer, const unsigned int intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
            , char* sentiment_buffer, const unsigned int sentiment_buffer_len
#endif // SENTIMENT_ENABLED
           );
#ifdef _CPLUSPLUS
} // extern "C"
#endif // if _CPLUSPLUS

#endif // _INAGIST_ERLANG_API_GIST_ERL_INTERFACE_H_
