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
                 const unsigned int text_len
#ifdef PROFANITY_CHECK_ENABLED
                 , char* profanity_status_buffer, const unsigned int profanity_status_buffer_len
#endif // PROFANITY_CHECK_ENABLED
#ifdef SCRIPT_DETECTION_ENABLED
                 , char* script_buffer, const unsigned int script_buffer_len
#endif // SCRIPT_DETECTION_ENABLED
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
#ifdef LOCATION_ENABLED
                 , char* locations_buffer, const unsigned int locations_buffer_len,
                 unsigned int* locations_len_ptr, unsigned int* locations_count_ptr
#endif // LOCATION_ENABLED
#ifdef INTENT_ENABLED
                 , int* intent_valence_ptr
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                 , int* sentiment_valence_ptr
#endif // SENTIMENT_ENABLED
                );
#ifdef _CPLUSPLUS
} // extern "C"
#endif // if _CPLUSPLUS

#endif // _INAGIST_ERLANG_API_GIST_ERL_INTERFACE_H_
