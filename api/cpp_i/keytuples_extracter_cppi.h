/* keytuples.h */

#ifndef _INAGIST_ERLANG_API_KEYTUPLES_ERL_INTERFACE_H_
#define _INAGIST_ERLANG_API_KEYTUPLES_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitKeyTuplesExtracter(const char* keytuples_config);

int GetKeyTuples(unsigned char* text_buffer, const unsigned int text_buffer_len,
                 const unsigned int text_len,
                 char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                 char* script_buffer, const unsigned int script_buffer_len,
                 unsigned char* named_entities_buffer, const unsigned int named_entities_buffer_len,
                 unsigned int* named_entities_len_ptr, unsigned int* named_entities_count_ptr,
                 unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                 unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                 unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                 unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr
#ifdef INTENT_ENABLED
                 , int* intent_valence_ptr
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                 , int* sentiment_valence_ptr
#endif // SENTIMENT_ENABLED
                );
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_ERLANG_API_KEYTUPLES_ERL_INTERFACE_H_
