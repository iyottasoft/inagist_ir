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

int GetKeyTuples(unsigned char* text, const unsigned int text_len,
                 char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                 char* script_buffer, const unsigned int script_buffer_len,
                 unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                 unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                 unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                 unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                 unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                 unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr
#ifdef INTENT_ENABLED
                 , char* intent_buffer, const unsigned int intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                 , char* sentiment_buffer, const unsigned int sentiment_buffer_len
#endif // SENTIMENT_ENABLED
                );
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_ERLANG_API_KEYTUPLES_ERL_INTERFACE_H_
