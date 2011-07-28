/* sentiment.h */

#ifndef _INAGIST_ERLANG_API_SENTIMENT_ERL_INTERFACE_H_
#define _INAGIST_ERLANG_API_SENTIMENT_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitKeyTuplesExtracter(const char* keytuples_config);

int GetSentiment(unsigned char* text_buffer, const unsigned int text_buffer_len,
                 const unsigned int text_len,
                 char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                 char* script_buffer, const unsigned int script_buffer_len,
                 unsigned char* sentiment_buffer, const unsigned int sentiment_buffer_len,
                 unsigned int* sentiment_len_ptr, unsigned int* sentiment_count_ptr);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_ERLANG_API_SENTIMENT_ERL_INTERFACE_H_
