/* keytuples.h */

#ifndef _INAGIST_ERLANG_API_KEYTUPLES_ERL_INTERFACE_H_
#define _INAGIST_ERLANG_API_KEYTUPLES_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitKeyTuplesExtracter(const char* stopwords_file_path,
         const char* dictionary_file_path,
         const char* unsafe_dictionary_file_path);

int GetKeyTuples(unsigned char* tweet, const unsigned int tweet_len,
                 char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                 char* script_buffer, const unsigned int script_buffer_len,
                 unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                 unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                 unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                 unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                 unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                 unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_ERLANG_API_KEYTUPLES_ERL_INTERFACE_H_
