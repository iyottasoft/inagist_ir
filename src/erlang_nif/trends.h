/* trends.h */

#ifndef _INAGIST_ERLANG_API_TRENDS_ERL_INTERFACE_H_
#define _INAGIST_ERLANG_API_TRENDS_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitTrendsManager(const char* stopwords_file_path,
         const char* dictionary_file_path,
         const char* unsafe_dictionary_file_path,
         const char* lang_detect_config_file_path,
         const char* channels_dictionary_file_path);

int GetKeywords(const unsigned char* tweet, const unsigned int tweet_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr,
                char* buffer1, const unsigned int buffer1_len,
                char* buffer2, const unsigned int buffer2_len,
                char* buffer3, const unsigned int buffer3_len,
                char* buffer4, const unsigned int buffer4_len);

int GetTrends(char* trends_buffer,
              unsigned int* trends_len_ptr,
              unsigned int* trends_count_ptr);

int GetTestTrends(const char* trends_file_path,
                  unsigned char* trends_buffer, const unsigned int trends_buffer_len,
                  unsigned int *trends_len_ptr, unsigned int *trends_count_ptr);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_ERLANG_API_TRENDS_ERL_INTERFACE_H_
