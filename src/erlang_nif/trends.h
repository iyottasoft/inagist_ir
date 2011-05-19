/* trends.h */

#ifndef _INAGIST_ERLANG_API_TRENDS_ERL_INTERFACE_H_
#define _INAGIST_ERLANG_API_TRENDS_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitTrendsManager(const char* keytuples_config_file);

int GetTrends(char* docs_buffer, unsigned int docs_len, unsigned int docs_count,
              char* trends_buffer, unsigned int* trends_len_ptr, unsigned int* trends_count_ptr);

int ProcessTrends(char* trends_buffer, unsigned int* trends_len_ptr, unsigned int* trends_count_ptr);

int GetTestTrendsFromFile(const char* trends_file_path,
                  unsigned char* trends_buffer, const unsigned int trends_buffer_len,
                  unsigned int *trends_len_ptr, unsigned int *trends_count_ptr);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_ERLANG_API_TRENDS_ERL_INTERFACE_H_
