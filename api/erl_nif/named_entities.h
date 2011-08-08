/* named_entities.h */

#ifndef _INAGIST_ERLANG_API_NAMED_ENTITIES_ERL_INTERFACE_H_
#define _INAGIST_ERLANG_API_NAMED_ENTITIES_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitKeyTuplesExtracter(const char* keytuples_config);

int GetKeywords(unsigned char* text_buffer, const unsigned int text_buffer_len,
                const unsigned int text_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                unsigned char* named_entities_buffer, const unsigned int named_entities_buffer_len,
                unsigned int* named_entities_len_ptr, unsigned int* named_entities_count_ptr);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_ERLANG_API_NAMED_ENTITIES_ERL_INTERFACE_H_
