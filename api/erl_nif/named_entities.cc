#ifdef _CPLUSPLUS
#include <cstring>
#else
#include <string.h>
#endif

#include "named_entities.h"
#include "keytuples_extracter.h"

#define MAX_BUFFER_LEN 1024

inagist_trends::KeyTuplesExtracter g_kt;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitKeyTuplesExtracter(const char* keytuples_config) {

  if (g_kt.Init(keytuples_config) < 0) {
    std::cerr << "ERROR: could not initialize keytuples extracter\n";
    return -1;
  }

  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetKeywords(unsigned char* text_buffer, const unsigned int text_buffer_len,
                const unsigned int text_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                unsigned char* named_entities_buffer, const unsigned int named_entities_buffer_len,
                unsigned int* named_entities_len_ptr, unsigned int* named_entities_count_ptr) {

  int ret_value = 0;

  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;

  if ((ret_value = g_kt.GetKeyTuples(text_buffer, text_buffer_len, text_len,
                safe_status_buffer, safe_status_buffer_len,
                script_buffer, script_buffer_len,
                named_entities_buffer, named_entities_buffer_len,
                named_entities_len, named_entities_count)) < 0) {
    std::cerr << "ERROR: could not get keytuples\n";
    return -1;
  }

  *named_entities_len_ptr = named_entities_len;
  *named_entities_count_ptr = named_entities_count;

  return ret_value;
}

