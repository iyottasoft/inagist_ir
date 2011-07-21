#ifdef _CPLUSPLUS
#include <cstring>
#else
#include <string.h>
#endif

#include "intent.h"
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
int GetIntent(unsigned char* text, const unsigned int text_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                unsigned char* intent_buffer, const unsigned int intent_buffer_len,
                unsigned int* intent_len_ptr, unsigned int* intent_count_ptr) {

  int ret_value = 0;

  unsigned int intent_len = 0;
  unsigned int intent_count = 0;

  if ((ret_value = g_kt.GetKeyTuples(text, text_len,
                safe_status_buffer, safe_status_buffer_len,
                script_buffer, script_buffer_len,
                (char *) intent_buffer, intent_buffer_len)) < 0) {
    std::cerr << "ERROR: could not get keytuples\n";
    return -1;
  } else {
    intent_len = strlen((char *) intent_buffer);
    if (intent_len > 0)
      intent_count = 1;
  }

  *intent_len_ptr = intent_len;
  *intent_count_ptr = intent_count;

  return ret_value;
}

