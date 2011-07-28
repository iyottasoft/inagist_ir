#ifdef _CPLUSPLUS
#include <cstring>
#else
#include <string.h>
#endif

#include "keytuples.h"
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
int GetKeyTuples(unsigned char* text_buffer, const unsigned int text_buffer_len,
                 const unsigned int text_len,
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
                ) {

  int ret_value = 0;

  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;

  if ((ret_value = g_kt.GetKeyTuples(text_buffer, text_buffer_len, text_len,
                safe_status_buffer, safe_status_buffer_len,
                script_buffer, script_buffer_len,
                keywords_buffer, keywords_buffer_len,
                keywords_len, keywords_count,
                hashtags_buffer, hashtags_buffer_len,
                hashtags_len, hashtags_count,
                keyphrases_buffer, keyphrases_buffer_len,
                keyphrases_len, keyphrases_count
#ifdef INTENT_ENABLED
                , intent_buffer, intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                , sentiment_buffer, sentiment_buffer_len
#endif // SENTIMENT_ENABLED
                )) < 0) {
    std::cerr << "ERROR: could not get keytuples\n";
    return -1;
  }

  *keywords_len_ptr = keywords_len;
  *keywords_count_ptr = keywords_count;
  *hashtags_len_ptr = hashtags_len;
  *hashtags_count_ptr = hashtags_count;
  *keyphrases_len_ptr = keyphrases_len;
  *keyphrases_count_ptr = keyphrases_count;

  return ret_value;
}

