#ifdef _CPLUSPLUS
#include <cstring>
#else
#include <string.h>
#endif

#include "keytuples_extracter_cppi.h"
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
                ) {

  int ret_value = 0;

  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
  int intent_valence=0;
  int sentiment_valence=0;

  if ((ret_value = g_kt.GetKeyTuples(text_buffer, text_buffer_len, text_len,
                safe_status_buffer, safe_status_buffer_len,
                script_buffer, script_buffer_len,
                named_entities_buffer, named_entities_buffer_len,
                named_entities_len, named_entities_count,
                keywords_buffer, keywords_buffer_len,
                keywords_len, keywords_count,
                keyphrases_buffer, keyphrases_buffer_len,
                keyphrases_len, keyphrases_count
#ifdef INTENT_ENABLED
                , intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                , sentiment_valence
#endif // SENTIMENT_ENABLED
                )) < 0) {
    std::cerr << "ERROR: could not get keytuples\n";
    return -1;
  }

  *named_entities_len_ptr = named_entities_len;
  *named_entities_count_ptr = named_entities_count;
  *keywords_len_ptr = keywords_len;
  *keywords_count_ptr = keywords_count;
  *keyphrases_len_ptr = keyphrases_len;
  *keyphrases_count_ptr = keyphrases_count;
  *intent_valence_ptr = intent_valence;
  *sentiment_valence_ptr = sentiment_valence;

  return ret_value;
}

