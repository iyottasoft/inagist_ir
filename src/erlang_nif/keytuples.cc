#ifdef _CPLUSPLUS
#include <cstring>
#else
#include <string.h>
#endif

#include "keytuples.h"
#include "keytuples_extracter.h"

inagist_trends::KeyTuplesExtracter g_kt;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitKeyTuplesExtracter(const char* stopwords_file_path,
         const char* dictionary_file_path,
         const char* unsafe_dictionary_file_path) {

  if (g_kt.Init(stopwords_file_path,
           dictionary_file_path,
           unsafe_dictionary_file_path) < 0) {
    std::cerr << "ERROR: could not initialize keytuples extracter\n";
    return -1;
  }

  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetKeyTuples(unsigned char* text, const unsigned int text_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr) {

  int ret_value = 0;
  if ((ret_value = g_kt.GetKeyTuples(text, text_len,
                safe_status_buffer, safe_status_buffer_len,
                script_buffer, script_buffer_len,
                (unsigned char*) keywords_buffer, keywords_buffer_len,
                keywords_len_ptr, keywords_count_ptr,
                (unsigned char*) hashtags_buffer, hashtags_buffer_len,
                hashtags_len_ptr, hashtags_count_ptr,
                (unsigned char*) keyphrases_buffer, keyphrases_buffer_len,
                keyphrases_len_ptr, keyphrases_count_ptr)) < 0) {
    std::cerr << "ERROR: could not get keytuples\n";
    return -1;
  }

  return ret_value;
}
