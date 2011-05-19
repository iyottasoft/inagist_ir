#ifdef _CPLUSPLUS
#include <cstring>
#else
#include <string.h>
#endif

#include "gist.h"
#include "gist_maker.h"

inagist::GistMaker g_gm;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitGistMaker(const char* keytuples_extracter_config_file,
                    const char* language_detection_config_file,
                    const char* text_classification_config_file,
                    const char* sentiment_analyser_config_file) {

  if (g_gm.Init(keytuples_extracter_config_file,
                language_detection_config_file,
                text_classification_config_file,
                sentiment_analyser_config_file) < 0) {
    std::cerr << "ERROR: could not initialize gist maker\n";
    return -1;
  }

  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetGist(const unsigned char* text, const unsigned int text_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                char* lang_buffer, const unsigned int lang_buffer_len,
                unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr,
                char* text_class_buffer, const unsigned int text_class_buffer_len,
                char* sub_class_buffer, const unsigned int sub_class_buffer_len,
                char* sentiment_buffer, const unsigned int sentiment_buffer_len) {

  int ret_value = 0;
  if ((ret_value = g_gm.GetGist(text, text_len,
                safe_status_buffer, safe_status_buffer_len,
                script_buffer, script_buffer_len,
                lang_buffer, lang_buffer_len,
                (unsigned char*) keywords_buffer, keywords_buffer_len,
                keywords_len_ptr, keywords_count_ptr,
                (unsigned char*) hashtags_buffer, hashtags_buffer_len,
                hashtags_len_ptr, hashtags_count_ptr,
                (unsigned char*) keyphrases_buffer, keyphrases_buffer_len,
                keyphrases_len_ptr, keyphrases_count_ptr,
                text_class_buffer, text_class_buffer_len,
                sub_class_buffer, sub_class_buffer_len,
                sentiment_buffer, sentiment_buffer_len)) < 0) {
    std::cerr << "ERROR: could not get gist for: " << text << std::endl;
    return -1;
  }
  
  return ret_value;
}
