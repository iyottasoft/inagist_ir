#ifdef _CPLUSPLUS
#include <cstring>
#else
#include <string.h>
#endif

#include "gist_maker_eapi.h"
#include "gist_maker.h"

inagist::GistMaker g_gist_maker;

#define MAX_BUFFER_LEN 1024
//#define GIST_DEBUG 1

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitGistMaker(const char* keytuples_extracter_config_file,
                    const char* language_detection_config_file,
                    const char* text_classification_config_file) {

  if (g_gist_maker.Init(keytuples_extracter_config_file,
                language_detection_config_file,
                text_classification_config_file) < 0) {
    std::cerr << "ERROR: could not initialize gist maker\n";
    return -1;
  }

  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int InaGist(unsigned char* text_buffer, const unsigned int text_len,
            char* safe_status_buffer, const unsigned int safe_status_buffer_len,
            char* script_buffer, const unsigned int script_buffer_len
            , char* lang_class_buffer, const unsigned int lang_class_buffer_len
            , unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
            unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr
            , unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
            unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr
            , unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
            unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr
            , char* top_text_classes_buffer, const unsigned int top_text_classes_buffer_len,
            unsigned int* top_text_classes_len_ptr, unsigned int* top_text_classes_count_ptr
            , char* intent_buffer, const unsigned int intent_buffer_len
            , char* sentiment_buffer, const unsigned int sentiment_buffer_len
           ) {

  unsigned char text_class_words_buffer[MAX_BUFFER_LEN];
  memset(text_class_words_buffer, 0, MAX_BUFFER_LEN);
  unsigned int text_class_words_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_class_words_len = 0;
  unsigned int text_class_words_count = 0;

  char text_class_buffer[MAX_BUFFER_LEN];
  memset(text_class_buffer, 0, MAX_BUFFER_LEN);
  unsigned int text_class_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_class_len = 0;
  unsigned int text_class_count = 0;

  unsigned char lang_class_words_buffer[MAX_BUFFER_LEN];
  memset(lang_class_words_buffer, 0, MAX_BUFFER_LEN);
  unsigned int lang_class_words_buffer_len = MAX_BUFFER_LEN;
  unsigned int lang_class_words_len = 0;
  unsigned int lang_class_words_count = 0;

  unsigned int lang_class_len = 0;
  unsigned int lang_class_count = 0;

  char top_lang_classes_buffer[MAX_BUFFER_LEN];
  top_lang_classes_buffer[0] = '\0';
  unsigned int top_lang_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int top_lang_classes_len = 0;
  unsigned int top_lang_classes_count = 0;

  int ret_value = 0;
  if ((ret_value = g_gist_maker.GetGist(text_buffer, text_len,
    safe_status_buffer, safe_status_buffer_len,
    script_buffer, script_buffer_len
    , (unsigned char*) keywords_buffer, keywords_buffer_len,
    keywords_len_ptr, keywords_count_ptr
    , (unsigned char*) hashtags_buffer, hashtags_buffer_len,
    hashtags_len_ptr, hashtags_count_ptr
    , (unsigned char*) keyphrases_buffer, keyphrases_buffer_len,
    keyphrases_len_ptr, keyphrases_count_ptr
    , (unsigned char*) lang_class_words_buffer, lang_class_words_buffer_len,
    &lang_class_words_len, &lang_class_words_count,
    (char *) lang_class_buffer, lang_class_buffer_len,
    &lang_class_len, &lang_class_count,
    (char *) top_lang_classes_buffer, top_lang_classes_buffer_len,
    &top_lang_classes_len, &top_lang_classes_count
    , (unsigned char*) text_class_words_buffer, text_class_words_buffer_len,
    &text_class_words_len, &text_class_words_count,
    (char *) text_class_buffer, text_class_buffer_len,
    &text_class_len, &text_class_count,
    (char *) top_text_classes_buffer, top_text_classes_buffer_len,
    top_text_classes_len_ptr, top_text_classes_count_ptr
    , intent_buffer, intent_buffer_len
    , sentiment_buffer, sentiment_buffer_len
   )) < 0) {
    std::cerr << "ERROR: could not get gist for: " << text_buffer << std::endl;
    return -1;
  }

  lang_class_words_buffer[0] = '\0';
  top_lang_classes_buffer[0] = '\0';
  text_class_words_buffer[0] = '\0';
  text_class_buffer[0] = '\0';

  return ret_value;
}
