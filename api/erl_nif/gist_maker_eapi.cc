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

  if (g_gist_maker.Init(keytuples_extracter_config_file
#ifdef LANG_ENABLED
                        , language_detection_config_file
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                        , text_classification_config_file
#endif // TEXT_CLASSIFICATION_ENABLED
                       ) < 0) {
    std::cerr << "ERROR: could not initialize gist maker\n";
    return -1;
  }

  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int InaGist(unsigned char* text_buffer, const unsigned int text_buffer_len,
            const unsigned int text_len,
            char* safe_status_buffer, const unsigned int safe_status_buffer_len,
            char* script_buffer, const unsigned int script_buffer_len
#ifdef LANG_ENABLED
            , char* lang_class_buffer, const unsigned int lang_class_buffer_len
#endif // LANG_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
            , unsigned char* named_entities_buffer, const unsigned int named_entities_buffer_len,
            unsigned int* named_entities_len_ptr, unsigned int* named_entities_count_ptr
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
            , unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
            unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
            , unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
            unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
            , char* top_text_classes_buffer, const unsigned int top_text_classes_buffer_len,
            unsigned int* top_text_classes_len_ptr, unsigned int* top_text_classes_count_ptr
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
            , char* intent_buffer, const unsigned int intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
            , char* sentiment_buffer, const unsigned int sentiment_buffer_len
#endif // SENTIMENT_ENABLED
           ) {

#ifdef TEXT_CLASSIFICATION_ENABLED
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
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LANG_ENABLED
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
#endif // LANG_ENABLED

  int ret_value = 0;
  if ((ret_value = g_gist_maker.GetGist(text_buffer, text_buffer_len, text_len,
    safe_status_buffer, safe_status_buffer_len,
    script_buffer, script_buffer_len
#ifdef NAMED_ENTITIES_ENABLED
    , (unsigned char*) named_entities_buffer, named_entities_buffer_len,
    named_entities_len_ptr, named_entities_count_ptr
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
    , (unsigned char*) keywords_buffer, keywords_buffer_len,
    keywords_len_ptr, keywords_count_ptr
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
    , (unsigned char*) keyphrases_buffer, keyphrases_buffer_len,
    keyphrases_len_ptr, keyphrases_count_ptr
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
    , (unsigned char*) lang_class_words_buffer, lang_class_words_buffer_len,
    &lang_class_words_len, &lang_class_words_count,
    (char *) lang_class_buffer, lang_class_buffer_len,
    &lang_class_len, &lang_class_count,
    (char *) top_lang_classes_buffer, top_lang_classes_buffer_len,
    &top_lang_classes_len, &top_lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    , (unsigned char*) text_class_words_buffer, text_class_words_buffer_len,
    &text_class_words_len, &text_class_words_count,
    (char *) text_class_buffer, text_class_buffer_len,
    &text_class_len, &text_class_count,
    (char *) top_text_classes_buffer, top_text_classes_buffer_len,
    top_text_classes_len_ptr, top_text_classes_count_ptr
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
    , intent_buffer, intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
    , sentiment_buffer, sentiment_buffer_len
#endif // SENTIMENT_ENABLED
   )) < 0) {
    std::cerr << "ERROR: could not get gist for: " << text_buffer << std::endl;
    return -1;
  }

#ifdef LANG_ENABLED
  lang_class_words_buffer[0] = '\0';
  top_lang_classes_buffer[0] = '\0';
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  text_class_words_buffer[0] = '\0';
  text_class_buffer[0] = '\0';
#endif // TEXT_CLASSIFICATION_ENABLED

  return ret_value;
}
