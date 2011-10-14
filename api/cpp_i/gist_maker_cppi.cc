#ifdef _CPLUSPLUS
#include <cstring>
#else
#include <string.h>
#endif

#include "gist_maker_cppi.h"
#include "gist_maker.h"

inagist::GistMaker g_gist_maker;

#define MAX_BUFFER_LEN 1024

#ifdef DEBUG
#if DEBUG>0
#define GIST_DEBUG DEBUG
#endif
#endif
//#define GIST_DEBUG 1

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitGistMaker(const char* gist_maker_config_file) {

  if (g_gist_maker.Init(gist_maker_config_file) < 0) {
    std::cerr << "ERROR: could not initialize gist maker\n";
    return -1;
  }

  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int CallMakeGist(unsigned char* text_buffer, const unsigned int text_buffer_len,
                 const unsigned int text_len
#ifdef PROFANITY_CHECK_ENABLED
                 , char* profanity_status_buffer, const unsigned int profanity_status_buffer_len
#endif // PROFANITY_CHECK_ENABLED
#ifdef SCRIPT_DETECTION_ENABLED
                 , char* script_buffer, const unsigned int script_buffer_len
#endif // SCRIPT_DETECTION_ENABLED
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
                 , char* text_classes_buffer, const unsigned int text_classes_buffer_len,
                 unsigned int* text_classes_len_ptr, unsigned int* text_classes_count_ptr
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                 , char* locations_buffer, const unsigned int locations_buffer_len,
                 unsigned int* locations_len_ptr, unsigned int* locations_count_ptr
#endif // LOCATION_ENABLED
#ifdef INTENT_ENABLED
                 , int* intent_valence_ptr
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                 , int* sentiment_valence_ptr
#endif // SENTIMENT_ENABLED
                ) {

#ifdef GIST_DEBUG
  if (!text_buffer || !profanity_status_buffer || !script_buffer || !lang_class_buffer || !named_entities_buffer || !named_entities_len_ptr || !named_entities_count_ptr || !keywords_buffer || !keywords_len_ptr || !keywords_count_ptr || !keyphrases_buffer || !keyphrases_len_ptr || !keyphrases_count_ptr || !text_classes_buffer || !text_classes_len_ptr || !text_classes_count_ptr || !locations_buffer || !locations_len_ptr || !locations_count_ptr || !intent_valence_ptr || !sentiment_valence_ptr) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }
/*
  std::cout << "INFO: initializing output parameters ...\n";
  profanity_status_buffer[0] = '\0';
  script_buffer[0] = '\0';
  named_entities_buffer[0] = '\0';
  *named_entities_len_ptr = 0;
  *named_entities_count_ptr = 0;
  keywords_buffer[0] = '\0';
  *keywords_len_ptr = 0;
  *keywords_count_ptr = 0;
  keyphrases_buffer[0] = '\0';
  *keyphrases_len_ptr = 0;
  *keyphrases_count_ptr = 0;
  text_classes_buffer[0] = '\0';
  *text_classes_len_ptr = 0;
  *text_classes_count_ptr = 0;
  locations_buffer[0] = '\0';
  *locations_len_ptr = 0;
  *locations_count_ptr = 0;
  std::cout << "INFO: output parameters initialized\n";
*/
#endif // GIST_DEBUG

#ifdef NAMED_ENTITIES_ENABLED
  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;
#endif // LOCATION_ENABLED
#ifdef LANG_ENABLED
  unsigned int lang_class_len = 0;
  unsigned int lang_class_count = 0;
#endif // LANG_ENABLED
#ifdef INTENT_ENABLED
  int intent_valence = 0;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  int sentiment_valence = 0;
#endif // SENTIMENT_ENABLED

  int ret_value = 0;
  if ((ret_value = g_gist_maker.MakeGist(text_buffer, text_buffer_len, text_len
#ifdef PROFANITY_CHECK_ENABLED
    , profanity_status_buffer, profanity_status_buffer_len
#endif // PROFANITY_CHECK_ENABLED
#ifdef SCRIPT_DETECTION_ENABLED
    , script_buffer, script_buffer_len
#endif // SCRIPT_DETECTION_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
    , named_entities_buffer, named_entities_buffer_len,
    named_entities_len, named_entities_count
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
    , keywords_buffer, keywords_buffer_len,
    keywords_len, keywords_count
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
    , keyphrases_buffer, keyphrases_buffer_len,
    keyphrases_len, keyphrases_count
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
    , (unsigned char *) lang_class_buffer, lang_class_buffer_len,
    lang_class_len, lang_class_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    , text_classes_buffer, text_classes_buffer_len,
    text_classes_len, text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
    , locations_buffer, locations_buffer_len,
    locations_len, locations_count
#endif // LOCATION_ENABLED
#ifdef INTENT_ENABLED
    , intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
    , sentiment_valence
#endif // SENTIMENT_ENABLED
   )) <= 0) {
#ifdef GIST_DEBUG
    if (ret_value < 0) {
      std::cerr << "ERROR: error calling MakeGist for: " << text_buffer << std::endl;
    }
#endif // GIST_DEBUG
    return ret_value;
  }

#ifdef NAMED_ENTITIES_ENABLED
  *named_entities_len_ptr = named_entities_len;
  *named_entities_count_ptr = named_entities_count;
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
  *keywords_len_ptr = keywords_len;
  *keywords_count_ptr = keywords_count;
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
  *keyphrases_len_ptr = keyphrases_len;
  *keyphrases_count_ptr = keyphrases_count;
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  *text_classes_len_ptr = text_classes_len;
  *text_classes_count_ptr = text_classes_count;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
  *locations_len_ptr = locations_len;
  *locations_count_ptr = locations_count;
#endif // LOCATION_ENABLED
#ifdef INTENT_ENABLED
  *intent_valence_ptr = intent_valence;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  *sentiment_valence_ptr = sentiment_valence;
#endif // SENTIMENT_ENABLED

  return ret_value;
}
