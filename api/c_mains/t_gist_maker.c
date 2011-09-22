#include "gist_maker_cppi.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define MAX_BUFFER_LEN 1024
#define MAX_CLASS_NAME  255

int main(int argc, char* argv[]) {

  if (InitGistMaker("../../configs/gist_maker.config") < 0) {
    printf("could not initialize gist_maker\n");
    return -1;
  }

  unsigned char tweet_str[MAX_BUFFER_LEN];
  unsigned int tweet_buffer_len = MAX_BUFFER_LEN;
  unsigned int tweet_len = 0;
  memset((char *) tweet_str, '\0', MAX_BUFFER_LEN);
  strcpy((char *) tweet_str, "this is a testing string. Sachin Tendulkar. i need a new phone. Excited!");
  tweet_len = strlen((char *) tweet_str);

#ifdef PROFANITY_CHECK_ENABLED
  char profanity_status_buffer[10];
  memset(profanity_status_buffer, '\0', 10);
  unsigned int profanity_status_buffer_len = 10;
#endif // PROFANITY_CHECK_ENABLED

#ifdef SCRIPT_DETECTION_ENABLED
  char script_buffer[4];
  memset(script_buffer, '\0', 4);
  unsigned int script_buffer_len = 4;
#endif // SCRIPT_DETECTION_ENABLED

#ifdef LANG_ENABLED
  char lang_buffer[MAX_BUFFER_LEN];
  lang_buffer[0] = '\0';
  unsigned int lang_buffer_len = MAX_BUFFER_LEN;
#endif // LANG_ENABLED

#ifdef NAMED_ENTITIES_ENABLED
  unsigned char named_entities_buffer[MAX_BUFFER_LEN];
  named_entities_buffer[0] = '\0';
  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;
  unsigned int named_entities_buffer_len = MAX_BUFFER_LEN;
#endif // NAMED_ENTITIES_ENABLED

#ifdef KEYWORDS_ENABLED
  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  keywords_buffer[0] = '\0';
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned int keywords_buffer_len = MAX_BUFFER_LEN;
#endif // KEYWORDS_ENABLED

#ifdef KEYPHRASE_ENABLED
  unsigned char keyphrases_buffer[MAX_BUFFER_LEN];
  keyphrases_buffer[0] = '\0';
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
  unsigned int keyphrases_buffer_len = MAX_BUFFER_LEN;
#endif // KEYPHRASE_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  char text_classes_buffer[MAX_BUFFER_LEN];
  text_classes_buffer[0] = '\0';
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;
  unsigned int text_classes_buffer_len = MAX_BUFFER_LEN;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LOCATION_ENABLED
  char locations_buffer[MAX_BUFFER_LEN];
  locations_buffer[0] = '\0';
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;
  unsigned int locations_buffer_len = MAX_BUFFER_LEN;
#endif // LOCATION_ENABLED

#ifdef INTENT_ENABLED
  /*
  char intent_buffer[MAX_CLASS_NAME];
  intent_buffer[0] = '\0';
  unsigned int intent_buffer_len = MAX_CLASS_NAME;
  */
  int intent_valence = 0;
#endif // INTENT_ENABLED

#ifdef SENTIMENT_ENABLED
  /*
  char sentiment_buffer[MAX_CLASS_NAME];
  sentiment_buffer[0] = '\0';
  unsigned int sentiment_buffer_len = MAX_CLASS_NAME;
  */
  int sentiment_valence = 0;
#endif // SENTIMENT_ENABLED

  int ret_value = 0;

  if ((ret_value = CallMakeGist((unsigned char *) tweet_str, tweet_buffer_len, tweet_len,
#ifdef PROFANITY_CHECK_ENABLED
                  (char *) profanity_status_buffer, profanity_status_buffer_len,
#endif // PROFANITY_CHECK_ENABLED
#ifdef SCRIPT_DETECTION_ENABLED
                  (char *) script_buffer, script_buffer_len
#endif // SCRIPT_DETECTION_ENABLED
#ifdef LANG_ENABLED
                  , (char *) lang_buffer, lang_buffer_len
#endif // LANG_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
                  , (unsigned char *) named_entities_buffer, named_entities_buffer_len,
                  &named_entities_len, &named_entities_count
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                  , (unsigned char *) keywords_buffer, keywords_buffer_len,
                  &keywords_len, &keywords_count
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                  , (unsigned char *) keyphrases_buffer, keyphrases_buffer_len,
                  &keyphrases_len, &keyphrases_count
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                  , (char *) text_classes_buffer, text_classes_buffer_len,
                  &text_classes_len, &text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                  , (char *) locations_buffer, locations_buffer_len,
                  &locations_len, &locations_count
#endif // LOCATION_ENABLED
#ifdef INTENT_ENABLED
                  //, (char *) intent_buffer, intent_buffer_len
                  , &intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                  //, (char *) sentiment_buffer, sentiment_buffer_len
                  , &sentiment_valence
#endif // SENTIMENT_ENABLED
                 )) < 0) {
    printf("ERROR\n");
    return -1;
  } else {
#ifdef SCRIPT_DETECTION_ENABLED
    printf("script: %s\n", script_buffer);
#endif // SCRIPT_DETECTION_ENABLED
#ifdef KEYPHRASE_ENABLED
    printf("keyphrases: %s\n", keyphrases_buffer);
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    printf("text_classes: %s\n", text_classes_buffer);
#endif // TEXT_CLASSIFICATION_ENABLED
  }

  return 0;

}
