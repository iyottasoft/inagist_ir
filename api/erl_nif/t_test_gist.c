#include "gist.h"
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define ULTIMATE_BUFFER_LEN 10240
#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_CLASS_NAME 32
#define MAX_LIST_BUFFER_LEN 20480

//#define GIST_DEBUG 1

int main() {

  unsigned char tweet_str[MAX_BUFFER_LEN];
  memset((char *) tweet_str, '\0', MAX_BUFFER_LEN);
  unsigned int tweet_len = 0;
  strcpy(tweet_str, "This is a Test Tweet ariyumo?");
  tweet_len = strlen(tweet_str);

  char safe_status_buffer[10];
  memset(safe_status_buffer, '\0', 10);
  unsigned int safe_status_buffer_len = 10;

  char script_buffer[4];
  memset(script_buffer, '\0', 4);
  unsigned int script_buffer_len = 4;

  char lang[MAX_BUFFER_LEN];
  lang[0] = '\0';

  unsigned char named_entities[MAX_BUFFER_LEN];
  named_entities[0] = '\0';
  int named_entities_len = 0;
  int named_entities_count = 0;

  unsigned char keywords[MAX_BUFFER_LEN];
  keywords[0] = '\0';
  int keywords_len = 0;
  int keywords_count = 0;

  unsigned char keyphrases[MAX_BUFFER_LEN];
  keyphrases[0] = '\0';
  int keyphrases_len = 0;
  int keyphrases_count = 0;

  unsigned char text_class_words_buffer[MAX_BUFFER_LEN];
  text_class_words_buffer[0] = '\0';
  int text_class_words_len = 0;
  int text_class_words_count = 0;

  char text_classes_buffer[MAX_BUFFER_LEN];
  text_classes_buffer[0] = '\0';
  int text_classes_len = 0;
  int text_classes_count = 0;
  unsigned char text_class_contributors_buffer[ULTIMATE_BUFFER_LEN];
  text_class_contributors_buffer[0] = '\0';
  int text_class_contributors_len = 0;
  int text_class_contributors_count = 0;

  char intent_buffer[MAX_CLASS_NAME];
  intent_buffer[0] = '\0';
  unsigned int intent_buffer_len = MAX_CLASS_NAME;

  char sentiment_buffer[MAX_CLASS_NAME];
  sentiment_buffer[0] = '\0';
  unsigned int sentiment_buffer_len = MAX_CLASS_NAME;

  printf("calling Inagist\n");
  int ret_value = 0;
  if ((ret_value = InaGist((const unsigned char *) tweet_str, tweet_len,
                  (char *) safe_status_buffer, safe_status_buffer_len,
                  (char *) script_buffer, script_buffer_len
                  , (char *) lang, MAX_BUFFER_LEN
                  , (unsigned char *) named_entities, MAX_BUFFER_LEN,
                  &named_entities_len, &named_entities_count
                  , (unsigned char *) keywords, MAX_BUFFER_LEN,
                  &keywords_len, &keywords_count
                  , (unsigned char *) keyphrases, MAX_BUFFER_LEN,
                  &keyphrases_len, &keyphrases_count
                  , (unsigned char *) text_class_words_buffer, MAX_BUFFER_LEN,
                  &text_class_words_len, &text_class_words_count,
                  (char *) text_classes_buffer, MAX_BUFFER_LEN,
                  &text_classes_len, &text_classes_count
//                  , (unsigned char *) text_class_contributors_buffer, ULTIMATE_BUFFER_LEN,
//                  &text_class_contributors_len, &text_class_contributors_count
                  , (char *) intent_buffer, intent_buffer_len 
                  , (char *) sentiment_buffer, sentiment_buffer_len
                 )) < 0) {
    printf("error\n");
  }
  tweet_str[0] = '\0';
  tweet_len = 0;

  printf("intent (.c): %s\n", intent_buffer);
  printf("sentiment (.c): %s\n", sentiment_buffer);

  if (named_entities)
    printf("named_entities: %s\n", named_entities);
  else
    printf("invalid named_entities buffer\n");

  if (intent_buffer)
    printf("intent: %s\n", intent_buffer);
  else
    printf("invalid intent_buffer\n");

  return 0;
}
