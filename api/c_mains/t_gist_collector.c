#include "gist_collector_cppi.h"
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
  unsigned int tweet_buffer_len = MAX_BUFFER_LEN;
  unsigned int tweet_len = 0;
  strcpy((char *) tweet_str, "This is a Test Tweet ariyumo?");
  tweet_len = strlen((char *) tweet_str);

  char safe_status_buffer[10];
  memset(safe_status_buffer, '\0', 10);
  unsigned int safe_status_buffer_len = 10;

  char script_buffer[4];
  memset(script_buffer, '\0', 4);
  unsigned int script_buffer_len = 4;

  char lang_buffer[MAX_BUFFER_LEN];
  lang_buffer[0] = '\0';
  unsigned int lang_buffer_len = MAX_BUFFER_LEN;

  unsigned char named_entities_buffer[MAX_BUFFER_LEN];
  named_entities_buffer[0] = '\0';
  unsigned int named_entities_buffer_len = MAX_BUFFER_LEN;
  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;

  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  keywords_buffer[0] = '\0';
  unsigned int keywords_buffer_len = MAX_BUFFER_LEN;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;

  unsigned char keyphrases_buffer[MAX_BUFFER_LEN];
  keyphrases_buffer[0] = '\0';
  unsigned int keyphrases_buffer_len = MAX_BUFFER_LEN;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;

  char text_classes_buffer[MAX_BUFFER_LEN];
  text_classes_buffer[0] = '\0';
  unsigned int text_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;

  char intent_buffer[MAX_CLASS_NAME];
  intent_buffer[0] = '\0';
  unsigned int intent_buffer_len = MAX_CLASS_NAME;

  char sentiment_buffer[MAX_CLASS_NAME];
  sentiment_buffer[0] = '\0';
  unsigned int sentiment_buffer_len = MAX_CLASS_NAME;

  int ret_value = 0;
  if ((ret_value = CallGetGist((unsigned char *) tweet_str, tweet_buffer_len, tweet_len,
                  (char *) safe_status_buffer, safe_status_buffer_len,
                  (char *) script_buffer, script_buffer_len
                  , (char *) lang_buffer, lang_buffer_len 
                  , (unsigned char *) named_entities_buffer, named_entities_buffer_len,
                  &named_entities_len, &named_entities_count
                  , (unsigned char *) keywords_buffer, keywords_buffer_len,
                  &keywords_len, &keywords_count
                  , (unsigned char *) keyphrases_buffer, keyphrases_buffer_len,
                  &keyphrases_len, &keyphrases_count,
                  (char *) text_classes_buffer, text_classes_buffer_len,
                  &text_classes_len, &text_classes_count
                  , (char *) intent_buffer, intent_buffer_len 
                  , (char *) sentiment_buffer, sentiment_buffer_len
                 )) < 0) {
    printf("error\n");
  }
  tweet_str[0] = '\0';
  tweet_len = 0;

  printf("intent (.c): %s\n", intent_buffer);
  printf("sentiment (.c): %s\n", sentiment_buffer);

  return 0;
}
