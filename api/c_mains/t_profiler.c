#include "profiler_cppi.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define ULTIMATE_BUFFER_LEN 10240
#define MAX_BUFFER_LEN 1024
#define MAX_CLASS_NAME  255

int main(int argc, char* argv[]) {

  if (InitProfiler("../../configs/gist_maker.config") < 0) {
    printf("could not initialize gist_maker\n");
    return -1;
  }

  SetProfilerDebugLevel(5);

  char twitter_handle[MAX_CLASS_NAME];
  memset((char *) twitter_handle, '\0', MAX_CLASS_NAME);
  strcpy((char *) twitter_handle, "balajiworld");
  unsigned int twitter_handle_len = strlen(twitter_handle);

#ifdef LANG_ENABLED
  char self_languages_buffer[MAX_BUFFER_LEN];
  self_languages_buffer[0] = '\0';
  unsigned int self_languages_buffer_len = MAX_BUFFER_LEN;
  unsigned int self_languages_count = 0;
  unsigned int self_languages_len = 0;
  char others_languages_buffer[MAX_BUFFER_LEN];
  others_languages_buffer[0] = '\0';
  unsigned int others_languages_buffer_len = MAX_BUFFER_LEN;
  unsigned int others_languages_count = 0;
  unsigned int others_languages_len = 0;
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  char self_text_classes_buffer[MAX_BUFFER_LEN];
  self_text_classes_buffer[0] = '\0';
  unsigned int self_text_classes_len = 0;
  unsigned int self_text_classes_count = 0;
  unsigned int self_text_classes_buffer_len = MAX_BUFFER_LEN;
  char others_text_classes_buffer[MAX_BUFFER_LEN];
  others_text_classes_buffer[0] = '\0';
  unsigned int others_text_classes_len = 0;
  unsigned int others_text_classes_count = 0;
  unsigned int others_text_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned char self_text_class_contributors_buffer[MAX_BUFFER_LEN];
  self_text_class_contributors_buffer[0] = '\0';
  unsigned int self_text_class_contributors_len = 0;
  unsigned int self_text_class_contributors_count = 0;
  unsigned int self_text_class_contributors_buffer_len = MAX_BUFFER_LEN;
  unsigned char others_text_class_contributors_buffer[MAX_BUFFER_LEN];
  others_text_class_contributors_buffer[0] = '\0';
  unsigned int others_text_class_contributors_len = 0;
  unsigned int others_text_class_contributors_count = 0;
  unsigned int others_text_class_contributors_buffer_len = MAX_BUFFER_LEN;
#endif // TEXT_CLASSIFICATION_ENABLED

  char locations_buffer[MAX_BUFFER_LEN];
  locations_buffer[0] = '\0';
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;
  unsigned int locations_buffer_len = MAX_BUFFER_LEN;

#ifdef LOCATION_ENABLED
  char self_location_classes_buffer[MAX_BUFFER_LEN];
  self_location_classes_buffer[0] = '\0';
  unsigned int self_location_classes_len = 0;
  unsigned int self_location_classes_count = 0;
  unsigned int self_location_classes_buffer_len = MAX_BUFFER_LEN;
  char others_location_classes_buffer[MAX_BUFFER_LEN];
  others_location_classes_buffer[0] = '\0';
  unsigned int others_location_classes_len = 0;
  unsigned int others_location_classes_count = 0;
  unsigned int others_location_classes_buffer_len = MAX_BUFFER_LEN;
#endif // LOCATION_ENABLED

#ifdef RECSYS_ENABLED
 unsigned char recommendations_buffer[ULTIMATE_BUFFER_LEN];
 unsigned int recommendations_buffer_len = ULTIMATE_BUFFER_LEN;
 unsigned int recommendations_count = 0;
 unsigned int recommendations_len = 0;
#endif // RECSYS_ENABLED

  char* profile_name = NULL;

  int ret_value = 0;
  if ((ret_value = GetProfile(twitter_handle, twitter_handle_len,
                              (unsigned char*) locations_buffer, locations_buffer_len,
                              &locations_len, &locations_count
#ifdef LANG_ENABLED
                              , self_languages_buffer, self_languages_buffer_len,
                              &self_languages_len, &self_languages_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                              , self_text_classes_buffer, self_text_classes_buffer_len,
                              &self_text_classes_len, &self_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                              , self_location_classes_buffer, self_location_classes_buffer_len,
                              &self_location_classes_len, &self_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                              , self_text_class_contributors_buffer, self_text_class_contributors_buffer_len,
                              &self_text_class_contributors_len, &self_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
                              , others_languages_buffer, others_languages_buffer_len,
                              &others_languages_len, &others_languages_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                              , others_text_classes_buffer, others_text_classes_buffer_len,
                              &others_text_classes_len, &others_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                              , others_location_classes_buffer, others_location_classes_buffer_len,
                              &others_location_classes_len, &others_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                              , others_text_class_contributors_buffer, others_text_class_contributors_buffer_len,
                              &others_text_class_contributors_len, &others_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef RECSYS_ENABLED
                              , recommendations_buffer, recommendations_buffer_len,
                              &recommendations_len, &recommendations_count
#endif // RECSYS_ENABLED
                              , profile_name)) < 0) {
    printf("ERROR\n");
    return -1;
  }

  locations_buffer[0] = '\0';
#ifdef LANG_ENABLED
  self_languages_buffer[0] = '\0';
  others_languages_buffer[0] = '\0';
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  self_text_classes_buffer[0] = '\0';
  others_text_classes_buffer[0] = '\0';
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef RECSYS_ENABLED
  recommendations_buffer[0] = '\0';
#endif // RECSYS_ENABLED

  return 0;

}
