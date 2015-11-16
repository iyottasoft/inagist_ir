#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include "gist_collector_cppi.h"

#define MAX_BUFFER_LEN 1024
#define MAX_CLASS_NAME 255
#define MAX_THREADS    10001

void *print_message_function( void *ptr );
void *test_gist_collector(void *ptr);

int main(int argc, char* argv[]) {

  if (argc != 2) {
    printf("Usage: %s <num_threads>\n", argv[0]);
    return -1;
  }

  unsigned int num_threads = atoi(argv[1]);

  pthread_t threads[MAX_THREADS];
  //pthread_t thread1, thread2;
  //const char *message1 = "Thread 1";
  //const char *message2 = "Thread 2";
  const char* text = "testing gist_collector Threads #hashtag";

  char test_string[MAX_BUFFER_LEN];
  memset(test_string, '\0', MAX_BUFFER_LEN);

  //int  iret1, iret2;
  int ret_value = 0;
  unsigned int i = 0;

  /* Create independent threads each of which will execute function */

  //iret1 = pthread_create( &thread1, NULL, print_message_function, (void*) message1);
  //iret2 = pthread_create( &thread2, NULL, print_message_function, (void*) message2);
  //iret1 = pthread_create( &thread1, NULL, test_gist_collector, (void*) message1);
  //iret2 = pthread_create( &thread2, NULL, test_gist_collector, (void*) message2);
  for (i=0; i<num_threads; i++) {
    sprintf(test_string, "%s Thread Number %d\n", text, i+1);
    ret_value = pthread_create(&threads[i], NULL, test_gist_collector, (void *) test_string);
    //ret_value = pthread_create(&threads[i], NULL, print_message_function, (void *) text);
  }

  /* Wait till threads are complete before main continues. Unless we  */
  /* wait we run the risk of executing an exit which will terminate   */
  /* the process and all threads before the threads have completed.   */

  //pthread_join( thread1, NULL);
  //pthread_join( thread2, NULL); 
  for (i=0; i<num_threads; i++) {
    pthread_join(threads[i], NULL);
  }

  //printf("Thread 1 returns: %d\n",iret1);
  //printf("Thread 2 returns: %d\n",iret2);
  //exit(0);

  return 0;
}

void *print_message_function( void *ptr )
{
  char *message;
  message = (char *) ptr;
  printf("%s \n", message);
  return (void *)0;
}

void *test_gist_collector(void *ptr) {

  unsigned char tweet_str[MAX_BUFFER_LEN];
  unsigned int tweet_buffer_len = MAX_BUFFER_LEN;
  memset((char *) tweet_str, '\0', MAX_BUFFER_LEN);

  strcpy((char *) tweet_str, (char *) ptr);
  unsigned int tweet_len = strlen((char *) tweet_str);

  char safe_status_buffer[10];
  memset(safe_status_buffer, '\0', 10);
  unsigned int safe_status_buffer_len = 10;

  char script_buffer[4];
  memset(script_buffer, '\0', 4);
  unsigned int script_buffer_len = 4;

#ifdef LANG_ENABLED
  char lang_buffer[MAX_BUFFER_LEN];
  lang_buffer[0] = '\0';
#endif // LANG_ENABLED

#ifdef NAMED_ENTITIES_ENABLED
  unsigned char named_entities[MAX_BUFFER_LEN];
  named_entities[0] = '\0';
  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;
#endif // NAMED_ENTITIES_ENABLED

#ifdef KEYWORDS_ENABLED
  unsigned char keywords[MAX_BUFFER_LEN];
  keywords[0] = '\0';
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
#endif // KEYWORDS_ENABLED

#ifdef KEYPHRASE_ENABLED
  unsigned char keyphrases[MAX_BUFFER_LEN];
  keyphrases[0] = '\0';
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
#endif // KEYPHRASE_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  char text_classes_buffer[MAX_BUFFER_LEN];
  text_classes_buffer[0] = '\0';
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef INTENT_ENABLED
  int intent_valence = 0;
#endif // INTENT_ENABLED

#ifdef SENTIMENT_ENABLED
  int sentiment_valence = 0;
#endif // SENTIMENT_ENABLED

  int ret_value = 0;

  if ((ret_value = CallGetGist((unsigned char *) tweet_str, tweet_buffer_len, tweet_len,
                  (char *) safe_status_buffer, safe_status_buffer_len,
                  (char *) script_buffer, script_buffer_len
#ifdef LANG_ENABLED
                  , (char *) lang_buffer, MAX_BUFFER_LEN
#endif // LANG_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
                  , (unsigned char *) named_entities, MAX_BUFFER_LEN,
                  &named_entities_len, &named_entities_count
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                  , (unsigned char *) keywords, MAX_BUFFER_LEN,
                  &keywords_len, &keywords_count
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                  , (unsigned char *) keyphrases, MAX_BUFFER_LEN,
                  &keyphrases_len, &keyphrases_count
#endif // KEYPHRASE_ENABLED
                  //, (unsigned char *) text_class_words_buffer, MAX_BUFFER_LEN,
                  //&text_class_words_len, &text_class_words_count,
#ifdef TEXT_CLASSIFICATION_ENABLED
                  , (char *) text_classes_buffer, MAX_BUFFER_LEN,
                  &text_classes_len, &text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                  , &intent_valence
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                  , &sentiment_valence
#endif // SENTIMENT_ENABLED
                 )) < 0) {
    printf("ERROR: could not get gist\n");
  }

  return 0;
}
