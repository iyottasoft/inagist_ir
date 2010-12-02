#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "trends_manager.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480

static int my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char *buf) {
  ERL_NIF_TERM cell, head, tail;
  int val;

  while (enif_get_list_cell(env, list, &head, &tail)) {
    if (!enif_get_int(env, head, &val)) {
      return -1;
    }
    *buf = (char)val;
    buf++;
    list = tail; 
  }
  *buf = '\0';

  return 0;
}

ERL_NIF_TERM nif_getkeywords(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1)
    return enif_make_atom(env, "error");

  ErlNifBinary tweet;
  char tweet_str[MAX_BUFFER_LEN];
 
  bool success = enif_inspect_binary(env, argv[0], &tweet);
  int tweet_len = tweet.size;
  if (success && tweet_len > 1 && tweet_len < MAX_BUFFER_LEN) {
    memcpy(tweet_str, tweet.data, tweet_len);
    tweet_str[tweet_len] = '\0';
    enif_release_binary(env, &tweet);
  } else {
    enif_release_binary(env, &tweet);
    return enif_make_atom(env, "error");
  }

  char safe_status[10];
  memset(safe_status, 0, 10);
  char script[4];
  memset(script, 0, 4);
  char keywords[MAX_BUFFER_LEN];
  keywords[0] = '\0';
  int keywords_len = 0;
  int keywords_count = 0;
  char keyphrases[MAX_BUFFER_LEN];
  keyphrases[0] = '\0';
  int keyphrases_len = 0;
  int keyphrases_count = 0;

  int ret_value = 0;
  if ((ret_value = SubmitTweet((const char *) tweet_str, tweet_len,
                  (char *) safe_status, 10,
                  (char *) script, 4,
                  (char *) keywords, MAX_BUFFER_LEN,
                  &keywords_len, &keywords_count,
                  (char *) keyphrases, MAX_BUFFER_LEN,
                  &keyphrases_len, &keyphrases_count)) < 0) {
    return enif_make_atom(env, "error");
  }
  
  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary safe_status_bin;
  ErlNifBinary script_bin;
  ErlNifBinary keywords_bin;
  ErlNifBinary keyphrases_bin;
  ERL_NIF_TERM safe_status_term; 
  ERL_NIF_TERM lang_term; 

  len = strlen(safe_status);
  if (len < 4 || len > 6) {
    strcpy(safe_status, "error");
    len = 5;
  }
  ret_val = enif_alloc_binary(env, len, &safe_status_bin);
  if (ret_val < 0)
    return enif_make_atom(env, "error");
  for (i=0; i<len; i++) {
    safe_status_bin.data[i] = *(safe_status + i);
  }
  safe_status_term = enif_make_binary(env, &safe_status_bin);

  len = strlen(script);
  if (len != 2 && len != 3) {
    strcpy(script, "00");
    len = 2;
  }
  ret_val = enif_alloc_binary(env, len, &script_bin);
  if (ret_val < 0)
    return enif_make_atom(env, "error");
  for (i=0; i<len; i++) {
    script_bin.data[i] = *(script + i);
  }
  lang_term = enif_make_binary(env, &script_bin);

  char *start = keywords;
  char *end = strstr(start, "|");

  ERL_NIF_TERM keywords_list = enif_make_list(env, 0);
  if (keywords_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;

      ret_val = enif_alloc_binary(env, len, &keywords_bin);
      if (ret_val < 0)
        return enif_make_atom(env, "error");
      for (i=0; i<len; i++) {
        keywords_bin.data[i] = *(start + i);
      }
      keywords_list = enif_make_list_cell(env, enif_make_binary(env, &keywords_bin), keywords_list);

      *end = '|';
      start = end + 1;
    }
  }

  start = keyphrases;
  end = strstr(start, "|");
  len = 0;

  ERL_NIF_TERM keyphrases_list = enif_make_list(env, 0);
  if (keyphrases_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;

      ret_val = enif_alloc_binary(env, len, &keyphrases_bin);
      if (ret_val < 0)
        return enif_make_atom(env, "error");
      for (i=0; i<len; i++) {
        keyphrases_bin.data[i] = *(start + i);
      }
      keyphrases_list = enif_make_list_cell(env, enif_make_binary(env, &keyphrases_bin), keyphrases_list);

      *end = '|';
      start = end + 1;
    }
  }

  return enif_make_tuple4(env, safe_status_term, lang_term, keywords_list, keyphrases_list);
}

ERL_NIF_TERM nif_gettrends(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary user_name;
  char user_name_str[MAX_NAME_LEN];
  memset(user_name_str, 0, MAX_NAME_LEN);

  if (enif_inspect_binary(env, argv[0], &user_name)) {
    memcpy((char *)user_name_str, user_name.data, user_name.size);
    enif_release_binary(env, &user_name);
  } else {
    return enif_make_atom(env, "error");
  }

  char trends_str[MAX_BUFFER_LEN];
  trends_str[0] = '\0';

  if (GetTrends((const char *) user_name_str, (char *) trends_str) < 0) {
    return enif_make_atom(env, "error");
  }

  return enif_make_atom(env, trends_str);
}

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if (argc != 3)
    return enif_make_atom(env, "error");

  ErlNifBinary file_path;
  char stopwords_file_path[MAX_NAME_LEN];

  bool success = enif_inspect_binary(env, argv[0], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(stopwords_file_path, file_path.data, file_path.size);
    stopwords_file_path[file_path.size] = '\0';
    enif_release_binary(env, &file_path);
  } else {
    enif_release_binary(env, &file_path);
    return enif_make_atom(env, "error");
  }

  char dictionary_file_path[MAX_NAME_LEN];

  success = enif_inspect_binary(env, argv[1], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(dictionary_file_path, file_path.data, file_path.size);
    dictionary_file_path[file_path.size] = '\0';
    enif_release_binary(env, &file_path);
  } else {
    enif_release_binary(env, &file_path);
    return enif_make_atom(env, "error");
  }

  char unsafe_dictionary_file_path[MAX_NAME_LEN];

  success = enif_inspect_binary(env, argv[2], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(unsafe_dictionary_file_path, file_path.data, file_path.size);
    unsafe_dictionary_file_path[file_path.size] = '\0';
    enif_release_binary(env, &file_path);
  } else {
    enif_release_binary(env, &file_path);
    return enif_make_atom(env, "error");
  }

  if (Init(stopwords_file_path, dictionary_file_path, unsafe_dictionary_file_path) < 0)
    return enif_make_atom(env, "error");

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_test_twitter_timeline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  char tweets_buffer[MAX_LIST_BUFFER_LEN];
  memset(tweets_buffer, 0, MAX_LIST_BUFFER_LEN);

  int out_length = 0;
  if (argc == 1) {
    char user_name_str[MAX_NAME_LEN];
    memset(user_name_str, 0, MAX_NAME_LEN);
    ErlNifBinary user_name;

    if (enif_inspect_binary(env, argv[0], &user_name)) {
      memcpy(user_name_str, user_name.data, user_name.size);
      enif_release_binary(env, &user_name);
    }   else {
      enif_release_binary(env, &user_name);
      return enif_make_atom(env, "error");
    }

    if (GetTestTweets(user_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
      return enif_make_atom(env, "error");
    }
  } else {
    if (GetTestTweets(NULL, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
      return enif_make_atom(env, "error");
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
    return enif_make_atom(env, "error");
  }

  ErlNifBinary tweet;
  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  int ret_val = 0;
  unsigned int i = 0;
  unsigned int tweet_len = 0;
  ERL_NIF_TERM arg_array[1]; 
  ERL_NIF_TERM tuple4;
  ERL_NIF_TERM tuple4_list = enif_make_list(env, 0);

  while (tweet_start && tweet_end && *tweet_end != '\0') {
    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN)
      return enif_make_atom(env, "error");
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
    if (ret_val < 0)
      return enif_make_atom(env, "error");
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    tuple4 = nif_getkeywords(env, 1, arg_array);
    if (!enif_is_atom(env, tuple4))
      tuple4_list = enif_make_list_cell(env, tuple4, tuple4_list);
    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuple4_list;
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 3, nif_init_c},
  {"getkeywords", 1, nif_getkeywords},
  {"gettrends", 1, nif_gettrends},
  {"test_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_twitter_timeline", 1, nif_test_twitter_timeline},
};
ERL_NIF_INIT(trends, nif_funcs, NULL, NULL, NULL, NULL)
