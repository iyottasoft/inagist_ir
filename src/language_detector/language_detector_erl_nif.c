#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "language_detector_erl_interface.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480

ERL_NIF_TERM nif_detect_lang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

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

  char lang_buffer[MAX_BUFFER_LEN];
  lang_buffer[0] = '\0';
  if (SubmitTweet(tweet_str, tweet_len, lang_buffer, MAX_BUFFER_LEN) <= 0) { 
    return enif_make_atom(env, "error");
  }

  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary lang;

  char *start = lang_buffer;
  char *end = strstr(start, "|");

  ERL_NIF_TERM lang_list = enif_make_list(env, 0);
  while (start && end && *end != '\0') {
    end = strstr(start, "|");
    if (!end)
      break;
    *end = '\0';
    len = end - start;

    ret_val = enif_alloc_binary(env, len, &lang);
    if (ret_val < 0)
      return enif_make_atom(env, "error");
    for (i=0; i<len; i++) {
      lang.data[i] = *(start + i);
    }
    lang_list = enif_make_list_cell(env, enif_make_binary(env, &lang), lang_list);

    *end = '|';
    start = end + 1;
  }

  return lang_list;
}

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if (argc != 1)
    return enif_make_atom(env, "error");

  ErlNifBinary file_path;
  char config_file[MAX_NAME_LEN];

  bool success = enif_inspect_binary(env, argv[0], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(config_file, file_path.data, file_path.size);
    config_file[file_path.size] = '\0';
    enif_release_binary(env, &file_path);
  } else {
    enif_release_binary(env, &file_path);
    return enif_make_atom(env, "error");
  }

  if (InitLangD(config_file) < 0)
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
      user_name_str[user_name.size] = '\0';
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
  ERL_NIF_TERM list;
  ERL_NIF_TERM lang_list = enif_make_list(env, 0);
  ERL_NIF_TERM cell, head, tail;

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
    list = nif_detect_lang(env, 1, arg_array);

    // now insert this into the bigger list
    while (enif_get_list_cell(env, list, &head, &tail)) {
      if (enif_is_binary(env, head)) {
        lang_list = enif_make_list_cell(env, head, lang_list);
      }
      list = tail; 
    }

    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return lang_list;
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 1, nif_init_c},
  {"detect_lang", 1, nif_detect_lang},
  {"test_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_twitter_timeline", 1, nif_test_twitter_timeline},
};
ERL_NIF_INIT(langd, nif_funcs, NULL, NULL, NULL, NULL)
