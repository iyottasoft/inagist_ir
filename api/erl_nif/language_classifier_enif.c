#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "erl_nif.h"
#include "erl_driver.h"

#include "twitter_api_cppi.h"
#include "language_classifier_cppi.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480
//#define LD_DEBUG 1
#define ERLANG_R14B02 1

ERL_NIF_TERM nif_detect_lang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary tweet_bin;
  char tweet_str[MAX_BUFFER_LEN];
  ERL_NIF_TERM tweet_term;
 
  bool success = enif_inspect_binary(env, argv[0], &tweet_bin);
  int tweet_len = tweet_bin.size;
  if (success && tweet_len > 1 && tweet_len < MAX_BUFFER_LEN) {
    memcpy(tweet_str, tweet_bin.data, tweet_len);
    tweet_str[tweet_len] = '\0';
    tweet_term = enif_make_binary(env, &tweet_bin);
    //enif_release_binary(env, &tweet_bin);
  } else {
#ifdef ERLANG_R14B02
    enif_release_binary(&tweet_bin);
#else
    enif_release_binary(env, &tweet_bin);
#endif
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_input_binary");
#endif
  }

  char lang_buffer[MAX_BUFFER_LEN];
  lang_buffer[0] = '\0';
  if (CallClassify(tweet_str, tweet_len, lang_buffer, MAX_BUFFER_LEN) < 0) { 
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_call_classify_failed");
#endif
  }

  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary lang_bin;
  ERL_NIF_TERM lang_term;

  len = strlen(lang_buffer);
  if (len < 2) {
    strcpy(lang_buffer, "00");
    len = 2;
    lang_buffer[2] = '\0';
  }

#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(len, &lang_bin);
#else
  ret_val = enif_alloc_binary(env, len, &lang_bin);
#endif
  if (ret_val < 0) {
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_lang_bin_alloc");
#endif
  }

  for (i=0; i<len; i++) {
    lang_bin.data[i] = *(lang_buffer + i);
  }
  lang_term = enif_make_binary(env, &lang_bin);

  return enif_make_tuple2(env, tweet_term, lang_term);
}

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary file_path;
  char config_file[MAX_NAME_LEN];

  bool success = enif_inspect_binary(env, argv[0], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(config_file, file_path.data, file_path.size);
    config_file[file_path.size] = '\0';
#ifdef ERLANG_R14B02
    enif_release_binary(&file_path);
#else
    enif_release_binary(env, &file_path);
#endif
  } else {
#ifdef ERLANG_R14B02
    enif_release_binary(&file_path);
#else
    enif_release_binary(env, &file_path);
#endif
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_input_binary");
#endif
  }

  if (InitLanguageClassifier(config_file) < 0) {
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_InitLanguageClassifier_failed");
#endif
  }

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
#ifdef ERLANG_R14B02
      enif_release_binary(&user_name);
#else
      enif_release_binary(env, &user_name);
#endif
    } else {
#ifdef ERLANG_R14B02
      enif_release_binary(&user_name);
#else
      enif_release_binary(env, &user_name);
#endif
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_user_name");
#endif
    }

    if (GetTestTweets(user_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  } else {
    if (GetTestTweets(NULL, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_null_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_out_length");
#endif
  }

  ErlNifBinary tweet;
  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  int ret_val = 0;
  unsigned int i = 0;
  unsigned int tweet_len = 0;
  ERL_NIF_TERM arg_array[1]; 
  ERL_NIF_TERM tuple2_list = enif_make_list(env, 0);

  while (tweet_start && tweet_end && *tweet_end != '\0') {
    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_length");
#endif
    }

#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif
    if (ret_val < 0) {
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_alloc_bin_failed_for_tweet");
#endif
    }

    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    ERL_NIF_TERM tuple2 = nif_detect_lang(env, 1, arg_array);
    if (enif_is_atom(env, tuple2)) {
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_not_a_tuple2");
#endif
    //} else if (enif_is_tuple(env, tuple2)) {
    } else {
      tuple2_list = enif_make_list_cell(env, tuple2, tuple2_list);
/*
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_undefined_detect_lang_output");
#endif
*/
    }

    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuple2_list;
}

ERL_NIF_TERM nif_test_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  char tweets_buffer[MAX_LIST_BUFFER_LEN];
  memset(tweets_buffer, 0, MAX_LIST_BUFFER_LEN);

  int out_length = 0;
  if (argc != 1) {
#ifndef LD_DEBUG
     return enif_make_atom(env, "error"); 
#else
     return enif_make_atom(env, "error_invalid_argc"); 
#endif
  } else {
    char file_name_str[MAX_NAME_LEN];
    memset(file_name_str, 0, MAX_NAME_LEN);
    ErlNifBinary file_name;

    if (enif_inspect_binary(env, argv[0], &file_name)) {
      memcpy(file_name_str, file_name.data, file_name.size);
      file_name_str[file_name.size] = '\0';
#ifdef ERLANG_R14B02
      enif_release_binary(&file_name);
#else
      enif_release_binary(env, &file_name);
#endif
    } else {
#ifdef ERLANG_R14B02
      enif_release_binary(&file_name);
#else
      enif_release_binary(env, &file_name);
#endif
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_file_name");
#endif
    }

    if (GetTestTweetsFromFile(file_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_out_length");
#endif
  }

  ErlNifBinary tweet;
  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  int ret_val = 0;
  unsigned int i = 0;
  unsigned int tweet_len = 0;
  ERL_NIF_TERM arg_array[1]; 
  ERL_NIF_TERM tuple2_list = enif_make_list(env, 0);

  while (tweet_start && tweet_end && *tweet_end != '\0') {
    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_length");
#endif
    }

#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif
    if (ret_val < 0) {
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_alloc_bin_failed_for_tweet");
#endif
    }

    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    ERL_NIF_TERM tuple2 = nif_detect_lang(env, 1, arg_array);
    if (enif_is_atom(env, tuple2)) {
#ifndef LD_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_not_a_tuple2");
#endif
    } else {
      tuple2_list = enif_make_list_cell(env, tuple2, tuple2_list);
    }

    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuple2_list;
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 1, nif_init_c},
  {"detect_lang", 1, nif_detect_lang},
  {"test_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_twitter_timeline", 1, nif_test_twitter_timeline},
  {"test_from_file", 1, nif_test_from_file},
};
ERL_NIF_INIT(language_classifier, nif_funcs, NULL, NULL, NULL, NULL)
