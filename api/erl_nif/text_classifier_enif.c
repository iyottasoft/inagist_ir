#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "erl_nif.h"
#include "erl_driver.h"

#include "twitter_api_cppi.h"
#include "text_classifier_cppi.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480
//#define LD_DEBUG 1
#define ERLANG_R14B02 1

ERL_NIF_TERM nif_classify_text(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary text_bin;
  char text_str[MAX_BUFFER_LEN];
  ERL_NIF_TERM text_term;
 
  bool success = enif_inspect_binary(env, argv[0], &text_bin);
  int text_len = text_bin.size;
  if (success && text_len > 1 && text_len < MAX_BUFFER_LEN) {
    memcpy(text_str, text_bin.data, text_len);
    text_str[text_len] = '\0';
    text_term = enif_make_binary(env, &text_bin);
    //enif_release_binary(env, &text_bin);
  } else {
#ifdef ERLANG_R14B02
    enif_release_binary(&text_bin);
#else
    enif_release_binary(env, &text_bin);
#endif
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_input_binary");
#endif
  }

  char text_classes_buffer[MAX_BUFFER_LEN];
  text_classes_buffer[0] = '\0';
  unsigned text_classes_len = 0;
  unsigned text_classes_count = 0;
  if (ClassifyText(text_str, text_len,
                   text_classes_buffer, MAX_BUFFER_LEN,
                   &text_classes_len, &text_classes_count) < 0) { 
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_classify_text_failed");
#endif
  }

  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary text_class_bin;

  len = strlen(text_classes_buffer);
  if (len <= 0) {
    strcpy(text_classes_buffer, "00|");
    len = 3;
    text_classes_buffer[3] = '\0';
  }

  char *start = text_classes_buffer;
  char *end = strstr(start, "|");

  ERL_NIF_TERM text_classes_list = enif_make_list(env, 0);
  if (text_classes_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_val = enif_alloc_binary(len, &text_class_bin);
#else
      ret_val = enif_alloc_binary(env, len, &text_class_bin);
#endif
      if (ret_val < 0) {
#ifndef LD_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_text_class_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        text_class_bin.data[i] = *(start + i);
      }
      text_classes_list = enif_make_list_cell(env, enif_make_binary(env, &text_class_bin), text_classes_list);

      *end = '|';
      start = end + 1;
    }
  }

  return enif_make_tuple2(env, text_term, text_classes_list);
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

  if (InitTextClassifier(config_file) < 0) {
#ifndef LD_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_init_text_classifier_failed");
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
    ERL_NIF_TERM tuple2 = nif_classify_text(env, 1, arg_array);
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
      return enif_make_atom(env, "error_undefined_text_classifier_output");
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
    ERL_NIF_TERM tuple2 = nif_classify_text(env, 1, arg_array);
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
  {"classify_text", 1, nif_classify_text},
  {"test_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_twitter_timeline", 1, nif_test_twitter_timeline},
  {"test_from_file", 1, nif_test_from_file},
};
ERL_NIF_INIT(text_classifier, nif_funcs, NULL, NULL, NULL, NULL)
