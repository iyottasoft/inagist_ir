#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "twitter_api_cppi.h"
#include "sentiment_analyser_cppi.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480
//#define SENTIMENT_DEBUG 1
#define ERLANG_R14B02 1

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef SENTIMENT_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary file_path;

  char config_file_path[MAX_NAME_LEN];
  bool success = enif_inspect_binary(env, argv[0], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(config_file_path, file_path.data, file_path.size);
    config_file_path[file_path.size] = '\0';
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
#ifndef SENTIMENT_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_config_file_path_inspect_bin");
#endif
  }

  if (InitKeyTuplesExtracter(config_file_path) < 0) {
#ifndef SENTIMENT_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_init_keytuples_extracter");
#endif
  }

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_get_sentiment(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef SENTIMENT_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary text;
  unsigned char text_str[MAX_BUFFER_LEN];
  unsigned int text_buffer_len = MAX_BUFFER_LEN;
  memset(text_str, 0, MAX_BUFFER_LEN);

  bool success = enif_inspect_binary(env, argv[0], &text);
  int text_len = text.size;
  if (success && text_len > 1 && text_len < MAX_BUFFER_LEN) {
    memcpy(text_str, text.data, text_len);
    text_str[text_len] = '\0';
#ifdef ERLANG_R14B02 
    enif_release_binary(&text);
#else
    enif_release_binary(env, &text);
#endif
  } else {
#ifdef ERLANG_R14B02 
    enif_release_binary(&text);
#else
    enif_release_binary(env, &text);
#endif
#ifndef SENTIMENT_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_tweet_len");
#endif
  }

  char safe_status[10];
  memset(safe_status, 0, 10);
  char script[4];
  memset(script, 0, 4);
  char sentiment[MAX_BUFFER_LEN];
  sentiment[0] = '\0';
  int sentiment_len = 0;
  int sentiment_count = 0;

  int ret_value = 0;
  if ((ret_value = GetSentiment((unsigned char *) text_str, text_buffer_len, text_len,
                  (char *) safe_status, 10,
                  (char *) script, 4,
                  (char *) sentiment, MAX_BUFFER_LEN,
                  &sentiment_len, &sentiment_count)) < 0) {
#ifndef SENTIMENT_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_GetSentiment");
#endif
  }

  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary safe_status_bin;
  ErlNifBinary script_bin;
  ErlNifBinary sentiment_bin;
  ERL_NIF_TERM safe_status_term; 
  ERL_NIF_TERM script_term; 

  len = strlen(safe_status);
  if (len < 4 || len > 6) {
    strcpy(safe_status, "ERROR");
    len = 5;
  }
#ifdef ERLANG_R14B02 
  ret_val = enif_alloc_binary(len, &safe_status_bin);
#else
  ret_val = enif_alloc_binary(env, len, &safe_status_bin);
#endif
  if (ret_val < 0) {
#ifndef SENTIMENT_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_safe_status_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    safe_status_bin.data[i] = *(safe_status + i);
  }
  safe_status_term = enif_make_binary(env, &safe_status_bin);

  len = strlen(script);
  if (len != 2 && len != 3) {
    strcpy(script, "00");
    len = 2;
  }
#ifdef ERLANG_R14B02 
  ret_val = enif_alloc_binary(len, &script_bin);
#else
  ret_val = enif_alloc_binary(env, len, &script_bin);
#endif
  if (ret_val < 0) {
#ifndef SENTIMENT_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_script_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    script_bin.data[i] = *(script + i);
  }
  script_term = enif_make_binary(env, &script_bin);

  char *start = sentiment;
  char *end = strstr(start, "|");

  ERL_NIF_TERM sentiment_list = enif_make_list(env, 0);
  if (sentiment_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_val = enif_alloc_binary(len, &sentiment_bin);
#else
      ret_val = enif_alloc_binary(env, len, &sentiment_bin);
#endif
      if (ret_val < 0) {
#ifndef SENTIMENT_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_sentiment_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        sentiment_bin.data[i] = *(start + i);
      }
      sentiment_list = enif_make_list_cell(env, enif_make_binary(env, &sentiment_bin), sentiment_list);

      *end = '|';
      start = end + 1;
    }
  }

  return enif_make_tuple3(env, safe_status_term, script_term, sentiment_list);

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
    }   else {
#ifdef ERLANG_R14B02 
      enif_release_binary(&user_name);
#else
      enif_release_binary(env, &user_name);
#endif
#ifndef SENTIMENT_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_user_name");
#endif
    }

    if (GetTestTweets(user_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef SENTIMENT_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  } else {
    if (GetTestTweets(NULL, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef SENTIMENT_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_null_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef SENTIMENT_DEBUG
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
  ERL_NIF_TERM tuple6;
  ERL_NIF_TERM tuple6_list = enif_make_list(env, 0);
  unsigned int error_count = 0;

  while (tweet_start && tweet_end && *tweet_end != '\0') {

    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
#ifndef SENTIMENT_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_out_length");
#endif
    }

#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif

    if (ret_val < 0) {
#ifndef SENTIMENT_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_len");
#endif
    }
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    tuple6 = nif_get_sentiment(env, 1, arg_array);
    if (enif_is_atom(env, tuple6)) {
      error_count++;
    } else {
      tuple6_list = enif_make_list_cell(env, tuple6, tuple6_list);
    }
    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuple6_list;
}

ERL_NIF_TERM nif_test_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  char tweets_buffer[MAX_LIST_BUFFER_LEN];
  memset(tweets_buffer, 0, MAX_LIST_BUFFER_LEN);

  int out_length = 0;
  if (argc != 1) {
#ifndef SENTIMENT_DEBUG
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
    }   else {
#ifdef ERLANG_R14B02 
      enif_release_binary(&file_name);
#else
      enif_release_binary(env, &file_name);
#endif
#ifndef SENTIMENT_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_file_name");
#endif
    }

    if (GetTestTweetsFromFile(file_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef SENTIMENT_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef SENTIMENT_DEBUG
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
  ERL_NIF_TERM tuple6;
  ERL_NIF_TERM tuple6_list = enif_make_list(env, 0);
  unsigned int error_count = 0;

  while (tweet_start && tweet_end && *tweet_end != '\0') {

    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
#ifndef SENTIMENT_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_out_length");
#endif
    }

#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif

    if (ret_val < 0) {
#ifndef SENTIMENT_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_len");
#endif
    }
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    tuple6 = nif_get_sentiment(env, 1, arg_array);
    if (enif_is_atom(env, tuple6)) {
      error_count++;
    } else {
      tuple6_list = enif_make_list_cell(env, tuple6, tuple6_list);
    }
    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuple6_list;
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 1, nif_init_c},
  {"get_sentiment", 1, nif_get_sentiment},
  {"test_sentiment_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_sentiment_twitter_timeline", 1, nif_test_twitter_timeline},
  {"test_sentiment_file", 1, nif_test_from_file},
};
ERL_NIF_INIT(sentiment, nif_funcs, NULL, NULL, NULL, NULL)
