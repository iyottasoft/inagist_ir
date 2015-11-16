#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "twitter_api_cppi.h"
#include "trends_manager_cppi.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480
//#define TRENDS_DEBUG 1
#define ERLANG_R14B02 1

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  return enif_make_atom(env, "ok");
}

// given a list of docs, this will return a list of trends
ERL_NIF_TERM nif_get_trends(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  ErlNifBinary docs_bin;
  unsigned char docs_buffer[MAX_LIST_BUFFER_LEN];
  unsigned int docs_len = 0;
  unsigned int docs_count = 0;

  if (enif_inspect_binary(env, argv[0], &docs_bin)) {
    memcpy((char *)docs_buffer, docs_bin.data, docs_bin.size);
    docs_buffer[docs_bin.size] = '\0';
    docs_len = docs_bin.size;
#ifdef ERLANG_R14B02 
    enif_release_binary(&docs_bin);
#else
    enif_release_binary(env, &docs_bin);
#endif
  } else {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_docs_bin");
#endif
  }

  unsigned char trends_buffer[MAX_LIST_BUFFER_LEN];
  unsigned int trends_len = 0;
  unsigned int trends_count = 0;
  if (GetTrends(docs_buffer, docs_len, docs_count,
                trends_buffer, &trends_len, &trends_count) < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_GetTrends");
#endif
  }

  ERL_NIF_TERM trends_list = enif_make_list(env, 0);

  if (strlen(trends_buffer) > 0) {
    char* start = (char*) trends_buffer;
    char* end = strstr(start, "|");
    unsigned int len = 0;
    ErlNifBinary trends_bin;
    int ret_val = 0;
    unsigned int i=0;

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02
      ret_val = enif_alloc_binary(len, &trends_bin);
#else
      ret_val = enif_alloc_binary(env, len, &trends_bin);
#endif
      if (ret_val < 0) {
#ifndef TRENDS_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_trends_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        trends_bin.data[i] = *(start + i);
      }
      trends_list = enif_make_list_cell(env, enif_make_binary(env, &trends_bin), trends_list);

      *end = '|';
      start = end + 1;
    }
  }

  return trends_list;
}

ERL_NIF_TERM nif_process_trends(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  ErlNifBinary trends_bin;
  unsigned char trends_buffer[MAX_LIST_BUFFER_LEN];
  unsigned int trends_len = 0;

  if (enif_inspect_binary(env, argv[0], &trends_bin)) {
    memcpy((char *)trends_buffer, trends_bin.data, trends_bin.size);
    trends_buffer[trends_bin.size] = '\0';
    trends_len = trends_bin.size;
#ifdef ERLANG_R14B02 
    enif_release_binary(&trends_bin);
#else
    enif_release_binary(env, &trends_bin);
#endif
  } else {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_trends_bin");
#endif
  }

  unsigned int trends_count = 0;
  if (ProcessTrends(trends_buffer, &trends_len, &trends_count) < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_GetTrends");
#endif
  }

  ERL_NIF_TERM processed_trends_list = enif_make_list(env, 0);

  if (strlen(trends_buffer) > 0) {
    char* start = (char*) trends_buffer;
    char* end = strstr(start, "|");
    unsigned int len = 0;
    ErlNifBinary processed_trends_bin;
    int ret_val = 0;
    unsigned int i=0;

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02
      ret_val = enif_alloc_binary(len, &processed_trends_bin);
#else
      ret_val = enif_alloc_binary(env, len, &processed_trends_bin);
#endif
      if (ret_val < 0) {
#ifndef TRENDS_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_processed_trends_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        processed_trends_bin.data[i] = *(start + i);
      }
      processed_trends_list = enif_make_list_cell(env,
                                                  enif_make_binary(env, &processed_trends_bin),
                                                  processed_trends_list);

      *end = '|';
      start = end + 1;
    }
  }

  return processed_trends_list;
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
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_user_name");
#endif
    }

    if (GetTestTweets(user_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  } else {
    if (GetTestTweets(NULL, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_null_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_out_length");
#endif
  }

  ErlNifBinary tweets_bin;
  unsigned int tweets_len = out_length;
  int ret_val = 0;
  unsigned int i = 0;
  ERL_NIF_TERM arg_array[1]; 
  ERL_NIF_TERM trends_list = enif_make_list(env, 0);

#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(tweets_len, &tweets_bin);
#else
  ret_val = enif_alloc_binary(env, tweets_len, &tweets_bin);
#endif

  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_tweet_len");
#endif
  }
  for (i=0; i<tweets_len; i++) {
    tweets_bin.data[i] = *(tweets_buffer + i);
  }

  arg_array[0] = enif_make_binary(env, &tweets_bin);
  trends_list = nif_get_trends(env, 1, arg_array);
  if (enif_is_atom(env, trends_list)) {
    return enif_make_atom(env, "error_nif_get_trends");
  }

  return trends_list;
}

ERL_NIF_TERM nif_test_from_file(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary file_path;

  char trends_file_path[MAX_NAME_LEN];
  bool success = enif_inspect_binary(env, argv[0], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(trends_file_path, file_path.data, file_path.size);
    trends_file_path[file_path.size] = '\0';
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
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_trends_file_path_inspect_bin");
#endif
  }

  unsigned char trends_buffer[MAX_LIST_BUFFER_LEN];
  unsigned int trends_len = 0;
  unsigned int trends_count = 0;
  if (GetTestTrendsFromFile(trends_file_path,
                    trends_buffer, MAX_LIST_BUFFER_LEN,
                    &trends_len, &trends_count) < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_get_test_trends_failed");
#endif
  }

  if (trends_len <= 0 || trends_len >= MAX_LIST_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_trends_len");
#endif
  }

  ErlNifBinary trends_bin;
  int ret_val = 0;
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(trends_len, &trends_bin);
#else
  ret_val = enif_alloc_binary(env, trends_len, &trends_bin);
#endif

  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_tweet_len");
#endif
  }
  unsigned int i=0;
  for (i=0; i<trends_len; i++) {
    trends_bin.data[i] = *(trends_buffer + i);
  }

  ERL_NIF_TERM arg_array[1];
  arg_array[0] = enif_make_binary(env, &trends_bin);
  ERL_NIF_TERM trends_list = nif_get_trends(env, 1, arg_array);
  if (enif_is_atom(env, trends_list)) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_get_trends_failed");
#endif
  }

  return trends_list; 
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 0, nif_init_c},
  {"get_trends", 1, nif_get_trends},
  {"process_trends", 1, nif_process_trends},
  {"test_trends_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_trends_file", 1, nif_test_from_file},
};
ERL_NIF_INIT(trends_manager, nif_funcs, NULL, NULL, NULL, NULL)
