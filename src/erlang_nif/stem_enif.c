#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "tweets.h"
#include "stem.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480
#define ERLANG_R14B02 1

ERL_NIF_TERM nif_stem(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1)
    return enif_make_atom(env, "error");

  ErlNifBinary tweet;
  char tweet_str[MAX_BUFFER_LEN];
 
  bool success = enif_inspect_binary(env, argv[0], &tweet);
  int tweet_len = tweet.size;
  if (success && tweet_len > 1 && tweet_len < MAX_BUFFER_LEN) {
    memcpy(tweet_str, tweet.data, tweet_len);
    tweet_str[tweet_len] = '\0';
#ifdef ERLANG_R14B02
    enif_release_binary(&tweet);
#else
    enif_release_binary(env, &tweet);
#endif
  } else {
#ifdef ERLANG_R14B02 
    enif_release_binary(&tweet);
#else
    enif_release_binary(env, &tweet);
#endif
    return enif_make_atom(env, "error");
  }

  char stems_buffer[MAX_BUFFER_LEN];
  stems_buffer[0] = '\0';
  if (Stem(tweet_str, tweet_len, stems_buffer, MAX_BUFFER_LEN) <= 0) { 
    return enif_make_list(env, 0);
  }

  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary stem;

  char *start = stems_buffer;
  char *end = strstr(start, "|");

  ERL_NIF_TERM stems_list = enif_make_list(env, 0);
  while (start && end && *end != '\0') {
    end = strstr(start, "|");
    if (!end)
      break;
    *end = '\0';
    len = end - start;

#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(len, &stem);
#else
    ret_val = enif_alloc_binary(env, len, &stem);
#endif
    if (ret_val < 0)
      return enif_make_atom(env, "error");
    for (i=0; i<len; i++) {
      stem.data[i] = *(start + i);
    }
    stems_list = enif_make_list_cell(env, enif_make_binary(env, &stem), stems_list);

    *end = '|';
    start = end + 1;
  }

  return stems_list;
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
    return enif_make_atom(env, "error");
  }

  char dictionary_file_path[MAX_NAME_LEN];

  success = enif_inspect_binary(env, argv[1], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(dictionary_file_path, file_path.data, file_path.size);
    dictionary_file_path[file_path.size] = '\0';
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
    return enif_make_atom(env, "error");
  }

  char stemmer_dictionary_file_path[MAX_NAME_LEN];

  success = enif_inspect_binary(env, argv[2], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(stemmer_dictionary_file_path, file_path.data, file_path.size);
    stemmer_dictionary_file_path[file_path.size] = '\0';
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
    return enif_make_atom(env, "error");
  }

  if (InitStemmer(stopwords_file_path, dictionary_file_path, stemmer_dictionary_file_path) < 0)
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
  ERL_NIF_TERM stems_list = enif_make_list(env, 0);
  ERL_NIF_TERM cell, head, tail;

  while (tweet_start && tweet_end && *tweet_end != '\0') {
    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN)
      return enif_make_atom(env, "error");
#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif
    if (ret_val < 0)
      return enif_make_atom(env, "error");
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    list = nif_stem(env, 1, arg_array);

    // now insert this into the bigger list
    while (enif_get_list_cell(env, list, &head, &tail)) {
      if (enif_is_binary(env, head)) {
        stems_list = enif_make_list_cell(env, head, stems_list);
      }
      list = tail; 
    }

    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return stems_list;
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 3, nif_init_c},
  {"stem", 1, nif_stem},
  {"test_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_twitter_timeline", 1, nif_test_twitter_timeline},
};
ERL_NIF_INIT(stem, nif_funcs, NULL, NULL, NULL, NULL)
