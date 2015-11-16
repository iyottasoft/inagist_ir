#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "test_utfcpp.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480
#define ERLANG_R14B02 1

ERL_NIF_TERM nif_test_utfcpp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1)
    return enif_make_atom(env, "error1");

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
    return enif_make_atom(env, "error2");
  }

  char script_buffer[4];
  script_buffer[0] = '\0';
  if (test_detect_script(tweet_str, tweet_len, script_buffer, 4) < 0) { 
    return enif_make_list(env, 0);
  }

  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary script_bin;

  len = strlen(script_buffer);
  if (len != 2 && len != 3) {
    return enif_make_atom(env, tweet_str);
    strcpy(script_buffer, "00");
    len = 2;
  }
#ifdef ERLANG_R14B02 
  ret_val = enif_alloc_binary(len, &script_bin);
#else
  ret_val = enif_alloc_binary(env, len, &script_bin);
#endif
  if (ret_val < 0)
    return enif_make_atom(env, "error4");
  for (i=0; i<len; i++) {
    script_bin.data[i] = *(script_buffer + i);
  }
  ERL_NIF_TERM lang_term = enif_make_binary(env, &script_bin);

  return lang_term;
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
      return enif_make_atom(env, "error5");
    }

    if (GetTestTweets(user_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
      return enif_make_atom(env, "error6");
    }
  } else {
    if (GetTestTweets(NULL, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
      return enif_make_atom(env, "error7");
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
    return enif_make_atom(env, "erro8r");
  }

  ErlNifBinary tweet;
  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  int ret_val = 0;
  unsigned int i = 0;
  unsigned int tweet_len = 0;
  ERL_NIF_TERM arg_array[1]; 
  ERL_NIF_TERM lang_term;
  ERL_NIF_TERM lang_list = enif_make_list(env, 0);

  while (tweet_start && tweet_end && *tweet_end != '\0') {
    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN)
      return enif_make_atom(env, "error9");
#ifdef ERLANG_R14B02 
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif
    if (ret_val < 0)
      return enif_make_atom(env, "error10");
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    lang_term = nif_test_utfcpp(env, 1, arg_array);

    // now insert this into the bigger list
    lang_list = enif_make_list_cell(env, lang_term, lang_list);

    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return lang_list;
}

static ErlNifFunc nif_funcs[] =
{
  {"test_utfcpp", 1, nif_test_utfcpp},
  {"test_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_twitter_timeline", 1, nif_test_twitter_timeline},
};
ERL_NIF_INIT(test_utfcpp, nif_funcs, NULL, NULL, NULL, NULL)
