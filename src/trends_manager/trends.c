#include "erl_nif.h"
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "trends_manager.h"

static int my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf) {
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

static ERL_NIF_TERM nif_test(ErlNifEnv *env, ERL_NIF_TERM tweet) {
  char str[1024];
  memset(str, 0, 1024);

  ERL_NIF_TERM head, tail;
  enif_get_list_cell(env, tweet, &head, &tail);

  printf("good");
  if (enif_is_atom(env, tweet))
    printf("atom");
  else if (enif_is_binary(env, tweet))
    printf("binary");
  else if (enif_is_ref(env, tweet))
    printf("ref");
//  else if (enif_is_tuple(env, tweet))
//    printf("tuple");
//  else if (enif_is_list(env, tweet))
//    printf("list");
  else
    printf("unknown");

  //if (enif_get_string(env, tweet, str, 1024, ERL_NIF_UTF8) < 1) {
  if (my_enif_get_string(env, tweet, str) < 0) {
    printf("ERROR: could not convert the tweet from erlang term to const char*\n");
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
  }

  return enif_make_string(env, str, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_getkeywords(ErlNifEnv *env, ERL_NIF_TERM user_name, ERL_NIF_TERM tweet) {
  char tweet_str[1024];
  memset(tweet_str, 0, 1024);
  if (my_enif_get_string(env, tweet, tweet_str) < 0) {
    printf("ERROR: could not convert the tweet from erlang term to const char*\n");
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
  }

  char user_name_str[255];
  memset(user_name_str, 0, 255);
  if (my_enif_get_string(env, user_name, user_name_str) < 0) {
    printf("ERROR: could not convert the user_name from erlang term to const char*\n");
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
  }

  if (SubmitTweet((const char*) user_name_str, (const char *) tweet_str) < 0) {
    printf("ERROR: could not submit tweet to keyword extracter\n");
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
  }

  return enif_make_string(env, "Success", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_gettrends(ErlNifEnv *env, ERL_NIF_TERM user_name) {
  char user_name_str[255];
  memset(user_name_str, 0, 255);
  if (my_enif_get_string(env, user_name, user_name_str) < 0) {
    printf("ERROR: could not convert the user_name from erlang term to const char*\n");
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
  }

  char trends_str[1024];
  memset(trends_str, 0, 1024);

  if (GetTrends((const char*) user_name_str, (char *) trends_str) < 0) {
    printf("ERROR: could not submit tweet to keyword extracter\n");
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
  }

  return enif_make_string(env, trends_str, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_submit(ErlNifEnv *env) {
  if (Init() < 0)
    return -1;
  return nif_getkeywords(env, enif_make_string(env, "tantricninja", ERL_NIF_LATIN1), enif_make_string(env, "Contigous Caps will be counted. So will be contiguous nonstopwords", ERL_NIF_LATIN1));
}

static ERL_NIF_TERM nif_trends(ErlNifEnv *env) {
  if (Init() < 0)
    return -1;
  return nif_gettrends(env, enif_make_string(env, "tantricninja", ERL_NIF_LATIN1));
}

static ErlNifFunc nif_funcs[] =
{
  {"submit", 0, nif_submit},
  {"trends", 0, nif_trends},
  {"getkeywords", 2, nif_getkeywords},
  {"gettrends", 1, nif_trends},
  {"test", 1, nif_test},
};
ERL_NIF_INIT(trends, nif_funcs, NULL, NULL, NULL, NULL)

