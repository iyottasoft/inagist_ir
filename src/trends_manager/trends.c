#include "erl_nif.h"
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "trends_manager.h"

static int my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf) {
  ERL_NIF_TERM cell, head, tail;
  int val;

  while (enif_get_list_cell(env, list, &head, &tail)) {
  printf("my enif get string\n");
    if (!enif_get_int(env, head, &val)) {
      return -1;
    }
    *buf = (char)val;
    buf++;
    list = tail; 
  }
  printf("1 enif get string\n");
  *buf = '\0';

  return 0;
}

static ERL_NIF_TERM nif_getkeywords(ErlNifEnv *env, ERL_NIF_TERM tweet) {
  char str[1024];
  memset(str, 0, 1024);
  if (my_enif_get_string(env, tweet, str) < 0) {
    printf("ERROR: could not convert the tweet from erlang term to const char*\n");
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
  }

  if (SubmitTweet((const char *)str) < 0) {
    printf("ERROR: could not submit tweet to keyword extracter\n");
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
  }

  return enif_make_string(env, "Success", ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_start(ErlNifEnv *env) {
  if (Init() < 0)
    return -1;
  return nif_getkeywords(env, enif_make_string(env, "Contigous Caps will be counted. So will be contiguous nonstopwords", ERL_NIF_LATIN1));
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

static ErlNifFunc nif_funcs[] =
{
  {"start", 0, nif_start},
  {"getkeywords", 1, nif_getkeywords},
  {"test", 1, nif_test},
};
ERL_NIF_INIT(trends, nif_funcs, NULL, NULL, NULL, NULL)

