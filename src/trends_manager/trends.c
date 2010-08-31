#include "erl_nif.h"
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "trends_manager.h"

#define NUM_KEYWORDS 100

ERL_NIF_TERM *keywords_array = NULL;

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

static int nif_getkeywords(ErlNifEnv *env, ERL_NIF_TERM user_name, ERL_NIF_TERM tweet, ERL_NIF_TERM *keywords_array) {
  char tweet_str[1024];
  memset(tweet_str, 0, 1024);
  if (my_enif_get_string(env, tweet, tweet_str) < 0) {
    printf("ERROR: could not convert the tweet from erlang term to const char*\n");
    return -1;
  }

  char user_name_str[255];
  memset(user_name_str, 0, 255);
  if (my_enif_get_string(env, user_name, user_name_str) < 0) {
    printf("ERROR: could not convert the user_name from erlang term to const char*\n");
    return -1;
  }

  char keywords[1024];
  memset(keywords, 0, 1024);
  if (SubmitTweet((const char*) user_name_str, (const char *) tweet_str, (char*) keywords) < 0) {
    printf("ERROR: could not submit tweet to keyword extracter\n");
    return -1;
  }

  char *start = keywords;
  char *end = NULL;
  while (start && end && *end != '\0') {
    end = strstr(start, ",");
    if (!end)
      break;
    *end = '\0';
    enif_make_string(env, (const char*) start, ERL_NIF_LATIN1);
    *end = ',';
    start = end + 1;
  }
  return 0;
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

static int nif_init_c(ErlNifEnv *env) {
  if (Init() < 0)
    return -1;

  if (!keywords_array) {
    keywords_array = (ERL_NIF_TERM *) enif_alloc(env, NUM_KEYWORDS * sizeof(ERL_NIF_TERM));
    if (!keywords_array) {
      printf("ERROR: could not allocate memory\n");
      return -1;
    }
  }

  return 0;
}

static int nif_submit(ErlNifEnv *env) {

  if (!keywords_array)
    return -1;

  if (nif_getkeywords(env,
                      enif_make_string(env, "tantricninja", ERL_NIF_LATIN1),
                      enif_make_string(env, "Contigous Caps will be counted. So will be contiguous nonstopwords", ERL_NIF_LATIN1),
                      keywords_array) < 0) {
    printf("Error\n");
    return -1;
  }

  return 0;
}

static int nif_deinit_c(ErlNifEnv *env) {
  if (keywords_array != NULL)
    enif_free(env, keywords_array);
}

static ERL_NIF_TERM nif_trends(ErlNifEnv *env) {
  if (Init() < 0)
    return -1;
  return nif_gettrends(env, enif_make_string(env, "tantricninja", ERL_NIF_LATIN1));
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 0, nif_init_c},
  {"submit", 0, nif_submit},
  {"trends", 0, nif_trends},
  {"deinit_c", 0, nif_deinit_c},
  {"getkeywords", 3, nif_getkeywords},
  {"gettrends", 1, nif_trends},
  {"test", 1, nif_test},
};
ERL_NIF_INIT(trends, nif_funcs, NULL, NULL, NULL, NULL)

