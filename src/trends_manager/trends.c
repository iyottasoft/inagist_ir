#include "erl_nif.h"
#include "erl_driver.h"

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

ERL_NIF_TERM nif_test(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary tweet;
  ErlNifBinary user;
  char tweet_str[1024];
  char tweet_user[1024];
  memset(tweet_str, 0, 1024);
  memset(tweet_user, 0, 1024);

  if (enif_inspect_binary(env, argv[0], &tweet) &&
      enif_inspect_binary(env, argv[1], &user)) {
    memcpy(tweet_str, tweet.data, tweet.size);
    memcpy(tweet_user, user.data, user.size);
    return enif_make_tuple2(env, enif_make_string(env, tweet_str, ERL_NIF_LATIN1),
                                 enif_make_string(env, tweet_user, ERL_NIF_LATIN1));
  } else 
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
}

ERL_NIF_TERM nif_getkeywords(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary tweet;
  ErlNifBinary user;
  
  char tweet_str[1024];
  char user_name_str[255];
  memset(user_name_str, 0, 255);
  memset(tweet_str, 0, 1024);
  
  if (enif_inspect_binary(env, argv[0], &user) &&
      enif_inspect_binary(env, argv[1], &tweet)) {
    memcpy(tweet_str, tweet.data, tweet.size);
    memcpy(user_name_str, user.data, user.size);
  } else
    return enif_make_atom(env, "error");


  char keywords[1024];
  memset(keywords, 0, 1024);
  if (SubmitTweet((const char*) user_name_str, (const char *) tweet_str, (char*) keywords) < 0) {
    return enif_make_atom(env, "submit_error");
  }

  ERL_NIF_TERM return_list = enif_make_list(env, 0);
  char *start = keywords;
  char *end = " ";
  while (start && end && *end != '\0') {
    end = strstr(start, ",");
    if (!end)
      break;
    *end = '\0';
    return_list = enif_make_list_cell(env, enif_make_string(env, (const char*) start, ERL_NIF_LATIN1), return_list);
    *end = ',';
    start = end + 1;
  }

  return return_list;
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

int nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  printf("good2");
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
  //{"deinit_c", 0, nif_deinit_c},
  {"getkeywords", 2, nif_getkeywords},
  {"gettrends", 1, nif_trends},
  {"test", 2, nif_test},
};
ERL_NIF_INIT(trends, nif_funcs, NULL, NULL, NULL, NULL)

