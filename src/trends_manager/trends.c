#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "trends_manager.h"

#define NUM_KEYWORDS 100

static int my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char *buf) {
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
  //ErlNifBinary user;
  ErlNifBinary keyword;
  char tweet_str[1024];
  //char user_name_str[255];
  //memset(user_name_str, 0, 255);
  memset(tweet_str, 0, 1024);
  
  //if (enif_inspect_binary(env, argv[0], &user) &&
  if (enif_inspect_binary(env, argv[0], &tweet)) {
    memcpy(tweet_str, tweet.data, tweet.size);
    enif_release_binary(env, &tweet);
    //memcpy(user_name_str, user.data, user.size);
  } else {
    enif_release_binary(env, &tweet);
    return enif_make_atom(env, "error");
  }

  char keywords[1024];
  memset(keywords, 0, 1024);
  if (SubmitTweet(/*(const char *) user_name_str,*/ (const char *) tweet_str, (char *) keywords) < 0) {
    return enif_make_atom(env, "submit_error");
  }

  ERL_NIF_TERM return_list = enif_make_list(env, 0);
  char *start = keywords;
  char *end = strstr(start, ",");
  unsigned int len = 0;
  unsigned int i = 0;
  while (start && end && *end != '\0') {
    end = strstr(start, ",");
    if (!end)
      break;
    *end = '\0';
    len = end - start;

    //return_list = enif_make_list_cell(env, enif_make_string(env, (const char *) start, ERL_NIF_LATIN1), return_list);
    int ret_val = enif_alloc_binary(env, len, &keyword);
    if (ret_val < 0)
      return enif_make_atom(env, "error allocating binary");
    for (i=0; i<len; i++) {
      keyword.data[i] = *(start + i);
    }
    return_list = enif_make_list_cell(env, enif_make_binary(env, &keyword), return_list);

    *end = ',';
    start = end + 1;
  }

  return return_list;
}

<<<<<<< HEAD
ERL_NIF_TERM nif_gettrends(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary user_name;
  char user_name_str[255];
  memset(user_name_str, 0, 255);

  if (enif_inspect_binary(env, argv[0], &user_name)) {
    memcpy((char *)user_name_str, user_name.data, user_name.size);
    enif_release_binary(env, &user_name);
  } else {
    printf("ERROR: could not convert the user_name from erlang binary to const char*\n");
    return enif_make_atom(env, "ERROR");
=======
ERL_NIF_TERM nif_gettrends(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  char user_name_str[255];
  memset(user_name_str, 0, 255);
  if (my_enif_get_string(env, argv[0], user_name_str) < 0) {
    printf("ERROR: could not convert the user_name from erlang term to const char*\n");
    return enif_make_string(env, "ERROR", ERL_NIF_LATIN1);
>>>>>>> efda74285cb0c0258953fbe64fde6f67b684e6fb
  }

  char trends_str[1024];
  memset(trends_str, 0, 1024);

  if (GetTrends((const char *) user_name_str, (char *) trends_str) < 0) {
    printf("ERROR: could not get trends\n");
    return enif_make_atom(env, "ERROR");
  }

  return enif_make_atom(env, trends_str);
}

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary file_path;
  char stopwords_file_path[255];
  memset(stopwords_file_path, 0, 255);

  if (enif_inspect_binary(env, argv[0], &file_path)) {
    memcpy(stopwords_file_path, file_path.data, file_path.size);
    enif_release_binary(env, &file_path);
  } else {
    printf("could not file stopwords file path\n");
    enif_release_binary(env, &file_path);
<<<<<<< HEAD
    return enif_make_atom(env, "ERROR");
  }

  if (Init(stopwords_file_path) < 0)
    return enif_make_atom(env, "ERROR");

  return enif_make_atom(env, "success");
=======
    return enif_make_int(env, -1);
  }

  if (Init(stopwords_file_path) < 0)
    return enif_make_int(env, -1);

  return enif_make_int(env, 0);
>>>>>>> efda74285cb0c0258953fbe64fde6f67b684e6fb
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 1, nif_init_c},
  {"getkeywords", 1, nif_getkeywords},
  {"gettrends", 1, nif_gettrends},
  {"test", 2, nif_test},
};
ERL_NIF_INIT(trends, nif_funcs, NULL, NULL, NULL, NULL)

