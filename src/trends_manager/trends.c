#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "trends_manager.h"

#define NUM_KEYWORDS 100
#define MAX_BUFFER_LEN 560

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
    return enif_make_string(env, "error", ERL_NIF_LATIN1);
}

ERL_NIF_TERM nif_getkeywords(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary tweet;
  //ErlNifBinary user;
  ErlNifBinary script_bin;
  ErlNifBinary keyword;
  ErlNifBinary keyphrase;
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

  char script[4];
  memset(script, 0, 4);
  char keywords[1024];
  memset(keywords, 0, 1024);
  char keyphrases[1024];
  memset(keyphrases, 0, 1024);
  if (SubmitTweet(/*(const char *) user_name_str,*/ (const char *) tweet_str, (char *) script, (char *) keywords, (char *) keyphrases) < 0) {
    return enif_make_atom(env, "error");
  }

  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
 
  ERL_NIF_TERM lang; 
  len = strlen(script);
  if (len == 2 || len == 3) {
    ret_val = enif_alloc_binary(env, len, &script_bin);
    if (ret_val < 0)
      return enif_make_atom(env, "error");
    for (i=0; i<len; i++) {
      script_bin.data[i] = *(script + i);
    }
    lang = enif_make_binary(env, &script_bin);
  }

  ERL_NIF_TERM keywords_list = enif_make_list(env, 0);
  char *start = keywords;
  char *end = strstr(start, "|");

  while (start && end && *end != '\0') {
    end = strstr(start, "|");
    if (!end)
      break;
    *end = '\0';
    len = end - start;

    ret_val = enif_alloc_binary(env, len, &keyword);
    if (ret_val < 0)
      return enif_make_atom(env, "error");
    for (i=0; i<len; i++) {
      keyword.data[i] = *(start + i);
    }
    keywords_list = enif_make_list_cell(env, enif_make_binary(env, &keyword), keywords_list);

    *end = '|';
    start = end + 1;
  }

  ERL_NIF_TERM keyphrases_list = enif_make_list(env, 0);
  start = keyphrases;
  end = strstr(start, "|");
  len = 0;
  
  while (start && end && *end != '\0') {
    end = strstr(start, "|");
    if (!end)
      break;
    *end = '\0';
    len = end - start;

    ret_val = enif_alloc_binary(env, len, &keyphrase);
    if (ret_val < 0)
      return enif_make_atom(env, "error");
    for (i=0; i<len; i++) {
      keyphrase.data[i] = *(start + i);
    }
    keyphrases_list = enif_make_list_cell(env, enif_make_binary(env, &keyphrase), keyphrases_list);

    *end = '|';
    start = end + 1;
  }

  return enif_make_tuple3(env, lang, keywords_list, keyphrases_list);
}

ERL_NIF_TERM nif_gettrends(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary user_name;
  char user_name_str[255];
  memset(user_name_str, 0, 255);

  if (enif_inspect_binary(env, argv[0], &user_name)) {
    memcpy((char *)user_name_str, user_name.data, user_name.size);
    enif_release_binary(env, &user_name);
  } else {
    return enif_make_atom(env, "error");
  }

  char trends_str[1024];
  memset(trends_str, 0, 1024);

  if (GetTrends((const char *) user_name_str, (char *) trends_str) < 0) {
    return enif_make_atom(env, "error");
  }

  return enif_make_atom(env, trends_str);
}

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  if (argc != 2)
    return enif_make_atom(env, "error");

  ErlNifBinary file_path;
  char stopwords_file_path[255];
  memset(stopwords_file_path, 0, 255);
  char dictionary_file_path[255];
  memset(dictionary_file_path, 0, 255);

  if (enif_inspect_binary(env, argv[0], &file_path)) {
    memcpy(stopwords_file_path, file_path.data, file_path.size);
    enif_release_binary(env, &file_path);
  } else {
    enif_release_binary(env, &file_path);
    return enif_make_atom(env, "error");
  }

  if (enif_inspect_binary(env, argv[1], &file_path)) {
    memcpy(dictionary_file_path, file_path.data, file_path.size);
    enif_release_binary(env, &file_path);
  } else {
    enif_release_binary(env, &file_path);
    return enif_make_atom(env, "error");
  }

  if (Init(stopwords_file_path, dictionary_file_path) < 0)
    return enif_make_atom(env, "error");

  return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 2, nif_init_c},
  {"getkeywords", 1, nif_getkeywords},
  {"gettrends", 1, nif_gettrends},
  {"test", 2, nif_test},
};
ERL_NIF_INIT(trends, nif_funcs, NULL, NULL, NULL, NULL)

