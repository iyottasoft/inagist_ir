#ifndef _INAGIST_TRENDS_TRENDS_ERL_NIF_H_
#define _INAGIST_TRENDS_TRENDS_ERL_NIF_H_

#include "erl_nif.h"

ERL_NIF_TERM nif_getkeywords(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_gettrends(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM nif_test_twitter_timeline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 2, nif_init_c},
  {"getkeywords", 1, nif_getkeywords},
  {"gettrends", 1, nif_gettrends},
  {"test_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_twitter_timeline", 1, nif_test_twitter_timeline},
};
ERL_NIF_INIT(trends, nif_funcs, NULL, NULL, NULL, NULL)

#endif // _INAGIST_TRENDS_TRENDS_ERL_NIF_H_
