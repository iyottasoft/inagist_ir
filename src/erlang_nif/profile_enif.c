#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255

#define ERLANG_R14B02 1

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 3) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary file_path;

  char keytuples_config_file_path[MAX_NAME_LEN];
  bool success = enif_inspect_binary(env, argv[0], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(keytuples_config_file_path, file_path.data, file_path.size);
    keytuples_config_file_path[file_path.size] = '\0';
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
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_keytuples_config_file_path_inspect_bin");
#endif
  }

  char language_detector_config_file_path[MAX_NAME_LEN];
  success = enif_inspect_binary(env, argv[1], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(language_detector_config_file_path, file_path.data, file_path.size);
    language_detector_config_file_path[file_path.size] = '\0';
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
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_language_detector_config_file_path_inspect_bin");
#endif
  }

  char text_classifier_config_file_path[MAX_NAME_LEN];
  success = enif_inspect_binary(env, argv[2], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(text_classifier_config_file_path, file_path.data, file_path.size);
    text_classifier_config_file_path[file_path.size] = '\0';
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
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_unsafe_dict_file_path_inspect_bin");
#endif
  }

  if (InitProfiler(keytuples_config_file_path,
                   text_classifier_config_file_path,
                   language_detector_config_file_path) < 0) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_init_trends_manager");
#endif
  }

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_profile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc < 1 || argc > 2) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary twitter_handle_bin;
  char twitter_handle_str[MAX_NAME_LEN];
  memset(twitter_handle_str, 0, MAX_NAME_LEN);

  bool success = enif_inspect_binary(env, argv[0], &twitter_handle_bin);
  int twitter_handle_len = twitter_handle_bin.size;
  if (success && twitter_handle_len > 1 && twitter_handle_len < MAX_NAME_LEN) {
    memcpy(twitter_handle_str, twitter_handle_bin.data, twitter_handle_len);
    twitter_handle_str[twitter_handle_len] = '\0';
#ifdef ERLANG_R14B02 
    enif_release_binary(&twitter_handle_bin);
#else
    enif_release_binary(env, &twitter_handle_bin);
#endif
  } else {
#ifdef ERLANG_R14B02 
    enif_release_binary(&twitter_handle_bin);
#else
    enif_release_binary(env, &twitter_handle_bin);
#endif
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_twitter_handle_len");
#endif
  }

  char class_names[MAX_BUFFER_LEN];
  class_names[0] = '\0';
  int class_names_len = 0;
  int class_names_count = 0;

  int ret_value = 0;
  if ((ret_value = Profile((const char *) twitter_handle_str,
                           twitter_handle_len,
                           (char *) class_names,
                           &class_names_len,
                           &class_names_count)) < 0) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_profile_failed");
#endif
  }

  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary class_names_bin;

  char *start = class_names;
  char *end = strstr(start, "|");

  ERL_NIF_TERM class_names_list = enif_make_list(env, 0);
  if (class_names_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_val = enif_alloc_binary(len, &class_names_bin);
#else
      ret_val = enif_alloc_binary(env, len, &class_names_bin);
#endif
      if (ret_val < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_class_names_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        class_names_bin.data[i] = *(start + i);
      }
      class_names_list = enif_make_list_cell(env, enif_make_binary(env, &class_names_bin), class_names_list);

      *end = '|';
      start = end + 1;
    }
  }

  return enif_make_tuple1(env, class_names_list);

}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 3, nif_init_c},
  {"profile", 1, nif_profile},
  {"profile", 2, nif_profile},
};
ERL_NIF_INIT(profile, nif_funcs, NULL, NULL, NULL, NULL)

