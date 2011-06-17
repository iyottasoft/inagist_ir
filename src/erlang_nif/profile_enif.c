#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "profile.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN    255
#define MAX_LIST_LEN    255
#define MAX_CLASS_NAME  255

#define ERLANG_R14B02 1
#define PROFILE_DEBUG 1

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

  char sentiment_analyser_config_file_path[MAX_NAME_LEN];

  if (InitProfiler(keytuples_config_file_path,
                   language_detector_config_file_path,
                   text_classifier_config_file_path,
                   sentiment_analyser_config_file_path) < 0) {
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
  unsigned char twitter_handle_buffer[MAX_NAME_LEN];
  memset(twitter_handle_buffer, 0, MAX_NAME_LEN);

  bool success = enif_inspect_binary(env, argv[0], &twitter_handle_bin);
  int twitter_handle_len = twitter_handle_bin.size;
  if (success && twitter_handle_len > 1 && twitter_handle_len < MAX_NAME_LEN) {
    memcpy(twitter_handle_buffer, twitter_handle_bin.data, twitter_handle_len);
    twitter_handle_buffer[twitter_handle_len] = '\0';
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

  char safe_status_buffer[10];
  //memset(safe_status_buffer, 0, 10);
  safe_status_buffer[0] = '\0';
  unsigned char locations_buffer[MAX_LIST_LEN];
  //memset(locations_buffer, 0, MAX_LIST_LEN);
  locations_buffer[0] = '\0';
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;
  char languages_buffer[MAX_LIST_LEN];
  //memset(languages_buffer, 0, MAX_LIST_LEN);
  languages_buffer[0] = '\0';
  unsigned int languages_len = 0;
  unsigned int languages_count = 0;
  char text_classes_buffer[MAX_LIST_LEN];
  //memset(text_classes_buffer, 0, MAX_LIST_LEN);
  text_classes_buffer[0] = '\0';
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;
  char sub_classes_buffer[MAX_LIST_LEN];
  //memset(sub_classes_buffer, 0, MAX_LIST_LEN);
  sub_classes_buffer[0] = '\0';
  unsigned int sub_classes_len = 0;
  unsigned int sub_classes_count = 0;
  char text_class_contributors_buffer[MAX_LIST_LEN];
  //memset(text_class_contributors_buffer, 0, MAX_LIST_LEN);
  text_class_contributors_buffer[0] = '\0';
  unsigned int text_class_contributors_len = 0;
  unsigned int text_class_contributors_count = 0;
  char sentiment_buffer[MAX_CLASS_NAME];
  //memset(sentiment_buffer, 0, MAX_CLASS_NAME);
  sentiment_buffer[0] = '\0';
  char* profile_name = NULL;

  int ret_value = 0;
  if ((ret_value = GetProfile(twitter_handle_buffer, twitter_handle_len,
                           locations_buffer, MAX_LIST_LEN,
                           &locations_len, &locations_count,
                           languages_buffer, MAX_LIST_LEN,
                           &languages_len, &languages_count,
                           text_classes_buffer, MAX_LIST_LEN,
                           &text_classes_len, &text_classes_count,
                           sub_classes_buffer, MAX_LIST_LEN,
                           &sub_classes_len, &sub_classes_count,
                           text_class_contributors_buffer, MAX_LIST_LEN,
                           &text_class_contributors_len, &text_class_contributors_count,
                           sentiment_buffer, MAX_CLASS_NAME,
                           profile_name)) < 0) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_profile_failed");
#endif
  }

  unsigned int len = 0;
  unsigned int i = 0;
  char *start = NULL;
  char *end = NULL; 

  ErlNifBinary locations_bin;
  ERL_NIF_TERM locations_list = enif_make_list(env, 0);

  if (locations_count > 0) {

    start = locations_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &locations_bin);
#else
      ret_value = enif_alloc_binary(env, len, &locations_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_locations_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        locations_bin.data[i] = *(start + i);
      }
      locations_list = enif_make_list_cell(env, enif_make_binary(env, &locations_bin), locations_list);

      *end = '|';
      start = end + 1;
    }
  }

  ErlNifBinary languages_bin;
  ERL_NIF_TERM languages_list = enif_make_list(env, 0);

  if (languages_count > 0) {

    start = languages_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &languages_bin);
#else
      ret_value = enif_alloc_binary(env, len, &languages_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_languages_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        languages_bin.data[i] = *(start + i);
      }
      languages_list = enif_make_list_cell(env, enif_make_binary(env, &languages_bin), languages_list);

      *end = '|';
      start = end + 1;
    }
  }

  // text class

  ErlNifBinary text_classes_bin;
  ERL_NIF_TERM text_classes_list = enif_make_list(env, 0);

  if (text_classes_count > 0) {

    start = text_classes_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &text_classes_bin);
#else
      ret_value = enif_alloc_binary(env, len, &text_classes_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_text_classes_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        text_classes_bin.data[i] = *(start + i);
      }
      text_classes_list = enif_make_list_cell(env, enif_make_binary(env, &text_classes_bin), text_classes_list);

      *end = '|';
      start = end + 1;
    }
  }

  // sub class

  ErlNifBinary sub_classes_bin;
  ERL_NIF_TERM sub_classes_list = enif_make_list(env, 0);

  if (sub_classes_count > 0) {

    start = sub_classes_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &sub_classes_bin);
#else
      ret_value = enif_alloc_binary(env, len, &sub_classes_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_sub_classes_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        sub_classes_bin.data[i] = *(start + i);
      }
      sub_classes_list = enif_make_list_cell(env, enif_make_binary(env, &sub_classes_bin), sub_classes_list);

      *end = '|';
      start = end + 1;
    }
  }

  // text class

  ErlNifBinary text_class_contributors_bin;
  ERL_NIF_TERM text_class_contributors_list = enif_make_list(env, 0);

  if (text_class_contributors_count > 0) {

    start = text_class_contributors_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &text_class_contributors_bin);
#else
      ret_value = enif_alloc_binary(env, len, &text_class_contributors_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_text_class_contributors_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        text_class_contributors_bin.data[i] = *(start + i);
      }
      text_class_contributors_list = enif_make_list_cell(env, enif_make_binary(env, &text_class_contributors_bin), text_class_contributors_list);

      *end = '|';
      start = end + 1;
    }
  }

  return enif_make_tuple5(env, locations_list,
                          languages_list,
                          text_classes_list,
                          sub_classes_list,
                          text_class_contributors_list);

}

ERL_NIF_TERM nif_profile_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc < 1 || argc > 2) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary docs_file_name_bin;
  unsigned char docs_file_name_buffer[MAX_NAME_LEN];
  memset(docs_file_name_buffer, 0, MAX_NAME_LEN);

  bool success = enif_inspect_binary(env, argv[0], &docs_file_name_bin);
  int docs_file_name_len = docs_file_name_bin.size;
  if (success && docs_file_name_len > 1 && docs_file_name_len < MAX_NAME_LEN) {
    memcpy(docs_file_name_buffer, docs_file_name_bin.data, docs_file_name_len);
    docs_file_name_buffer[docs_file_name_len] = '\0';
#ifdef ERLANG_R14B02 
    enif_release_binary(&docs_file_name_bin);
#else
    enif_release_binary(env, &docs_file_name_bin);
#endif
  } else {
#ifdef ERLANG_R14B02 
    enif_release_binary(&docs_file_name_bin);
#else
    enif_release_binary(env, &twitter_handle_bin);
#endif
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_twitter_handle_len");
#endif
  }

  char safe_status_buffer[10];
  safe_status_buffer[0] = '\0';
  char languages_buffer[MAX_LIST_LEN];
  languages_buffer[0] = '\0';
  unsigned int languages_len = 0;
  unsigned int languages_count = 0;
  char text_classes_buffer[MAX_LIST_LEN];
  text_classes_buffer[0] = '\0';
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;
  char sub_classes_buffer[MAX_LIST_LEN];
  sub_classes_buffer[0] = '\0';
  unsigned int sub_classes_len = 0;
  unsigned int sub_classes_count = 0;
  char sentiment_buffer[MAX_CLASS_NAME];
  sentiment_buffer[0] = '\0';
  char* profile_name = NULL;

  int ret_value = 0;
  if ((ret_value = GetProfileFromFile(docs_file_name_buffer, docs_file_name_len,
                           languages_buffer, MAX_LIST_LEN,
                           &languages_len, &languages_count,
                           text_classes_buffer, MAX_LIST_LEN,
                           &text_classes_len, &text_classes_count,
                           sub_classes_buffer, MAX_LIST_LEN,
                           &sub_classes_len, &sub_classes_count,
                           sentiment_buffer, MAX_CLASS_NAME,
                           profile_name)) < 0) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_profile_from_file_failed");
#endif
  }

  unsigned int len = 0;
  unsigned int i = 0;

  ErlNifBinary languages_bin;

  char *start = languages_buffer;
  char *end = strstr(start, "|");

  ERL_NIF_TERM languages_list = enif_make_list(env, 0);
  if (languages_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &languages_bin);
#else
      ret_value = enif_alloc_binary(env, len, &languages_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_languages_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        languages_bin.data[i] = *(start + i);
      }
      languages_list = enif_make_list_cell(env, enif_make_binary(env, &languages_bin), languages_list);

      *end = '|';
      start = end + 1;
    }
  }

  // text class

  ErlNifBinary text_classes_bin;

  start = text_classes_buffer;
  end = strstr(start, "|");

  ERL_NIF_TERM text_classes_list = enif_make_list(env, 0);
  if (text_classes_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &text_classes_bin);
#else
      ret_value = enif_alloc_binary(env, len, &text_classes_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_text_classes_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        text_classes_bin.data[i] = *(start + i);
      }
      text_classes_list = enif_make_list_cell(env, enif_make_binary(env, &text_classes_bin), text_classes_list);

      *end = '|';
      start = end + 1;
    }
  }

  // sub class

  ErlNifBinary sub_classes_bin;

  start = sub_classes_buffer;
  end = strstr(start, "|");

  ERL_NIF_TERM sub_classes_list = enif_make_list(env, 0);
  if (sub_classes_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &sub_classes_bin);
#else
      ret_value = enif_alloc_binary(env, len, &sub_classes_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_sub_classes_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        sub_classes_bin.data[i] = *(start + i);
      }
      sub_classes_list = enif_make_list_cell(env, enif_make_binary(env, &sub_classes_bin), sub_classes_list);

      *end = '|';
      start = end + 1;
    }
  }

  return enif_make_tuple3(env, languages_list, text_classes_list, sub_classes_list);

}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 3, nif_init_c},
  {"profile", 1, nif_profile},
  {"profile", 2, nif_profile},
  {"profile_from_file", 1, nif_profile_from_file},
};
ERL_NIF_INIT(profile, nif_funcs, NULL, NULL, NULL, NULL)

