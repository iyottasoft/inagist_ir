#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "tweets.h"
#include "ir_cpp.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480
//#define TRENDS_DEBUG 1
#define ERLANG_R14B02 1

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

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 5) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary file_path;

  char stopwords_file_path[MAX_NAME_LEN];
  bool success = enif_inspect_binary(env, argv[0], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(stopwords_file_path, file_path.data, file_path.size);
    stopwords_file_path[file_path.size] = '\0';
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
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_stopwords_file_path_inspect_bin");
#endif
  }

  char dictionary_file_path[MAX_NAME_LEN];
  success = enif_inspect_binary(env, argv[1], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(dictionary_file_path, file_path.data, file_path.size);
    dictionary_file_path[file_path.size] = '\0';
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
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_dictionary_file_path_inspect_bin");
#endif
  }

  char unsafe_dictionary_file_path[MAX_NAME_LEN];
  success = enif_inspect_binary(env, argv[2], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(unsafe_dictionary_file_path, file_path.data, file_path.size);
    unsafe_dictionary_file_path[file_path.size] = '\0';
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
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_unsafe_dict_file_path_inspect_bin");
#endif
  }

  char lang_detect_config_file_path[MAX_NAME_LEN];
  success = enif_inspect_binary(env, argv[3], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(lang_detect_config_file_path, file_path.data, file_path.size);
    lang_detect_config_file_path[file_path.size] = '\0';
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
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_lang_detect_config_file_path_inspect_bin");
#endif
  }

  char channels_dictionary_file_path[MAX_NAME_LEN];
  success = enif_inspect_binary(env, argv[4], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(channels_dictionary_file_path, file_path.data, file_path.size);
    channels_dictionary_file_path[file_path.size] = '\0';
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
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_channels_dict_file_path_inspect_bin");
#endif
  }

  if (InitTrendsManager(stopwords_file_path,
           dictionary_file_path,
           unsafe_dictionary_file_path,
           lang_detect_config_file_path,
           channels_dictionary_file_path) < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_init_trends_manager");
#endif
  }

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_get_keywords(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary tweet;
  char tweet_str[MAX_BUFFER_LEN];
  memset(tweet_str, 0, MAX_BUFFER_LEN);

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
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_tweet_len");
#endif
  }

  char safe_status[10];
  memset(safe_status, 0, 10);
  char script[4];
  memset(script, 0, 4);
  char keywords[MAX_BUFFER_LEN];
  keywords[0] = '\0';
  int keywords_len = 0;
  int keywords_count = 0;
  char hashtags[MAX_BUFFER_LEN];
  hashtags[0] = '\0';
  int hashtags_len = 0;
  int hashtags_count = 0;
  char keyphrases[MAX_BUFFER_LEN];
  keyphrases[0] = '\0';
  int keyphrases_len = 0;
  int keyphrases_count = 0;
  char buffer1[MAX_BUFFER_LEN];
  buffer1[0] = '\0';
  char buffer2[MAX_BUFFER_LEN];
  buffer2[0] = '\0';
  char buffer3[MAX_BUFFER_LEN];
  buffer3[0] = '\0';
  char buffer4[MAX_BUFFER_LEN];
  buffer4[0] = '\0';

  int ret_value = 0;
  if ((ret_value = GetKeywords((const char *) tweet_str, tweet_len,
                  (char *) safe_status, 10,
                  (char *) script, 4,
                  (char *) keywords, MAX_BUFFER_LEN,
                  &keywords_len, &keywords_count,
                  (char *) hashtags, MAX_BUFFER_LEN,
                  &hashtags_len, &hashtags_count,
                  (char *) keyphrases, MAX_BUFFER_LEN,
                  &keyphrases_len, &keyphrases_count,
                  (char *) buffer1, MAX_BUFFER_LEN,
                  (char *) buffer2, MAX_BUFFER_LEN,
                  (char *) buffer3, MAX_BUFFER_LEN,
                  (char *) buffer4, MAX_BUFFER_LEN)) < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_GetKeywords");
#endif
  }
  
  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary safe_status_bin;
  ErlNifBinary script_bin;
  ErlNifBinary keywords_bin;
  ErlNifBinary hashtags_bin;
  ErlNifBinary keyphrases_bin;
  ErlNifBinary buffer1_bin;
  ErlNifBinary buffer2_bin;
  ErlNifBinary channels_bin;
  ErlNifBinary buffer4_bin;
  ERL_NIF_TERM safe_status_term; 
  ERL_NIF_TERM lang_term; 
  ERL_NIF_TERM buffer1_term; 
  ERL_NIF_TERM buffer2_term; 
  ERL_NIF_TERM buffer4_term; 

  len = strlen(safe_status);
  if (len < 4 || len > 6) {
    strcpy(safe_status, "ERROR");
    len = 5;
  }
#ifdef ERLANG_R14B02 
  ret_val = enif_alloc_binary(len, &safe_status_bin);
#else
  ret_val = enif_alloc_binary(env, len, &safe_status_bin);
#endif
  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_safe_status_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    safe_status_bin.data[i] = *(safe_status + i);
  }
  safe_status_term = enif_make_binary(env, &safe_status_bin);

  len = strlen(script);
  if (len != 2 && len != 3) {
    strcpy(script, "00");
    len = 2;
  }
#ifdef ERLANG_R14B02 
  ret_val = enif_alloc_binary(len, &script_bin);
#else
  ret_val = enif_alloc_binary(env, len, &script_bin);
#endif
  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_script_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    script_bin.data[i] = *(script + i);
  }
  lang_term = enif_make_binary(env, &script_bin);

  char *start = keywords;
  char *end = strstr(start, "|");

  ERL_NIF_TERM keywords_list = enif_make_list(env, 0);
  if (keywords_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_val = enif_alloc_binary(len, &keywords_bin);
#else
      ret_val = enif_alloc_binary(env, len, &keywords_bin);
#endif
      if (ret_val < 0) {
#ifndef TRENDS_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_keywords_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        keywords_bin.data[i] = *(start + i);
      }
      keywords_list = enif_make_list_cell(env, enif_make_binary(env, &keywords_bin), keywords_list);

      *end = '|';
      start = end + 1;
    }
  }

  start = hashtags;
  end = strstr(start, "|");
  len = 0;

  ERL_NIF_TERM hashtags_list = enif_make_list(env, 0);
  if (hashtags_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_val = enif_alloc_binary(len, &hashtags_bin);
#else
      ret_val = enif_alloc_binary(env, len, &hashtags_bin);
#endif
      if (ret_val < 0) {
#ifndef TRENDS_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_hashtags_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        hashtags_bin.data[i] = *(start + i);
      }
      hashtags_list = enif_make_list_cell(env, enif_make_binary(env, &hashtags_bin), hashtags_list);

      *end = '|';
      start = end + 1;
    }
  }

  start = keyphrases;
  end = strstr(start, "|");
  len = 0;

  ERL_NIF_TERM keyphrases_list = enif_make_list(env, 0);
  if (keyphrases_count > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_val = enif_alloc_binary(len, &keyphrases_bin);
#else
      ret_val = enif_alloc_binary(env, len, &keyphrases_bin);
#endif
      if (ret_val < 0) {
#ifndef TRENDS_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_keyphrases_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        keyphrases_bin.data[i] = *(start + i);
      }
      keyphrases_list = enif_make_list_cell(env, enif_make_binary(env, &keyphrases_bin), keyphrases_list);

      *end = '|';
      start = end + 1;
    }
  }

  len = strlen(buffer1);
  if (len != 2 && len != 3) {
    strcpy(buffer1, "00");
    len = 2;
  }
#ifdef ERLANG_R14B02 
  ret_val = enif_alloc_binary(len, &buffer1_bin);
#else
  ret_val = enif_alloc_binary(env, len, &buffer1_bin);
#endif
  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_buffer1_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    buffer1_bin.data[i] = *(buffer1 + i);
  }
  buffer1_term = enif_make_binary(env, &buffer1_bin);

  len = strlen(buffer2);
  if (len != 2 && len != 3) {
    strcpy(buffer2, "00");
    len = 2;
  }
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(len, &buffer2_bin);
#else
  ret_val = enif_alloc_binary(env, len, &buffer2_bin);
#endif
  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_buffer2_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    buffer2_bin.data[i] = *(buffer2 + i);
  }
  buffer2_term = enif_make_binary(env, &buffer2_bin);

  start = buffer3;
  end = strstr(start, "|");
  len = 0;

  ERL_NIF_TERM channels_list = enif_make_list(env, 0);
  if (strlen(buffer3) > 0) {
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02
      ret_val = enif_alloc_binary(len, &channels_bin);
#else
      ret_val = enif_alloc_binary(env, len, &channels_bin);
#endif
      if (ret_val < 0) {
#ifndef TRENDS_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_channels_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        channels_bin.data[i] = *(start + i);
      }
      channels_list = enif_make_list_cell(env, enif_make_binary(env, &channels_bin), channels_list);

      *end = '|';
      start = end + 1;
    }
  }

  len = strlen(buffer4);
  if (len < 3) {
    strcpy(buffer4, "000");
    len = 3;
  }
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(len, &buffer4_bin);
#else
  ret_val = enif_alloc_binary(env, len, &buffer4_bin);
#endif
  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_buffer4_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    buffer4_bin.data[i] = *(buffer4 + i);
  }
  buffer4_term = enif_make_binary(env, &buffer4_bin);

#ifndef TRENDS_DEBUG
  return enif_make_tuple9(env, safe_status_term, lang_term, keywords_list, hashtags_list, keyphrases_list, buffer1_term, buffer2_term, channels_list, buffer4_term);
#else
  return enif_make_tuple9(env, safe_status_term, lang_term, keywords_list, hashtags_list, keyphrases_list, buffer1_term, buffer2_term, channels_list, buffer4_term);
/*
  ErlNifBinary tweet_debug_bin;
  ERL_NIF_TERM tweet_debug_term; 
  len = strlen(tweet_str);
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(len, &tweet_debug_bin);
#else
  ret_val = enif_alloc_binary(env, len, &tweet_debug_bin);
#endif
  if (ret_val < 0) {
    return enif_make_atom(env, "error");
  }
  for (i=0; i<len; i++) {
    tweet_debug_bin.data[i] = *(tweet_str + i);
  }
  tweet_debug_term = enif_make_binary(env, &tweet_debug_bin);
  return enif_make_tuple10(env, tweet_debug_term, safe_status_term, lang_term, keywords_list, hashtags_list, keyphrases_list, buffer1_term, buffer2_term, channels_list, buffer4_term);
*/
#endif
}

ERL_NIF_TERM nif_get_lang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary tweet;
  char tweet_str[MAX_BUFFER_LEN];
  memset(tweet_str, 0, MAX_BUFFER_LEN);

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
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_tweet_len");
#endif
  }

  char safe_status[10];
  memset(safe_status, 0, 10);
  char script[4];
  memset(script, 0, 4);
  char keywords[MAX_BUFFER_LEN];
  keywords[0] = '\0';
  int keywords_len = 0;
  int keywords_count = 0;
  char hashtags[MAX_BUFFER_LEN];
  hashtags[0] = '\0';
  int hashtags_len = 0;
  int hashtags_count = 0;
  char keyphrases[MAX_BUFFER_LEN];
  keyphrases[0] = '\0';
  int keyphrases_len = 0;
  int keyphrases_count = 0;
  char buffer1[MAX_BUFFER_LEN];
  buffer1[0] = '\0';
  char buffer2[MAX_BUFFER_LEN];
  buffer2[0] = '\0';
  char buffer3[MAX_BUFFER_LEN];
  buffer3[0] = '\0';
  char buffer4[MAX_BUFFER_LEN];
  buffer4[0] = '\0';

  int ret_value = 0;
  if ((ret_value = GetKeywords((const char *) tweet_str, tweet_len,
                  (char *) safe_status, 10,
                  (char *) script, 4,
                  (char *) keywords, MAX_BUFFER_LEN,
                  &keywords_len, &keywords_count,
                  (char *) hashtags, MAX_BUFFER_LEN,
                  &hashtags_len, &hashtags_count,
                  (char *) keyphrases, MAX_BUFFER_LEN,
                  &keyphrases_len, &keyphrases_count,
                  (char *) buffer1, MAX_BUFFER_LEN,
                  (char *) buffer2, MAX_BUFFER_LEN,
                  (char *) buffer3, MAX_BUFFER_LEN,
                  (char *) buffer4, MAX_BUFFER_LEN)) < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_GetKeywords");
#endif
  }
  
  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;
  ErlNifBinary buffer1_bin;
  ErlNifBinary buffer2_bin;
  ERL_NIF_TERM buffer1_term; 
  ERL_NIF_TERM buffer2_term; 

  len = strlen(buffer1);
  if (len != 2 && len != 3) {
    strcpy(buffer1, "00");
    len = 2;
  }
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(len, &buffer1_bin);
#else
  ret_val = enif_alloc_binary(env, len, &buffer1_bin);
#endif
  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_buffer1_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    buffer1_bin.data[i] = *(buffer1 + i);
  }
  buffer1_term = enif_make_binary(env, &buffer1_bin);

  len = strlen(buffer2);
  if (len != 2 && len != 3) {
    strcpy(buffer2, "00");
    len = 2;
  }
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(len, &buffer2_bin);
#else
  ret_val = enif_alloc_binary(env, len, &buffer2_bin);
#endif
  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_buffer2_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    buffer2_bin.data[i] = *(buffer2 + i);
  }
  buffer2_term = enif_make_binary(env, &buffer2_bin);

//#ifndef TRENDS_DEBUG
//  return enif_make_tuple2(env, buffer1_term, buffer2_term);
//#else
  ErlNifBinary tweet_debug_bin;
  ERL_NIF_TERM tweet_debug_term; 
  len = strlen(tweet_str);
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(len, &tweet_debug_bin);
#else
  ret_val = enif_alloc_binary(env, len, &tweet_debug_bin);
#endif
  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_no_memory_for_tweet_debug_bin");
#endif
  }
  for (i=0; i<len; i++) {
    tweet_debug_bin.data[i] = *(tweet_str + i);
  }
  tweet_debug_term = enif_make_binary(env, &tweet_debug_bin);
  return enif_make_tuple3(env, tweet_debug_term, buffer1_term, buffer2_term);
//#endif
}

ERL_NIF_TERM nif_get_trends(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  ErlNifBinary trends_bin;
  unsigned char trends_buffer[MAX_LIST_BUFFER_LEN];
  unsigned int trends_len = 0;

  if (enif_inspect_binary(env, argv[0], &trends_bin)) {
    memcpy((char *)trends_buffer, trends_bin.data, trends_bin.size);
    trends_buffer[trends_bin.size] = '\0';
    trends_len = trends_bin.size;
#ifdef ERLANG_R14B02 
    enif_release_binary(&trends_bin);
#else
    enif_release_binary(env, &trends_bin);
#endif
  } else {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_trends_bin");
#endif
  }

  unsigned int trends_count = 0;
  if (GetTrends(trends_buffer, &trends_len, &trends_count) < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_GetTrends");
#endif
  }

  ERL_NIF_TERM trends_list = enif_make_list(env, 0);

  if (strlen(trends_buffer) > 0) {
    char* start = (char*) trends_buffer;
    char* end = strstr(start, "|");
    unsigned int len = 0;
    ErlNifBinary trend_bin;
    int ret_val = 0;
    unsigned int i=0;

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02
      ret_val = enif_alloc_binary(len, &trend_bin);
#else
      ret_val = enif_alloc_binary(env, len, &trend_bin);
#endif
      if (ret_val < 0) {
#ifndef TRENDS_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_trend_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        trend_bin.data[i] = *(start + i);
      }
      trends_list = enif_make_list_cell(env, enif_make_binary(env, &trend_bin), trends_list);

      *end = '|';
      start = end + 1;
    }
  }

  return trends_list;
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
      user_name_str[user_name.size] = '\0';
#ifdef ERLANG_R14B02 
      enif_release_binary(&user_name);
#else
      enif_release_binary(env, &user_name);
#endif
    }   else {
#ifdef ERLANG_R14B02 
      enif_release_binary(&user_name);
#else
      enif_release_binary(env, &user_name);
#endif
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_user_name");
#endif
    }

    if (GetTestTweets(user_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  } else {
    if (GetTestTweets(NULL, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_null_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_out_length");
#endif
  }

  ErlNifBinary tweet;
  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  int ret_val = 0;
  unsigned int i = 0;
  unsigned int tweet_len = 0;
  ERL_NIF_TERM arg_array[1]; 
  ERL_NIF_TERM tuple9;
  ERL_NIF_TERM tuple9_list = enif_make_list(env, 0);
  unsigned int error_count = 0;

  while (tweet_start && tweet_end && *tweet_end != '\0') {

    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_out_length");
#endif
    }

#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif

    if (ret_val < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_len");
#endif
    }
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    tuple9 = nif_get_keywords(env, 1, arg_array);
    if (enif_is_atom(env, tuple9)) {
      error_count++;
    } else {
      tuple9_list = enif_make_list_cell(env, tuple9, tuple9_list);
    }
    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuple9_list;
/*
  ERL_NIF_TERM tuple2_list = enif_make_list(env, 0);
  enif_make_list_cell(env, tuple2_list, tuple9_list);
  char error_str[10];
  sprintf(error_str, "%d", error_count);
  ERL_NIF_TERM error_term = enif_make_atom(env, error_str);
  enif_make_list_cell(env, tuple2_list, error_term);
  
  return tuple2_list;
*/
}

ERL_NIF_TERM nif_test_twitter_timeline_lang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  char tweets_buffer[MAX_LIST_BUFFER_LEN];
  memset(tweets_buffer, 0, MAX_LIST_BUFFER_LEN);

  int out_length = 0;
  if (argc == 1) {
    char user_name_str[MAX_NAME_LEN];
    memset(user_name_str, 0, MAX_NAME_LEN);
    ErlNifBinary user_name;

    if (enif_inspect_binary(env, argv[0], &user_name)) {
      memcpy(user_name_str, user_name.data, user_name.size);
      user_name_str[user_name.size] = '\0';
#ifdef ERLANG_R14B02 
      enif_release_binary(&user_name);
#else
      enif_release_binary(env, &user_name);
#endif
    }   else {
#ifdef ERLANG_R14B02 
      enif_release_binary(&user_name);
#else
      enif_release_binary(env, &user_name);
#endif
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_user_name");
#endif
    }

    if (GetTestTweets(user_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  } else {
    if (GetTestTweets(NULL, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_null_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_out_length");
#endif
  }

  ErlNifBinary tweet;
  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  int ret_val = 0;
  unsigned int i = 0;
  unsigned int tweet_len = 0;
  ERL_NIF_TERM arg_array[1]; 
  ERL_NIF_TERM tuples;
  ERL_NIF_TERM tuples_list = enif_make_list(env, 0);
  unsigned int error_count = 0;

  while (tweet_start && tweet_end && *tweet_end != '\0') {

    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_out_length");
#endif
    }

#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif

    if (ret_val < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_len");
#endif
    }
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    tuples = nif_get_lang(env, 1, arg_array);
    if (enif_is_atom(env, tuples)) {
      error_count++;
    } else {
      tuples_list = enif_make_list_cell(env, tuples, tuples_list);
    }
    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuples_list;
}

ERL_NIF_TERM nif_test_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  char tweets_buffer[MAX_LIST_BUFFER_LEN];
  memset(tweets_buffer, 0, MAX_LIST_BUFFER_LEN);

  int out_length = 0;
  if (argc != 1) {
#ifndef TRENDS_DEBUG
     return enif_make_atom(env, "error"); 
#else
     return enif_make_atom(env, "error_invalid_argc"); 
#endif
  } else {
    char file_name_str[MAX_NAME_LEN];
    memset(file_name_str, 0, MAX_NAME_LEN);
    ErlNifBinary file_name;

    if (enif_inspect_binary(env, argv[0], &file_name)) {
      memcpy(file_name_str, file_name.data, file_name.size);
      file_name_str[file_name.size] = '\0';
#ifdef ERLANG_R14B02 
      enif_release_binary(&file_name);
#else
      enif_release_binary(env, &file_name);
#endif
    }   else {
#ifdef ERLANG_R14B02 
      enif_release_binary(&file_name);
#else
      enif_release_binary(env, &file_name);
#endif
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_file_name");
#endif
    }

    if (GetTestTweetsFromFile(file_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_out_length");
#endif
  }

  ErlNifBinary tweet;
  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  int ret_val = 0;
  unsigned int i = 0;
  unsigned int tweet_len = 0;
  ERL_NIF_TERM arg_array[1]; 
  ERL_NIF_TERM tuple9;
  ERL_NIF_TERM tuple9_list = enif_make_list(env, 0);
  unsigned int error_count = 0;

  while (tweet_start && tweet_end && *tweet_end != '\0') {

    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_out_length");
#endif
    }

#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif

    if (ret_val < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_len");
#endif
    }
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    tuple9 = nif_get_keywords(env, 1, arg_array);
    if (enif_is_atom(env, tuple9)) {
      error_count++;
    } else {
      tuple9_list = enif_make_list_cell(env, tuple9, tuple9_list);
    }
    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuple9_list;
/*
  ERL_NIF_TERM tuple2_list = enif_make_list(env, 0);
  enif_make_list_cell(env, tuple2_list, tuple9_list);
  char error_str[10];
  sprintf(error_str, "%d", error_count);
  ERL_NIF_TERM error_term = enif_make_atom(env, error_str);
  enif_make_list_cell(env, tuple2_list, error_term);
  
  return tuple2_list;
*/
}

ERL_NIF_TERM nif_test_lang_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  char tweets_buffer[MAX_LIST_BUFFER_LEN];
  memset(tweets_buffer, 0, MAX_LIST_BUFFER_LEN);

  int out_length = 0;
  if (argc != 1) {
#ifndef TRENDS_DEBUG
     return enif_make_atom(env, "error"); 
#else
     return enif_make_atom(env, "error_invalid_argc"); 
#endif
  } else {
    char file_name_str[MAX_NAME_LEN];
    memset(file_name_str, 0, MAX_NAME_LEN);
    ErlNifBinary file_name;

    if (enif_inspect_binary(env, argv[0], &file_name)) {
      memcpy(file_name_str, file_name.data, file_name.size);
      file_name_str[file_name.size] = '\0';
#ifdef ERLANG_R14B02 
      enif_release_binary(&file_name);
#else
      enif_release_binary(env, &file_name);
#endif
    }   else {
#ifdef ERLANG_R14B02 
      enif_release_binary(&file_name);
#else
      enif_release_binary(env, &file_name);
#endif
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_file_name");
#endif
    }

    if (GetTestTweetsFromFile(file_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_out_length");
#endif
  }

  ErlNifBinary tweet;
  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  int ret_val = 0;
  unsigned int i = 0;
  unsigned int tweet_len = 0;
  ERL_NIF_TERM arg_array[1]; 
  ERL_NIF_TERM tuples;
  ERL_NIF_TERM tuples_list = enif_make_list(env, 0);
  unsigned int error_count = 0;

  while (tweet_start && tweet_end && *tweet_end != '\0') {

    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_out_length");
#endif
    }

#ifdef ERLANG_R14B02
    ret_val = enif_alloc_binary(tweet_len, &tweet);
#else
    ret_val = enif_alloc_binary(env, tweet_len, &tweet);
#endif

    if (ret_val < 0) {
#ifndef TRENDS_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_len");
#endif
    }
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    tuples = nif_get_lang(env, 1, arg_array);
    if (enif_is_atom(env, tuples)) {
      error_count++;
    } else {
      tuples_list = enif_make_list_cell(env, tuples, tuples_list);
    }
    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuples_list;
}

ERL_NIF_TERM nif_test_get_trends(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary file_path;

  char trends_file_path[MAX_NAME_LEN];
  bool success = enif_inspect_binary(env, argv[0], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(trends_file_path, file_path.data, file_path.size);
    trends_file_path[file_path.size] = '\0';
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
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_trends_file_path_inspect_bin");
#endif
  }

  unsigned char trends_buffer[MAX_LIST_BUFFER_LEN];
  unsigned int trends_len = 0;
  unsigned int trends_count = 0;
  if (GetTestTrends(trends_file_path,
                    trends_buffer, MAX_LIST_BUFFER_LEN,
                    &trends_len, &trends_count) < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_get_test_trends_failed");
#endif
  }

  if (trends_len <= 0 || trends_len >= MAX_LIST_BUFFER_LEN) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_trends_len");
#endif
  }

  ErlNifBinary trends_bin;
  int ret_val = 0;
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(trends_len, &trends_bin);
#else
  ret_val = enif_alloc_binary(env, trends_len, &trends_bin);
#endif

  if (ret_val < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_tweet_len");
#endif
  }
  unsigned int i=0;
  for (i=0; i<trends_len; i++) {
    trends_bin.data[i] = *(trends_buffer + i);
  }

  ERL_NIF_TERM arg_array[1];
  arg_array[0] = enif_make_binary(env, &trends_bin);
  ERL_NIF_TERM trends_list = nif_get_trends(env, 1, arg_array);
  if (enif_is_atom(env, trends_list)) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_get_trends_failed");
#endif
  }

  return trends_list; 
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 5, nif_init_c},
  {"getkeywords", 1, nif_get_keywords},
  {"getlang", 1, nif_get_lang},
  {"gettrends", 1, nif_get_trends},
  {"test_keywords_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_keywords_twitter_timeline", 1, nif_test_twitter_timeline},
  {"test_keywords_file", 1, nif_test_from_file},
  {"test_lang_twitter_timeline", 0, nif_test_twitter_timeline_lang},
  {"test_lang_twitter_timeline", 1, nif_test_twitter_timeline_lang},
  {"test_lang_file", 1, nif_test_lang_from_file},
  {"test_trends_file", 1, nif_test_get_trends},
};
ERL_NIF_INIT(ir_cpp, nif_funcs, NULL, NULL, NULL, NULL)
