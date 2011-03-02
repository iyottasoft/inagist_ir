#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "trends_manager.h"

#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_LIST_BUFFER_LEN 20480
//#define TRENDS_DEBUG 1

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

ERL_NIF_TERM nif_getkeywords(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

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
    enif_release_binary(&tweet);
  } else {
    enif_release_binary(&tweet);
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
  if ((ret_value = SubmitTweet((const char *) tweet_str, tweet_len,
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
    strcpy(safe_status, "error");
    len = 5;
  }
  ret_val = enif_alloc_binary(len, &safe_status_bin);
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
  ret_val = enif_alloc_binary(len, &script_bin);
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

      ret_val = enif_alloc_binary(len, &keywords_bin);
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

      ret_val = enif_alloc_binary(len, &hashtags_bin);
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

      ret_val = enif_alloc_binary(len, &keyphrases_bin);
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
  ret_val = enif_alloc_binary(len, &buffer1_bin);
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
  ret_val = enif_alloc_binary(len, &buffer2_bin);
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

      ret_val = enif_alloc_binary(len, &channels_bin);
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
  ret_val = enif_alloc_binary(len, &buffer4_bin);
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
  ErlNifBinary tweet_debug_bin;
  ERL_NIF_TERM tweet_debug_term; 
  len = strlen(tweet_str);
  ret_val = enif_alloc_binary(env, len, &tweet_debug_bin);
  if (ret_val < 0) {
    return enif_make_atom(env, "error");
  }
  for (i=0; i<len; i++) {
    tweet_debug_bin.data[i] = *(tweet_str + i);
  }
  tweet_debug_term = enif_make_binary(env, &tweet_debug_bin);
  return enif_make_tuple10(env, tweet_debug_term, safe_status_term, lang_term, keywords_list, hashtags_list, keyphrases_list, buffer1_term, buffer2_term, channels_list, buffer4_term);
#endif
}

ERL_NIF_TERM nif_getlang(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

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
    enif_release_binary(&tweet);
  } else {
    enif_release_binary(&tweet);
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
  if ((ret_value = SubmitTweet((const char *) tweet_str, tweet_len,
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
  ret_val = enif_alloc_binary(len, &buffer1_bin);
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
  ret_val = enif_alloc_binary(len, &buffer2_bin);
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
  ret_val = enif_alloc_binary(len, &tweet_debug_bin);
  if (ret_val < 0) {
    return enif_make_atom(env, "error");
  }
  for (i=0; i<len; i++) {
    tweet_debug_bin.data[i] = *(tweet_str + i);
  }
  tweet_debug_term = enif_make_binary(env, &tweet_debug_bin);
  return enif_make_tuple3(env, tweet_debug_term, buffer1_term, buffer2_term);
//#endif
}

ERL_NIF_TERM nif_gettrends(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ErlNifBinary user_name;
  char user_name_str[MAX_NAME_LEN];
  memset(user_name_str, 0, MAX_NAME_LEN);

  if (enif_inspect_binary(env, argv[0], &user_name)) {
    memcpy((char *)user_name_str, user_name.data, user_name.size);
    user_name_str[user_name.size] = '\0';
    enif_release_binary(&user_name);
  } else {
    return enif_make_atom(env, "error");
  }

  char trends_str[MAX_BUFFER_LEN];
  trends_str[0] = '\0';

  if (GetTrends((const char *) user_name_str, (char *) trends_str) < 0) {
    return enif_make_atom(env, "error");
  }

  return enif_make_atom(env, trends_str);
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
    enif_release_binary(&file_path);
  } else {
    enif_release_binary(&file_path);
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
    enif_release_binary(&file_path);
  } else {
    enif_release_binary(&file_path);
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
    enif_release_binary(&file_path);
  } else {
    enif_release_binary(&file_path);
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
    enif_release_binary(&file_path);
  } else {
    enif_release_binary(&file_path);
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
    enif_release_binary(&file_path);
  } else {
    enif_release_binary(&file_path);
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_channels_dict_file_path_inspect_bin");
#endif
  }

  if (Init(stopwords_file_path,
           dictionary_file_path,
           unsafe_dictionary_file_path,
           lang_detect_config_file_path,
           channels_dictionary_file_path) < 0) {
#ifndef TRENDS_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_init_dicts");
#endif
  }

  return enif_make_atom(env, "ok");
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
      enif_release_binary(&user_name);
    }   else {
      enif_release_binary(&user_name);
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

    ret_val = enif_alloc_binary(tweet_len, &tweet);
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
    tuple9 = nif_getkeywords(env, 1, arg_array);
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
      enif_release_binary(&user_name);
    }   else {
      enif_release_binary(&user_name);
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

    ret_val = enif_alloc_binary(tweet_len, &tweet);
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
    tuples = nif_getlang(env, 1, arg_array);
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
#ifndef LD_DEBUG
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
      enif_release_binary(&file_name);
    }   else {
      enif_release_binary(&file_name);
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

    ret_val = enif_alloc_binary(tweet_len, &tweet);
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
    tuple9 = nif_getkeywords(env, 1, arg_array);
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
#ifndef LD_DEBUG
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
      enif_release_binary(&file_name);
    }   else {
      enif_release_binary(&file_name);
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

    ret_val = enif_alloc_binary(tweet_len, &tweet);
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
    tuples = nif_getlang(env, 1, arg_array);
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

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 5, nif_init_c},
  {"getkeywords", 1, nif_getkeywords},
  {"getlang", 1, nif_getlang},
  {"gettrends", 1, nif_gettrends},
  {"test_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_twitter_timeline", 1, nif_test_twitter_timeline},
  {"test_twitter_timeline_lang", 0, nif_test_twitter_timeline_lang},
  {"test_twitter_timeline_lang", 1, nif_test_twitter_timeline_lang},
  {"test_from_file", 1, nif_test_from_file},
  {"test_lang_from_file", 1, nif_test_lang_from_file},
};
ERL_NIF_INIT(trends, nif_funcs, NULL, NULL, NULL, NULL)
