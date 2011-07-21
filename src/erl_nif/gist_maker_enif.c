#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "tweets.h"
#include "gist_maker_eapi.h"

#define ULTIMATE_BUFFER_LEN 10240
#define MAX_BUFFER_LEN 1024
#define MAX_NAME_LEN 255
#define MAX_CLASS_NAME 32
#define MAX_LIST_BUFFER_LEN 20480

//#define GIST_DEBUG 1
#define ERLANG_R14B02 1

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 3) {
#ifndef GIST_DEBUG
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
#ifndef GIST_DEBUG
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
#ifndef GIST_DEBUG
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
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_unsafe_dict_file_path_inspect_bin");
#endif
  }

  if (InitGistMaker(keytuples_config_file_path,
                    language_detector_config_file_path,
                    text_classifier_config_file_path) < 0) {
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_init_trends_manager");
#endif
  }

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_get_gist(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary tweet;
  unsigned char tweet_str[MAX_BUFFER_LEN];
  memset((char *) tweet_str, '\0', MAX_BUFFER_LEN);

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
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_tweet_len");
#endif
  }

  char safe_status_buffer[10];
  memset(safe_status_buffer, '\0', 10);
  unsigned int safe_status_buffer_len = 10;

  char script_buffer[4];
  memset(script_buffer, '\0', 4);
  unsigned int script_buffer_len = 4;

  char lang[MAX_BUFFER_LEN];
  lang[0] = '\0';

  unsigned char keywords[MAX_BUFFER_LEN];
  keywords[0] = '\0';
  int keywords_len = 0;
  int keywords_count = 0;

  unsigned char hashtags[MAX_BUFFER_LEN];
  hashtags[0] = '\0';
  int hashtags_len = 0;
  int hashtags_count = 0;

  unsigned char keyphrases[MAX_BUFFER_LEN];
  keyphrases[0] = '\0';
  int keyphrases_len = 0;
  int keyphrases_count = 0;

  unsigned char text_class_words_buffer[MAX_BUFFER_LEN];
  text_class_words_buffer[0] = '\0';
  int text_class_words_len = 0;
  int text_class_words_count = 0;

  char text_classes_buffer[MAX_BUFFER_LEN];
  text_classes_buffer[0] = '\0';
  int text_classes_len = 0;
  int text_classes_count = 0;

  char intent_buffer[MAX_CLASS_NAME];
  intent_buffer[0] = '\0';
  unsigned int intent_buffer_len = MAX_CLASS_NAME;

  char sentiment_buffer[MAX_CLASS_NAME];
  sentiment_buffer[0] = '\0';
  unsigned int sentiment_buffer_len = MAX_CLASS_NAME;

  int ret_value = 0;
  if ((ret_value = InaGist((const unsigned char *) tweet_str, tweet_len,
                  (char *) safe_status_buffer, safe_status_buffer_len,
                  (char *) script_buffer, script_buffer_len
                  , (char *) lang, MAX_BUFFER_LEN
                  , (unsigned char *) keywords, MAX_BUFFER_LEN,
                  &keywords_len, &keywords_count
                  , (unsigned char *) hashtags, MAX_BUFFER_LEN,
                  &hashtags_len, &hashtags_count
                  , (unsigned char *) keyphrases, MAX_BUFFER_LEN,
                  &keyphrases_len, &keyphrases_count
                  //, (unsigned char *) text_class_words_buffer, MAX_BUFFER_LEN,
                  //&text_class_words_len, &text_class_words_count,
                  , (char *) text_classes_buffer, MAX_BUFFER_LEN,
                  &text_classes_len, &text_classes_count
                  , (char *) intent_buffer, MAX_CLASS_NAME
                  , (char *) sentiment_buffer, MAX_CLASS_NAME
                 )) < 0) {
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_GetGist_failed");
#endif
  }
  tweet_str[0] = '\0';
  tweet_len = 0;

  unsigned int len = 0;
  unsigned int i = 0;
  int ret_val = 0;

  // safe/unsafe status
  len = strlen(safe_status_buffer);
  if (len < 4 || len > 6) {
    strcpy(safe_status_buffer, "ERROR");
    len = 5;
  }
  ErlNifBinary safe_status_bin;
#ifdef ERLANG_R14B02 
  ret_val = enif_alloc_binary(len, &safe_status_bin);
#else
  ret_val = enif_alloc_binary(env, len, &safe_status_bin);
#endif
  if (ret_val < 0) {
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_safe_status_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    safe_status_bin.data[i] = *(safe_status_buffer + i);
  }
  ERL_NIF_TERM safe_status_term = enif_make_binary(env, &safe_status_bin);
  safe_status_buffer[0] = '\0';

  // script
  len = strlen(script_buffer);
  if (len != 2 && len != 3) {
    strcpy(script_buffer, "00");
    len = 2;
  }
  ErlNifBinary script_bin;
#ifdef ERLANG_R14B02 
  ret_val = enif_alloc_binary(len, &script_bin);
#else
  ret_val = enif_alloc_binary(env, len, &script_bin);
#endif
  if (ret_val < 0) {
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_script_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    script_bin.data[i] = *(script_buffer + i);
  }
  ERL_NIF_TERM script_term = enif_make_binary(env, &script_bin);
  script_buffer[0] = '\0';

  // language
  ERL_NIF_TERM lang_term;
  len = strlen(lang);
  if (len != 2 && len != 3) {
    strcpy(lang, "00");
    len = 2;
  }
  ErlNifBinary lang_bin;
#ifdef ERLANG_R14B02 
  ret_val = enif_alloc_binary(len, &lang_bin);
#else
  ret_val = enif_alloc_binary(env, len, &lang_bin);
#endif
  if (ret_val < 0) {
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_lang_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    lang_bin.data[i] = *(lang + i);
  }
  lang_term = enif_make_binary(env, &lang_bin);
  lang[0] = '\0';

  // keywords
  char* start = NULL;
  char* end = NULL;
  ERL_NIF_TERM keywords_list = enif_make_list(env, 0);
  if (keywords_count > 0) {
    start = keywords;
    end = strstr(start, "|");
    len = 0;
    ErlNifBinary keywords_bin;
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
#ifndef GIST_DEBUG
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
  keywords[0] = '\0';

  // hashtags
  ERL_NIF_TERM hashtags_list = enif_make_list(env, 0);
  if (hashtags_count > 0) {
    start = hashtags;
    end = strstr(start, "|");
    len = 0;
    ErlNifBinary hashtags_bin;
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
#ifndef GIST_DEBUG
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
  hashtags[0] = '\0';

  // keyphrases
  ERL_NIF_TERM keyphrases_list = enif_make_list(env, 0);
  if (keyphrases_count > 0) {
    start = keyphrases;
    end = strstr(start, "|");
    len = 0;
    ErlNifBinary keyphrases_bin;
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
#ifndef GIST_DEBUG
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
  keyphrases[0] = 0;

  // text_class
  ERL_NIF_TERM text_classes_list = enif_make_list(env, 0);
  len = strlen(text_classes_buffer);
  /*
  if (len < 1) {
    strcpy(text_classes_buffer, "00|");
    len = 3;
    text_classes_count = 1;
  }
  */

  if (text_classes_count > 0) {
    ErlNifBinary text_class_bin;
    start = text_classes_buffer;
    end = strstr(start, "|");
    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_val = enif_alloc_binary(len, &text_class_bin);
#else
      ret_val = enif_alloc_binary(env, len, &text_class_bin);
#endif
      if (ret_val < 0) {
#ifndef GIST_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_text_class_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        text_class_bin.data[i] = *(start + i);
      }
      text_classes_list = enif_make_list_cell(env, enif_make_binary(env, &text_class_bin), text_classes_list);

      *end = '|';
      start = end + 1;
    }
  }
  text_classes_buffer[0] = '\0';

  // intent
  ERL_NIF_TERM intent_term;
  len = strlen(intent_buffer);
  if (len < 1) {
    strcpy(intent_buffer, "00");
    len = 2;
  }
  ErlNifBinary intent_bin;
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(len, &intent_bin);
#else
  ret_val = enif_alloc_binary(env, len, &intent_bin);
#endif
  if (ret_val < 0) {
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_intent_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    intent_bin.data[i] = *(intent_buffer + i);
  }
  intent_term = enif_make_binary(env, &intent_bin);
  intent_buffer[0] = '\0';

  // sentiment
  ERL_NIF_TERM sentiment_term;
  len = strlen(sentiment_buffer);
  if (len < 1) {
    strcpy(sentiment_buffer, "00");
    len = 2;
  }
  ErlNifBinary sentiment_bin;
#ifdef ERLANG_R14B02
  ret_val = enif_alloc_binary(len, &sentiment_bin);
#else
  ret_val = enif_alloc_binary(env, len, &sentiment_bin);
#endif
  if (ret_val < 0) {
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_sentiment_bin_alloc");
#endif
  }
  for (i=0; i<len; i++) {
    sentiment_bin.data[i] = *(sentiment_buffer + i);
  }
  sentiment_term = enif_make_binary(env, &sentiment_bin);
  sentiment_buffer[0] = '\0';

  start = NULL;
  end = NULL;

  return enif_make_tuple9(env,
                          safe_status_term,
                          script_term,
                          lang_term,
                          keywords_list,
                          hashtags_list,
                          keyphrases_list,
                          text_classes_list,
                          // text_class_contributors_list,
                          intent_term,
                          sentiment_term);

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
#ifndef GIST_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_user_name");
#endif
    }

    if (GetTestTweets(user_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef GIST_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  } else {
    if (GetTestTweets(NULL, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef GIST_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_null_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef GIST_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_out_length");
#endif
  }

  ErlNifBinary tweet;
  ErlNifBinary out_tweet;
  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  int ret_val = 0;
  unsigned int i = 0;
  unsigned int tweet_len = 0;
  ERL_NIF_TERM my_argv[1]; 
  unsigned int my_argc = 1;
  ERL_NIF_TERM tuple9;
  ERL_NIF_TERM tuple2;
  ERL_NIF_TERM tuple2_list = enif_make_list(env, 0);
  unsigned int error_count = 0;

  while (tweet_start && tweet_end && *tweet_end != '\0') {

    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
#ifndef GIST_DEBUG
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
#ifndef GIST_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_len");
#endif
    }
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    my_argv[0] = enif_make_binary(env, &tweet);
    tuple9 = nif_get_gist(env, my_argc, my_argv);
    if (enif_is_atom(env, tuple9)) {
      error_count++;
    } else {
#ifdef ERLANG_R14B02
       ret_val = enif_alloc_binary(tweet_len, &out_tweet);
#else
       ret_val = enif_alloc_binary(env, tweet_len, &out_tweet);
#endif
      if (ret_val < 0) {
#ifndef GIST_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_tweet_len");
#endif
      }
      for (i=0; i<tweet_len; i++) {
        out_tweet.data[i] = *(tweet_start + i);
      }
      tuple2 = enif_make_tuple2(env, enif_make_binary(env, &out_tweet), tuple9);
      tuple2_list = enif_make_list_cell(env, tuple2, tuple2_list);
    }
    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

  return tuple2_list;
}

ERL_NIF_TERM nif_test_from_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  char tweets_buffer[MAX_LIST_BUFFER_LEN];
  memset(tweets_buffer, 0, MAX_LIST_BUFFER_LEN);

  int out_length = 0;
  if (argc != 1) {
#ifndef GIST_DEBUG
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
#ifndef GIST_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_file_name");
#endif
    }

    if (GetTestTweetsFromFile(file_name_str, MAX_LIST_BUFFER_LEN, tweets_buffer, &out_length) < 0) {
#ifndef GIST_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_get_test_tweets_for_user");
#endif
    }
  }

  if (0 == out_length || out_length > MAX_LIST_BUFFER_LEN) {
#ifndef GIST_DEBUG
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
#ifndef GIST_DEBUG
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
#ifndef GIST_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_tweet_len");
#endif
    }
    for (i=0; i<tweet_len; i++) {
      tweet.data[i] = *(tweet_start + i);
    }

    arg_array[0] = enif_make_binary(env, &tweet);
    tuple9 = nif_get_gist(env, 1, arg_array);
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
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 3, nif_init_c},
  {"get_gist", 1, nif_get_gist},
  {"test_gist_twitter_timeline", 0, nif_test_twitter_timeline},
  {"test_gist_twitter_timeline", 1, nif_test_twitter_timeline},
  {"test_gist_file", 1, nif_test_from_file},
};
ERL_NIF_INIT(gist_maker, nif_funcs, NULL, NULL, NULL, NULL)

