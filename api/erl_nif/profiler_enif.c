/*
  TODO (balaji) use struct to reduce the parameters and make the code look sane.
  copy pasting scores of parameters to meet current deadlines.
*/

#include "erl_nif.h"
#include "erl_driver.h"

#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "profile.h"

#define ULTIMATE_BUFFER_LEN 10240
#define MAX_BUFFER_LEN       1024
#define MAX_NAME_LEN          255
#define MAX_LIST_LEN          255
#define MAX_CLASS_NAME        255

#define ERLANG_R14B02 1
#define PROFILE_DEBUG 1

ERL_NIF_TERM nif_init_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc != 1) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  ErlNifBinary file_path;

  char gist_maker_config_file_path[MAX_NAME_LEN];
  bool success = enif_inspect_binary(env, argv[0], &file_path);
  if (success && (file_path.size < MAX_NAME_LEN)) {
    memcpy(gist_maker_config_file_path, file_path.data, file_path.size);
    gist_maker_config_file_path[file_path.size] = '\0';
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
    return enif_make_atom(env, "error_gist_maker_config_file_path_inspect_bin");
#endif
  }

  if (InitProfiler(gist_maker_config_file_path) < 0) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_init_trends_manager");
#endif
  }

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM nif_profile(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

  if (argc < 1 || argc > 3) {
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_argc");
#endif
  }

  // TODO (balaji) make this integer
  ErlNifBinary input_type_bin;
  unsigned char input_type_buffer[MAX_NAME_LEN];
  memset(input_type_buffer, 0, MAX_NAME_LEN);

  ErlNifBinary input_value_bin;
  unsigned char input_value_buffer[MAX_NAME_LEN];
  memset(input_value_buffer, 0, MAX_NAME_LEN);

  bool success = enif_inspect_binary(env, argv[0], &input_type_bin);
  int input_type_len = input_type_bin.size;
  if (success && input_type_len > 1 && input_type_len < MAX_NAME_LEN) {
    memcpy(input_type_buffer, input_type_bin.data, input_type_len);
    input_type_buffer[input_type_len] = '\0';
#ifdef ERLANG_R14B02 
    enif_release_binary(&input_type_bin);
#else
    enif_release_binary(env, &input_type_bin);
#endif
  } else {
#ifdef ERLANG_R14B02 
    enif_release_binary(&input_type_bin);
#else
    enif_release_binary(env, &input_type_bin);
#endif
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_input_type_len");
#endif
  }

  success = enif_inspect_binary(env, argv[1], &input_value_bin);
  int input_value_len = input_value_bin.size;
  if (success && input_value_len > 1 && input_value_len < MAX_NAME_LEN) {
    memcpy(input_value_buffer, input_value_bin.data, input_value_len);
    input_value_buffer[input_value_len] = '\0';
#ifdef ERLANG_R14B02 
    enif_release_binary(&input_value_bin);
#else
    enif_release_binary(env, &input_value_bin);
#endif
  } else {
#ifdef ERLANG_R14B02 
    enif_release_binary(&input_value_bin);
#else
    enif_release_binary(env, &input_value_bin);
#endif
#ifndef PROFILE_DEBUG
    return enif_make_atom(env, "error");
#else
    return enif_make_atom(env, "error_invalid_input_value_len");
#endif
  }

  char safe_status_buffer[10];
  safe_status_buffer[0] = '\0';

  unsigned char locations_buffer[MAX_LIST_LEN];
  locations_buffer[0] = '\0';
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;

  char self_languages_buffer[MAX_LIST_LEN];
  self_languages_buffer[0] = '\0';
  unsigned int self_languages_len = 0;
  unsigned int self_languages_count = 0;

  char self_text_classes_buffer[MAX_BUFFER_LEN];
  self_text_classes_buffer[0] = '\0';
  unsigned int self_text_classes_len = 0;
  unsigned int self_text_classes_count = 0;

#ifdef LOCATION_ENABLED
  char self_location_classes_buffer[MAX_BUFFER_LEN];
  self_location_classes_buffer[0] = '\0';
  unsigned int self_location_classes_len = 0;
  unsigned int self_location_classes_count = 0;
#endif // LOCATION_ENABLED

  char self_text_class_contributors_buffer[ULTIMATE_BUFFER_LEN];
  self_text_class_contributors_buffer[0] = '\0';
  unsigned int self_text_class_contributors_len = 0;
  unsigned int self_text_class_contributors_count = 0;

  char others_languages_buffer[MAX_LIST_LEN];
  others_languages_buffer[0] = '\0';
  unsigned int others_languages_len = 0;
  unsigned int others_languages_count = 0;

  char others_text_classes_buffer[MAX_BUFFER_LEN];
  others_text_classes_buffer[0] = '\0';
  unsigned int others_text_classes_len = 0;
  unsigned int others_text_classes_count = 0;

#ifdef LOCATION_ENABLED
  char others_location_classes_buffer[MAX_BUFFER_LEN];
  others_location_classes_buffer[0] = '\0';
  unsigned int others_location_classes_len = 0;
  unsigned int others_location_classes_count = 0;
#endif // LOCATION_ENABLED

  char others_text_class_contributors_buffer[ULTIMATE_BUFFER_LEN];
  others_text_class_contributors_buffer[0] = '\0';
  unsigned int others_text_class_contributors_len = 0;
  unsigned int others_text_class_contributors_count = 0;

  char recommendations_buffer[MAX_BUFFER_LEN];
  recommendations_buffer[0] = '\0';
  unsigned int recommendations_len = 0;
  unsigned int recommendations_count = 0;

  char* profile_name = NULL;
  int ret_value = 0;

  if ((strcmp(input_type_buffer, "handle") == 0) ||
      (strcmp(input_type_buffer, "handle+") == 0)) {
    if ((ret_value = GetProfile(input_value_buffer, input_value_len,
                       locations_buffer, MAX_LIST_LEN,
                       &locations_len, &locations_count,
                       self_languages_buffer, MAX_LIST_LEN,
                       &self_languages_len, &self_languages_count,
                       self_text_classes_buffer, MAX_BUFFER_LEN,
                       &self_text_classes_len, &self_text_classes_count,
#ifdef LOCATION_ENABLED
                       self_location_classes_buffer, MAX_BUFFER_LEN,
                       &self_location_classes_len, &self_location_classes_count,
#endif // LOCATION_ENABLED
                       self_text_class_contributors_buffer, ULTIMATE_BUFFER_LEN,
                       &self_text_class_contributors_len, &self_text_class_contributors_count,
                       others_languages_buffer, MAX_LIST_LEN,
                       &others_languages_len, &others_languages_count,
                       others_text_classes_buffer, MAX_BUFFER_LEN,
                       &others_text_classes_len, &others_text_classes_count,
#ifdef LOCATION_ENABLED
                       others_location_classes_buffer, MAX_BUFFER_LEN,
                       &others_location_classes_len, &others_location_classes_count,
#endif // LOCATION_ENABLED
                       others_text_class_contributors_buffer, ULTIMATE_BUFFER_LEN,
                       &others_text_class_contributors_len, &others_text_class_contributors_count,
                       recommendations_buffer, ULTIMATE_BUFFER_LEN,
                       &recommendations_len, &recommendations_count,
                       profile_name)) < 0) {
#ifndef PROFILE_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_profile_failed");
#endif
    }
  } else if (strcmp(input_type_buffer, "file") == 0) {
/*
    if ((ret_value = GetProfileFromFile(input_value_buffer, input_value_len,
                             self_languages_buffer, MAX_LIST_LEN,
                             &self_languages_len, &self_languages_count,
                             self_text_classes_buffer, MAX_LIST_LEN,
                             &self_text_classes_len, &self_text_classes_count,
#ifdef LOCATION_ENABLED
                             self_location_classes_buffer, MAX_LIST_LEN,
                             &self_location_classes_len, &self_location_classes_count,
#endif // LOCATION_ENABLED
                             profile_name)) < 0) {
#ifndef PROFILE_DEBUG
      return enif_make_atom(env, "error");
#else
      return enif_make_atom(env, "error_profile_from_file_failed");
#endif
    }
*/
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
  locations_buffer[0] = '\0';

  // languages by the user

  ErlNifBinary self_languages_bin;
  ERL_NIF_TERM self_languages_list = enif_make_list(env, 0);

  if (self_languages_count > 0) {

    start = self_languages_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &self_languages_bin);
#else
      ret_value = enif_alloc_binary(env, len, &self_languages_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_self_languages_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        self_languages_bin.data[i] = *(start + i);
      }
      self_languages_list = enif_make_list_cell(env, enif_make_binary(env, &self_languages_bin), self_languages_list);

      *end = '|';
      start = end + 1;
    }
  }
  self_languages_buffer[0] = '\0';

  // text class for the user

  ErlNifBinary self_text_class_bin;
  ERL_NIF_TERM self_text_classes_list = enif_make_list(env, 0);

  ErlNifBinary self_text_class_label_bin;
  ERL_NIF_TERM self_text_class_labels_list = enif_make_list(env, 0);

  char* middle = NULL;
  if (self_text_classes_count > 0) {

    start = self_text_classes_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';

      middle = strstr(start, " ");
      if (!middle)
        break;
      *middle = '\0';
      len = middle - start;

#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &self_text_class_label_bin);
#else
      ret_value = enif_alloc_binary(env, len, &self_text_class_label_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_self_text_class_label_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        self_text_class_label_bin.data[i] = *(start + i);
      }
      self_text_class_labels_list = enif_make_list_cell(env, enif_make_binary(env, &self_text_class_label_bin), self_text_class_labels_list);
      *middle = ' ';
      start = middle + 1;

      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &self_text_class_bin);
#else
      ret_value = enif_alloc_binary(env, len, &self_text_class_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_self_text_class_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        self_text_class_bin.data[i] = *(start + i);
      }
      self_text_classes_list = enif_make_list_cell(env, enif_make_binary(env, &self_text_class_bin), self_text_classes_list);

      *end = '|';
      start = end + 1;
    }
  }
  self_text_classes_buffer[0] = '\0';

  // sub class for the user

  ErlNifBinary self_location_classes_bin;
  ERL_NIF_TERM self_location_classes_list = enif_make_list(env, 0);

#ifdef LOCATION_ENABLED
  if (self_location_classes_count > 0) {

    start = self_location_classes_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &self_location_classes_bin);
#else
      ret_value = enif_alloc_binary(env, len, &self_location_classes_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_self_location_classes_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        self_location_classes_bin.data[i] = *(start + i);
      }
      self_location_classes_list = enif_make_list_cell(env, enif_make_binary(env, &self_location_classes_bin), self_location_classes_list);

      *end = '|';
      start = end + 1;
    }
  }
  self_location_classes_buffer[0] = '\0';
#endif // LOCATION_ENABLED

  // text class contributors for the user

  ErlNifBinary self_text_class_contributors_bin;
  ERL_NIF_TERM self_text_class_contributors_list = enif_make_list(env, 0);

  if (self_text_class_contributors_count > 0) {

    start = self_text_class_contributors_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &self_text_class_contributors_bin);
#else
      ret_value = enif_alloc_binary(env, len, &self_text_class_contributors_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_self_text_class_contributors_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        self_text_class_contributors_bin.data[i] = *(start + i);
      }
      self_text_class_contributors_list = enif_make_list_cell(env, enif_make_binary(env, &self_text_class_contributors_bin), self_text_class_contributors_list);

      *end = '|';
      start = end + 1;
    }
  }
  self_text_class_contributors_buffer[0] = '\0';
  recommendations_buffer[0] = '\0';

/*
  char* start_of_element = NULL;
  char* end_of_element = NULL;
  if (self_text_class_contributors_count > 0) {

    start = self_text_class_contributors_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';

      // keyword
      start_of_element = start;
      end_of_element = strstr(start, ":");
      if (!end_of_element)
        break;
      len = end_of_element - start_of_element;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &self_text_class_contributors_bin);
#else
      ret_value = enif_alloc_binary(env, len, &self_text_class_contributors_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_self_text_class_contributors_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        self_text_class_contributors_bin.data[i] = *(start_of_element + i);
      }
      ERL_NIF_TERM self_text_class_contributor_term = enif_make_binary(env, &self_text_class_contributors_bin);

      // classes in which the above keyword is present
      start_of_element = end_of_element + 1;
      end_of_element = strstr(start, ";");

      ERL_NIF_TERM self_text_classes_list = enif_make_list(env, 0);
      // note this null term which end_of_element is looking for was the earlier "|" set to null
      while (start_of_element && end_of_element && *end_of_element != '\0') {
        end_of_element = strstr(start_of_element, ";");
        if (!end_of_element)
          break;
        *end_of_element = '\0';

        len = end_of_element - start_of_element;
#ifdef ERLANG_R14B02 
        ret_value = enif_alloc_binary(len, &self_text_class_bin);
#else
        ret_value = enif_alloc_binary(env, len, &self_text_class_bin);
#endif
        if (ret_value < 0) {
#ifndef PROFILE_DEBUG
          return enif_make_atom(env, "error");
#else
          return enif_make_atom(env, "error_self_text_class_bin_alloc");
#endif
        }
        for (i=0; i<len; i++) {
          self_text_class_bin.data[i] = *(start_of_element + i);
        }
        self_text_classes_list = enif_make_list_cell(env, enif_make_binary(env, &self_text_class_bin), self_text_classes_list);
        *end_of_element = ';';
        start_of_element = end_of_element + 1;
      }

      ERL_NIF_TERM self_text_class_contributors_2tuple = enif_make_tuple2(env,
                                                       self_text_class_contributor_term,
                                                       self_text_classes_list);

      self_text_class_contributors_list = enif_make_list_cell(env,
                                                         self_text_class_contributors_2tuple,
                                                         self_text_class_contributors_list);

      *end = '|';
      start = end + 1;
    }
  }
  start_of_element = NULL;
  end_of_element = NULL;
*/

  start = NULL;
  end = NULL;

  if (strcmp(input_type_buffer, "handle") == 0) {
    return enif_make_tuple5(env, locations_list,
                          self_languages_list,
                          self_text_classes_list,
                          self_location_classes_list,
                          self_text_class_contributors_list);
  }

  // languages by the user

  ErlNifBinary others_languages_bin;
  ERL_NIF_TERM others_languages_list = enif_make_list(env, 0);

  if (others_languages_count > 0) {

    start = others_languages_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &others_languages_bin);
#else
      ret_value = enif_alloc_binary(env, len, &others_languages_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_others_languages_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        others_languages_bin.data[i] = *(start + i);
      }
      others_languages_list = enif_make_list_cell(env, enif_make_binary(env, &others_languages_bin), others_languages_list);

      *end = '|';
      start = end + 1;
    }
  }
  others_languages_buffer[0] = '\0';

  // text class for the user

  ErlNifBinary others_text_class_bin;
  ERL_NIF_TERM others_text_classes_list = enif_make_list(env, 0);

  ErlNifBinary others_text_class_label_bin;
  ERL_NIF_TERM others_text_class_labels_list = enif_make_list(env, 0);

  if (others_text_classes_count > 0) {

    start = others_text_classes_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';

      middle = strstr(start, " ");
      if (!middle)
        break;
      *middle = '\0';
      len = middle - start;

#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &others_text_class_label_bin);
#else
      ret_value = enif_alloc_binary(env, len, &others_text_class_label_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_others_text_class_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        others_text_class_label_bin.data[i] = *(start + i);
      }
      others_text_class_labels_list = enif_make_list_cell(env, enif_make_binary(env, &others_text_class_label_bin), others_text_class_labels_list);
      *middle = ' ';
      start = middle + 1;

      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &others_text_class_bin);
#else
      ret_value = enif_alloc_binary(env, len, &others_text_class_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_others_text_class_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        others_text_class_bin.data[i] = *(start + i);
      }
      others_text_classes_list = enif_make_list_cell(env, enif_make_binary(env, &others_text_class_bin), others_text_classes_list);

      *end = '|';
      start = end + 1;
    }
  }
  others_text_classes_buffer[0] = '\0';

  // sub class for the user

  ErlNifBinary others_location_classes_bin;
  ERL_NIF_TERM others_location_classes_list = enif_make_list(env, 0);

#ifdef LOCATION_ENABLED
  if (others_location_classes_count > 0) {

    start = others_location_classes_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &others_location_classes_bin);
#else
      ret_value = enif_alloc_binary(env, len, &others_location_classes_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_others_location_classes_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        others_location_classes_bin.data[i] = *(start + i);
      }
      others_location_classes_list = enif_make_list_cell(env, enif_make_binary(env, &others_location_classes_bin), others_location_classes_list);

      *end = '|';
      start = end + 1;
    }
  }
  others_location_classes_buffer[0] = '\0';
#endif // LOCATION_ENABLED

  // text class contributors for others

  ErlNifBinary others_text_class_contributors_bin;
  ERL_NIF_TERM others_text_class_contributors_list = enif_make_list(env, 0);

  if (others_text_class_contributors_count > 0) {

    start = others_text_class_contributors_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';
      len = end - start;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &others_text_class_contributors_bin);
#else
      ret_value = enif_alloc_binary(env, len, &others_text_class_contributors_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_others_text_class_contributors_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        others_text_class_contributors_bin.data[i] = *(start + i);
      }
      others_text_class_contributors_list = enif_make_list_cell(env, enif_make_binary(env, &others_text_class_contributors_bin), others_text_class_contributors_list);

      *end = '|';
      start = end + 1;
    }
  }
  others_text_class_contributors_buffer[0] = '\0';

  /*
  start_of_element = NULL;
  end_of_element = NULL;
  if (others_text_class_contributors_count > 0) {

    start = others_text_class_contributors_buffer;
    end = strstr(start, "|");

    while (start && end && *end != '\0') {
      end = strstr(start, "|");
      if (!end)
        break;
      *end = '\0';

      // keyword
      start_of_element = start;
      end_of_element = strstr(start, ":");
      if (!end_of_element)
        break;
      len = end_of_element - start_of_element;
#ifdef ERLANG_R14B02 
      ret_value = enif_alloc_binary(len, &others_text_class_contributors_bin);
#else
      ret_value = enif_alloc_binary(env, len, &others_text_class_contributors_bin);
#endif
      if (ret_value < 0) {
#ifndef PROFILE_DEBUG
        return enif_make_atom(env, "error");
#else
        return enif_make_atom(env, "error_others_text_class_contributors_bin_alloc");
#endif
      }
      for (i=0; i<len; i++) {
        others_text_class_contributors_bin.data[i] = *(start_of_element + i);
      }
      ERL_NIF_TERM others_text_class_contributor_term = enif_make_binary(env, &others_text_class_contributors_bin);

      // classes in which the above keyword is present
      start_of_element = end_of_element + 1;
      end_of_element = strstr(start, ";");

      ERL_NIF_TERM others_text_classes_list = enif_make_list(env, 0);
      // note this null term which end_of_element is looking for was the earlier "|" set to null
      while (start_of_element && end_of_element && *end_of_element != '\0') {
        end_of_element = strstr(start_of_element, ";");
        if (!end_of_element)
          break;
        *end_of_element = '\0';

        len = end_of_element - start_of_element;
#ifdef ERLANG_R14B02 
        ret_value = enif_alloc_binary(len, &others_text_class_bin);
#else
        ret_value = enif_alloc_binary(env, len, &others_text_class_bin);
#endif
        if (ret_value < 0) {
#ifndef PROFILE_DEBUG
          return enif_make_atom(env, "error");
#else
          return enif_make_atom(env, "error_others_text_class_bin_alloc");
#endif
        }
        for (i=0; i<len; i++) {
          others_text_class_bin.data[i] = *(start_of_element + i);
        }
        others_text_classes_list = enif_make_list_cell(env, enif_make_binary(env, &others_text_class_bin), others_text_classes_list);
        *end_of_element = ';';
        start_of_element = end_of_element + 1;
      }

      ERL_NIF_TERM others_text_class_contributors_2tuple = enif_make_tuple2(env,
                                                       others_text_class_contributor_term,
                                                       others_text_classes_list);

      others_text_class_contributors_list = enif_make_list_cell(env,
                                                         others_text_class_contributors_2tuple,
                                                         others_text_class_contributors_list);

      *end = '|';
      start = end + 1;
    }
  }
  start_of_element = NULL;
  end_of_element = NULL;
  */

  start = NULL;
  end = NULL;

  return enif_make_tuple9(env, locations_list,
                          self_languages_list,
                          self_text_class_labels_list,
                          self_text_classes_list,
//                          self_location_classes_list,
                          self_text_class_contributors_list,
                          others_languages_list,
                          others_text_class_labels_list,
                          others_text_classes_list,
//                          others_location_classes_list,
                          others_text_class_contributors_list);

}

ERL_NIF_TERM nif_profile_handles_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  return enif_make_atom(env, "bleh!");
}

ERL_NIF_TERM nif_profile_docs_list(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  return enif_make_atom(env, "bleh!");
}

static ErlNifFunc nif_funcs[] =
{
  {"init_c", 1, nif_init_c},
  {"profile", 2, nif_profile},
};
ERL_NIF_INIT(profiler, nif_funcs, NULL, NULL, NULL, NULL)

