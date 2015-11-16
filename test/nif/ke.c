#include "erl_nif.h"
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

static int my_enif_get_string(ErlNifEnv *env, ERL_NIF_TERM list, char* buf) {
  ERL_NIF_TERM cell, head, tail;
  int val;
  
  while (enif_get_list_cell(env, list, &head, &tail)) {
    if (!enif_get_int(env, head, &val)) {
      return 0;
    }
    *buf = (char)val;
    buf++;
    list = tail; 
  }
  *buf = '\0';
  
  return 1;
}

static ERL_NIF_TERM nif_getkeywords(ErlNifEnv *env, ERL_NIF_TERM tweet) {

  char stopwords[] = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your,#ff,#FF,#fail,follow,follower,followers,tweet,#news,update,rt";

  char *ptr = NULL;
  char *probe = NULL;;
  char current_word_delimiter;
  char prev_word_delimiter;

  unsigned int current_word_len = 0;
  int score = 0;

  char *current_word_start = NULL;
  char *current_word_end = NULL;
  char *prev_word_start = NULL;
  char *prev_word_end = NULL;
  char *next_word_start = NULL;

  char *caps_entity_start = NULL;
  char *caps_entity_end = NULL;
  char *stopwords_entity_start = NULL;
  char *stopwords_entity_end = NULL;

  // TODO (balaji) use bit map and masks to reduce comparisons
  //bool sentence_start = true;
  bool current_word_caps = false;
  bool current_word_all_caps = false;
  bool current_word_has_mixed_case = false;
  bool prev_word_caps = false;
  bool prev_word_all_caps = false;
  bool prev_word_has_mixed_case = false;
  bool next_word_caps = false;
  bool next_word_all_caps = false;
  bool invisible_word_before_next = false;

  bool current_word_stop = false;
  bool current_word_dict = false;
  bool prev_word_stop = false;
  bool prev_word_dict = false;

  //bool second_letter_has_caps = false;

  char output[1024];
  char *outptr = output;

  // misc
  char *pch = NULL;
  char ch;

  char str[1024];
  memset(str, 0, 1024);
  //if (enif_get_string(env, tweet, str, 140, ERL_NIF_LATIN1) < 2) {
  if (!my_enif_get_string(env, tweet, str)) {
    printf("ERROR: could not get string from erlang\n");
    return enif_make_string(env, "", ERL_NIF_LATIN1);
  }

  if (!str) {
    return enif_make_string(env, "", ERL_NIF_LATIN1);
  }

  memset(outptr, 0, 1024);

  // go to the first word, ignoring handles and punctuations
  ptr = str;
  while (ptr && (' ' == *ptr || (ispunct(*ptr)/* && '\'' != *ptr*/))) {
    if ('@' == *ptr || !strcmp(ptr, "http://") || !strcmp(ptr, "www.") || !strcmp(ptr, "&#")) {
      while (' ' != *ptr && '\0' != *ptr)
        ptr++;
    } else {
      ptr++;
    }
  }

  if (!ptr || '\0' == *ptr)
    return 0;

  if (isupper(*ptr)) {
    current_word_caps = true;
    current_word_all_caps = true;
  }

  current_word_start = ptr;
  probe = ptr + 1;

  while (ptr && probe && ptr != '\0') {
    if (' ' == *probe || '\0' == *probe || (ispunct(*probe) && /*'\'' != *probe &&*/ '#' != *probe && (strcmp(probe, "&#") != 0))) {
      if (NULL != stopwords_entity_end)
        printf("ERROR: stopwords entity end is not null. did you not write it before?\n");
      if (NULL != caps_entity_end)
        printf("ERROR: caps entity end is not null. did you not write it before?\n");
      // word boundary
      score = 0;

      current_word_delimiter = *probe;
      current_word_end = probe;
      *probe = '\0';

      current_word_len = current_word_end - current_word_start;
      if (current_word_len < 2)
        score-=5;

      if ('#' == *current_word_start) {
        score++;
      }

      if (current_word_all_caps) {
        score--;
        if (current_word_len > 1 && current_word_len < 6) {
          score++;
        } else {
        }
      } else if (current_word_has_mixed_case) {
        score++;
      } else if (current_word_caps) {
        score++;
      }

      // stop words
      //if ((m_stopwords_dictionary.find(ptr) != m_stopwords_dictionary.end())) {
      if (strstr(stopwords, ptr) != NULL) {
        if (!strcmp(ptr, "of")) {
          if (prev_word_start == NULL || !prev_word_caps) {
            current_word_stop = true;
            score--;
          }
        } else {
        current_word_stop = true;
        score--;
        }
      } else {
        current_word_stop = false;
      }

      /*
      // dictionary words
      if (current_word_caps)
        *ptr = tolower(*ptr);
      if ((m_dictionary.find(ptr) != m_dictionary.end())) {
        current_word_dict = true;
        score--;
      } else {
        current_word_dict = false;
      }
      if (current_word_caps)
        *ptr = toupper(*ptr);
      */

      if (score > 0) {
        if ((pch = strstr(ptr, "\'s"))) {
          ch = *pch;
          *pch = '\0';
          /*
          strcpy(outptr, ptr);
          outptr += strlen(ptr);
          strcpy(outptr, ' ');
          outptr++;
          */
          *pch = ch;
        } else {
          /*
          strcpy(outptr, ptr);
          outptr += strlen(ptr);
          strcpy(outptr, ' ');
          outptr++;
          */
        }
      }

      if (prev_word_end)
        *prev_word_end = prev_word_delimiter;

      // if current word is not the known last word, briefly probe to see if next word exists
      if ('\0' != current_word_delimiter) {
        ptr = probe + 1;
        if (!ptr) {
          printf("ERROR: Fatal Exception trying to access unallocated memory space\n");
          exit(-1);
        }

        // find the next position of ptr
        while (' ' == *ptr || (ispunct(*ptr) && /* '\'' != *ptr &&*/ '#' != *ptr)) {
          if ('@' == *ptr || !strcmp(ptr, "http://") || !strcmp(ptr, "www.") || !strcmp(ptr, "&#")) {
            invisible_word_before_next = true;
            score--;
            while (' ' != *ptr && '\0' != *ptr)
              ptr++;
          } else {
            ptr++;
          }
        }

        next_word_start = ptr;

        // after finding the start of next word, probe shud be at the same place as ptr
        probe = ptr;

        if (isupper(*next_word_start)) {
          next_word_caps = true;
          next_word_all_caps = true;
        } else {
          next_word_caps = false;
          next_word_all_caps = false;
        }
      }

      if (NULL == caps_entity_start) {
        if (current_word_caps && !current_word_stop &&
            !current_word_dict &&
            '\0' != *next_word_start &&
            !invisible_word_before_next &&
            current_word_len > 1) {
          if (' ' == current_word_delimiter &&
              ((current_word_end + 1) == next_word_start)) {
            caps_entity_start = current_word_start;
          }
        }
        caps_entity_end = NULL;
      } else {
        if (current_word_stop ||
            !current_word_caps ||
            current_word_dict ||
            current_word_len < 2) {
          if (caps_entity_start != prev_word_start) {
            caps_entity_end = prev_word_end;
          }
          else {
            caps_entity_start = NULL;
            caps_entity_end = NULL;
          }
        } else {
          if (' ' != current_word_delimiter ||
              '\0' == *next_word_start ||
              invisible_word_before_next ||
              ((current_word_end + 1) != next_word_start)) {
            if (caps_entity_start != current_word_start) {
              caps_entity_end = current_word_end;
            }
            else {
              caps_entity_start = NULL;
              caps_entity_end = NULL;
            }
          }
        }
      }

      if (NULL == stopwords_entity_start) {
        if (!current_word_stop &&
            !current_word_dict &&
            '#' != *current_word_start &&
            next_word_start && 
            '\0' != *next_word_start &&
            !invisible_word_before_next &&
            current_word_len > 1) {
          if (' ' == current_word_delimiter &&
              ((current_word_end + 1) == next_word_start)) {
            stopwords_entity_start = current_word_start;
          }
        }
        stopwords_entity_end = NULL;
      } else {
        if (current_word_stop || current_word_dict || '#' == *current_word_start || current_word_len < 2) {
          if (stopwords_entity_start != prev_word_start) {
            stopwords_entity_end = prev_word_end;
          }
          else {
            stopwords_entity_start = NULL;
            stopwords_entity_end = NULL;
          }
        } else {
          if (' ' != current_word_delimiter ||
              (next_word_start && '\0' == *next_word_start) ||
              invisible_word_before_next ||
              (next_word_start && ((current_word_end + 1) != next_word_start))) {
            if (stopwords_entity_start != current_word_start) {
              stopwords_entity_end = current_word_end;
            }
            else {
              stopwords_entity_start = NULL;
              stopwords_entity_end = NULL;
            }
          }
        }
      }

      // write entities
      if (NULL != stopwords_entity_start && NULL != stopwords_entity_end) {
        if (stopwords_entity_start < stopwords_entity_end) {
          if ((caps_entity_start == stopwords_entity_start) && (caps_entity_end == stopwords_entity_end)) {
            stopwords_entity_start = NULL;
            stopwords_entity_end = NULL;
          }
          else {
            strncpy(outptr, stopwords_entity_start, (stopwords_entity_end - stopwords_entity_start));
            outptr += (stopwords_entity_end - stopwords_entity_start);
            strcpy(outptr, ",");
            outptr++;
          }
        } else {
          printf("ERROR: stopwords entity markers are wrong\n");
        }
        stopwords_entity_start = NULL;
        stopwords_entity_end = NULL;
      }

      if (NULL != caps_entity_start && NULL != caps_entity_end) {
        if (caps_entity_start < caps_entity_end) {
          strncpy(outptr, caps_entity_start, (caps_entity_end - caps_entity_start));
          outptr += (caps_entity_end - caps_entity_start);
          strcpy(outptr, ",");
          outptr++;
        } else {
          printf("ERROR: caps entity markers are wrong\n");
        }
        caps_entity_start = NULL;
        caps_entity_end = NULL;
      }

      // exit conditions
      if ('\0' == current_word_delimiter || '\0' == *next_word_start) {
        *current_word_end = current_word_delimiter;
        break;
      }

      // ****************************** beginning of next cycle ******************************* //

      prev_word_end = current_word_end;
      prev_word_start = current_word_start;

      prev_word_caps = current_word_caps;
      prev_word_has_mixed_case = current_word_has_mixed_case;
      prev_word_all_caps = current_word_all_caps;
      prev_word_stop = current_word_stop;
      prev_word_dict = current_word_dict;

      prev_word_delimiter = current_word_delimiter;

      current_word_start = next_word_start;
      current_word_caps = next_word_caps;
      current_word_all_caps = next_word_all_caps;
      current_word_has_mixed_case = false;

      invisible_word_before_next = false;
      next_word_start = NULL;
      next_word_caps = false;
      next_word_all_caps = false;

    } else {
      if (!strcmp(probe, "&#")) {
        while (' ' != *probe && '\0' != *probe)
          probe++;
        if ('\0' == *probe)
          break;
      }
      if (isupper(*probe)) {
        if (!current_word_all_caps) {
          //if ((probe-1) == ptr)
            //second_letter_has_caps = true;
          //else
            current_word_has_mixed_case = true;
        }
      } else {
        if (current_word_caps)
          current_word_has_mixed_case = false;
        current_word_all_caps = false;
      }
    }

    probe++;
  }
  //if (outptr != output && ',' == *(outptr-1))
  //  *(outptr-1) = '\0';
  return enif_make_string(env, output, ERL_NIF_LATIN1);
}

static ERL_NIF_TERM nif_run(ErlNifEnv *env) {
  return nif_getkeywords(env, enif_make_string(env, "Contigous Caps will be counted. So will be contiguous nonstopwords", ERL_NIF_LATIN1));
}

static ErlNifFunc nif_funcs[] =
{
  {"run", 0, nif_run},
};
ERL_NIF_INIT(ke, nif_funcs, NULL, NULL, NULL, NULL)

