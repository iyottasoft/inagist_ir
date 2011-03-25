/* trends_manager.cc */

#include "trends_manager.h"

#ifdef DEBUG
#if DEBUG>0
#define TRENDS_DEBUG DEBUG
#endif
#endif
//#define TRENDS_DEBUG 3

#include <set>
#include <cstring>
#include <cstdlib>

#define MAX_BUFFER_LEN 1024

namespace inagist_trends {

TrendsManager::TrendsManager() {
}

TrendsManager::~TrendsManager() {
}

int TrendsManager::Init(const char* stopwords_file_path,
         const char* dictionary_file_path,
         const char* unsafe_dictionary_file_path,
         const char* lang_detect_config_file_path,
         const char* channels_dictionary_file_path) {

  if (!stopwords_file_path ||
      !dictionary_file_path ||
      !unsafe_dictionary_file_path ||
      !lang_detect_config_file_path ||
      !channels_dictionary_file_path) {
#ifdef TRENDS_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif
    return -1;
  }

  if (m_keytuples_extracter.Init(stopwords_file_path,
                              dictionary_file_path,
                              unsafe_dictionary_file_path,
                              lang_detect_config_file_path,
                              channels_dictionary_file_path) < 0) {
#ifdef TRENDS_DEBUG
    std::cerr << "ERROR: could not initialize KeyTuplesExtracter\n";
#endif
    return -1;
  }

  return 0;
}

int TrendsManager::GetKeyTuples(const std::string& text, std::string& safe_status,
                  std::string& script, std::string& lang, std::set<std::string>& keywords,
                  std::set<std::string>& hashtags, std::set<std::string>& keyphrases) {

  if (text.length() < 1) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  unsigned char buffer[MAX_BUFFER_LEN];
  memset(buffer, 0, MAX_BUFFER_LEN);
  char safe_status_buffer[10];
  memset(safe_status_buffer, 0, 10);
  char script_buffer[4];
  memset(script_buffer, 0, 4);
  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  memset(keywords_buffer, 0, MAX_BUFFER_LEN);
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned char hashtags_buffer[MAX_BUFFER_LEN];
  memset(hashtags_buffer, 0, MAX_BUFFER_LEN);
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;
  unsigned char keyphrases_buffer[MAX_BUFFER_LEN];
  memset(keyphrases_buffer, 0, MAX_BUFFER_LEN);
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
  char buffer1[4];
  memset(buffer1, 0, 4);
  char buffer2[4];
  memset(buffer2, 0, 4);
  char buffer3[4];
  memset(buffer3, 0, 4);
  char buffer4[4];
  memset(buffer4, 0, 4);

  strcpy((char *) buffer, text.c_str());
  int ret_value = 0;
  if ((ret_value = GetKeyTuples((const unsigned char*) buffer, strlen((char *) buffer),
                safe_status_buffer, 10,
                script_buffer, 4,
                (unsigned char*) keywords_buffer, MAX_BUFFER_LEN,
                &keywords_len, &keywords_count,
                (unsigned char*) hashtags_buffer, MAX_BUFFER_LEN,
                &hashtags_len, &hashtags_count,
                (unsigned char*) keyphrases_buffer, MAX_BUFFER_LEN,
                &keyphrases_len, &keyphrases_count,
                buffer1, 4,
                buffer2, 4,
                buffer3, 4,
                buffer4, 4)) < 0) {
    std::cerr << "ERROR: could not get keywords\n";
  } else {
    safe_status = std::string(safe_status_buffer);
    script = std::string(script_buffer);
    lang = std::string(buffer1);
  }

  return ret_value;
}

// keywords and keyphrases are output parameters
int TrendsManager::GetKeyTuples(const unsigned char* tweet, const unsigned int tweet_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr,
                char* buffer1, const unsigned int buffer1_len,
                char* buffer2, const unsigned int buffer2_len,
                char* buffer3, const unsigned int buffer3_len,
                char* buffer4, const unsigned int buffer4_len) {

#ifdef TRENDS_DEBUG
  std::cout << tweet << std::endl;
#endif

  // this can be global. keeping it local for the time being
  unsigned char buffer[MAX_BUFFER_LEN];
  if (tweet_len > 0 && tweet_len < MAX_BUFFER_LEN) {
    memcpy((char *) buffer, (char *) tweet, tweet_len);
    buffer[tweet_len] = '\0';
  } else {
#ifdef TRENDS_DEBUG
    strcpy(safe_status_buffer, "errST");
    strcpy(script_buffer,"rr");
    strcpy((char *) keywords_buffer, "error_submit_tweet_invalid_len");
    *keywords_len_ptr = strlen((char *) keywords_buffer);
    *keywords_count_ptr = 1;
    strcpy((char *) hashtags_buffer, "error_submit_tweet_invalid_len");
    *hashtags_len_ptr = strlen((char *) hashtags_buffer);
    *hashtags_count_ptr = 1;
    strcpy((char *) keyphrases_buffer, "error_submit_tweet_invalid_len");
    *keyphrases_len_ptr = strlen((char *) keyphrases_buffer);
    *keyphrases_count_ptr = 1;
#endif
    memset(buffer, '\0', MAX_BUFFER_LEN);
    return -1;
  }

  int ret_value = 0;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;

  ret_value = m_keytuples_extracter.GetKeyTuples(buffer, tweet_len,
                   safe_status_buffer, safe_status_buffer_len, script_buffer, script_buffer_len,
                   keywords_buffer, keywords_buffer_len, keywords_len, keywords_count,
                   hashtags_buffer, hashtags_buffer_len, hashtags_len, hashtags_count,
                   keyphrases_buffer, keyphrases_buffer_len, keyphrases_len, keyphrases_count,
                   buffer1, buffer1_len, buffer2, buffer2_len,
                   buffer3, buffer3_len, buffer4, buffer4_len);
  if (ret_value <= 0) {
    if (ret_value < 0 ) {
#ifdef TRENDS_DEBUG
      std::cout << "Error: could not get keywords from KeyTuplesExtracter\n";
      strcpy(safe_status_buffer, "errGK");
      strcpy(script_buffer, "rr");
      return -1;
#else
      *safe_status_buffer = '\0';
      *script_buffer = '\0';
#endif
    }
    *keywords_buffer = '\0';
    *keywords_len_ptr = 0;
    *keywords_count_ptr = 0;
    *hashtags_buffer = '\0';
    *hashtags_len_ptr = 0;
    *hashtags_count_ptr = 0;
    *keyphrases_buffer = '\0';
    *keyphrases_len_ptr = 0;
    *keyphrases_count_ptr = 0;
    *buffer1 = '\0';
    *buffer2 = '\0';
    *buffer3 = '\0';
    *buffer4 = '\0';
  }
  buffer[0] = '\0';
  *keywords_len_ptr = keywords_len;
  *keywords_count_ptr = keywords_count;
  *hashtags_len_ptr = hashtags_len;
  *hashtags_count_ptr = hashtags_count;
  *keyphrases_len_ptr = keyphrases_len;
  *keyphrases_count_ptr = keyphrases_count;

  return ret_value;
}

} // namespace inagist_trends
