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

int TrendsManager::Init(const char* keytuples_config_file) {

  if (!keytuples_config_file) {
#ifdef TRENDS_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif
    return -1;
  }

  if (m_keytuples_extracter.Init(keytuples_config_file) < 0) {
#ifdef TRENDS_DEBUG
    std::cerr << "ERROR: could not initialize KeyTuplesExtracter\n";
#endif
    return -1;
  }

  return 0;
}

int TrendsManager::GetTrends(const std::string& text,
                             std::set<std::string>& trends) {

  if (text.length() < 1) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  unsigned char text_buffer[MAX_BUFFER_LEN];
  unsigned int text_buffer_len = MAX_BUFFER_LEN;
  strcpy((char*) text_buffer, text.c_str());
  unsigned int text_len = text.length();

  unsigned char trends_buffer[MAX_BUFFER_LEN];
  memset(trends_buffer, 0, MAX_BUFFER_LEN);
  unsigned int trends_buffer_len = MAX_BUFFER_LEN;
  unsigned int trends_len = 0;
  unsigned int trends_count = 0;

  return GetTrends(text_buffer, text_buffer_len, text_len, trends_buffer, trends_buffer_len, trends_len, trends_count);

}

int TrendsManager::GetTrends(const unsigned char* text_buffer, const unsigned int text_buffer_len,
                  const unsigned int& text_len,
                  unsigned char* trends_buffer, const unsigned int& buffer_len,
                  unsigned int& trends_len, unsigned int& trends_count) {

  if (!text_buffer || text_len <= 1) {
    return -1;
  }

  char safe_status_buffer[10];
  memset(safe_status_buffer, 0, 10);
  unsigned int safe_status_buffer_len = 10;

  char script_buffer[4];
  memset(script_buffer, 0, 4);
  unsigned int script_buffer_len = 4;

  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  memset(keywords_buffer, 0, MAX_BUFFER_LEN);
  unsigned int keywords_buffer_len = MAX_BUFFER_LEN;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;

  unsigned char hashtags_buffer[MAX_BUFFER_LEN];
  memset(hashtags_buffer, 0, MAX_BUFFER_LEN);
  unsigned int hashtags_buffer_len = MAX_BUFFER_LEN;
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;

  unsigned char keyphrases_buffer[MAX_BUFFER_LEN];
  memset(keyphrases_buffer, 0, MAX_BUFFER_LEN);
  unsigned int keyphrases_buffer_len = MAX_BUFFER_LEN;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;

  int ret_value = 0;

  ret_value = m_keytuples_extracter.GetKeyTuples((unsigned char*) text_buffer, text_buffer_len, text_len,
                   safe_status_buffer, safe_status_buffer_len,
                   script_buffer, script_buffer_len,
                   keywords_buffer, keywords_buffer_len, keywords_len, keywords_count,
                   hashtags_buffer, hashtags_buffer_len, hashtags_len, hashtags_count,
                   keyphrases_buffer, keyphrases_buffer_len, keyphrases_len, keyphrases_count);

  if (ret_value <= 0) {
    if (ret_value < 0)
      std::cout << "Error: could not get keywords from KeyTuplesExtracter\n";
    return ret_value;
  }

  keywords_buffer[0] = '\0';
  hashtags_buffer[0] = '\0';
  keyphrases_buffer[0] = '\0';

  return ret_value;
}

// takes a pipe separated list of trends and returns a purged list
int TrendsManager::ProcessTrends(char* trends_buffer,
                                 const unsigned int& trends_buffer_len,
                                 unsigned int& trends_len,
                                 unsigned int& trends_count) {
  return trends_count;
}

} // namespace inagist_trends
