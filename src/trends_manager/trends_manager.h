/* trends_manager.h */

#ifndef _INAGIST_TRENDS_MANAGER_H_
#define _INAGIST_TRENDS_MANAGER_H_

#include <string>
#include "keytuples_extracter.h"
#include "keywords_manager.h"

namespace inagist_trends {

class TrendsManager {
 public:
  TrendsManager();
  ~TrendsManager();
  int Init(const char* stopwords_file_path,
           const char* dictionary_file_path,
           const char* unsafe_dictionary_file_path,
           const char* lang_detect_config_file_path,
           const char* channels_dictionary_file_path);
  int GetKeyTuples(const std::string& text, std::string& safe_status,
                  std::string& script, std::string& lang, std::set<std::string>& keywords,
                  std::set<std::string>& keyphrases, std::set<std::string>& hashtags);
  int GetKeyTuples(const unsigned char* tweet, const unsigned int tweet_len,
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
              char* buffer4, const unsigned int buffer4_len);
 private:
  KeyTuplesExtracter m_keytuples_extracter;
  KeywordsManager m_keywords_manager;
  char m_buffer[1024];
};

} // namespace inagist_trends

#endif // _INAGIST_TRENDS_MANAGER_H_
