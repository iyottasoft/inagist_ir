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

  int Init(const char* keytuples_config_file);

  int GetTrends(const std::string& text,
                std::set<std::string>& trends);

  int GetTrends(const unsigned char* text_buffer, const unsigned int text_len,
                unsigned char* trends_buffer, const unsigned int& buffer_len,
                unsigned int& trends_len, unsigned int& trends_count);

  int ProcessTrends(char* trends_buffer,
                    const unsigned int& trends_buffer_len,
                    unsigned int& trends_len,
                    unsigned int& trends_count);
 private:
  KeyTuplesExtracter m_keytuples_extracter;
  KeywordsManager m_keywords_manager;
  char m_buffer[1024];
};

} // namespace inagist_trends

#endif // _INAGIST_TRENDS_MANAGER_H_
