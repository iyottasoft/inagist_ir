#ifndef _INAGIST_DASHBOARD_TWITTER_SEARCHER_
#define _INAGIST_DASHBOARD_TWITTER_SEARCHER_

#include <string>
#include <set>
#include <map>
#include "curl_request_maker.h"
#include "keywords_extract.h"

namespace inagist_dashboard {

#define MAX_BUFFER_LEN 540

class TwitterSearcher {
 public:
  TwitterSearcher();
  ~TwitterSearcher();
  int Search(const std::string& url, std::ofstream &tweets_file_stream,
             std::set<std::string> &tweeters_set,
             std::set<std::string> &keywords_set);
  int Init(std::string root_dir);
  int DeInit();
 private:
  std::string m_search_data_dir;
  std::string m_search_data_history_file;
  std::map<std::string, std::string> m_search_data_map;
  inagist_api::CurlRequestMaker m_curl_request_maker;
  inagist_trends::KeywordsExtract m_keywords_extract;
  char m_buffer[MAX_BUFFER_LEN];

};

}
#endif // _INAGIST_DASHBOARD_TWITTER_SEARCER_
