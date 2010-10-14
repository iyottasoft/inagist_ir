#ifndef _INAGIST_DASHBOARD_TWITTER_SEARCHER_
#define _INAGIST_DASHBOARD_TWITTER_SEARCHER_

#include <string>
#include <set>
#include <map>

namespace inagist_dashboard {

class TwitterSearcher {
 public:
  TwitterSearcher();
  ~TwitterSearcher();
  int Search(std::string url, std::string root_dir, std::string handle, std::string time_stamp, std::set<std::string> &tweeters);
  int Init(std::string root_dir);
  int DeInit();
 private:
  std::string m_search_data_dir;
  std::string m_search_data_history_file;
  std::map<std::string, std::string> m_search_data_map;
};

}
#endif // _INAGIST_DASHBOARD_TWITTER_SEARCER_
