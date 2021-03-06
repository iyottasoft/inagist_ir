#ifndef _INAGIST_API_TWITTER_SEARCHER_
#define _INAGIST_API_TWITTER_SEARCHER_

#include <string>
#include <set>
#include <map>

namespace inagist_api {

#define MAX_BUFFER_LEN 1024

class TwitterSearcher {
 public:
  TwitterSearcher();
  ~TwitterSearcher();
  int Search(const std::string& url,
             std::ofstream &tweets_file_stream,
             std::multimap<std::string, std::string> &tweets_map);
  int Init(std::string root_dir);
  int DeInit();
  static int GetTweetsFromSearchUrl(const std::string& url, std::set<std::string>& tweets);
  static int GetTweetsFromUser(const std::string& user_name, std::set<std::string>& tweets);
  static int Get100TweetsFromUser(const std::string& user_name, std::set<std::string>& tweets);
  static int Search(const std::string& query, std::set<std::string>& tweets);
 private:
  std::string m_search_data_dir;
  std::string m_search_data_history_file;
  std::map<std::string, std::string> m_search_data_map;
  char m_buffer[MAX_BUFFER_LEN];

};

}
#endif // _INAGIST_API_TWITTER_SEARCER_
