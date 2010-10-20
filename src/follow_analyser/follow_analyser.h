#ifndef _INAGIST_DASHBOARD_FOLLOW_ANALYSER_H_
#define _INAGIST_DASHBOARD_FOLLOW_ANALYSER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <iostream>
#include <string>
#include <set>
#include "twitter_searcher.h"
#include "curl_request_maker.h"

// cc includes
// curl_request_maker.h

namespace inagist_dashboard {

class FollowAnalyser {

 public:
  FollowAnalyser();
  ~FollowAnalyser();
  int Init(std::string root_dir);
  int GetFollowers(std::string handle, std::set<std::string> &followers);
  int GetKeywords(inagist_dashboard::TwitterSearcher* twitter_searcher,
                               const std::set<std::string>& followers,
                               const std::string& tweets_file_name,
                               const std::string& keywords_file_name);
  int GetKeywordsFromFollowers(inagist_dashboard::TwitterSearcher* twitter_searcher,
                               const std::set<std::string>& followers,
                               const std::string& tweets_file_name,
                               const std::string& keywords_file_name,
                               const std::string& scripts_tweeters_map_file_name,
                               const std::string& keywords_tweeters_map_file_name);

 private:
  std::string m_follower_maps_dir;
  std::string m_follower_maps_index_file;
  int ReadFollowers(std::string handle, std::set<std::string> &followers);
};

} // inagist_dashboard

#endif // _INAGIST_DASHBOARD_FOLLOW_ANALYSER_H_
