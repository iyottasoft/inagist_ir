#ifndef _INAGIST_API_TWITTER_API_H_
#define _INAGIST_API_TWITTER_API_H_

#include <set>
#include <map>
#include <string>

namespace inagist_api {

class TwitterAPI {

 public:
  TwitterAPI();
  ~TwitterAPI();
  static int GetPublicTimeLine(std::set<std::string>& tweets);
  static int GetUserTimeLine(const std::string& user_name, std::set<std::string>& tweets);
  static int GetListStatuses(const std::string& user_name,
                             const std::string& list_name,
                             std::set<std::string>& tweets);
  static int GetListMembers(const std::string& user_name,
                            const std::string& list_name,
                            std::set<std::string>& members);
  static int GetLists(const std::string& user_name,
                      std::map<std::string, std::string>& list_id_name_map);

  static int GetUserInfo(const std::string& handle, std::string& user_info);

  static int GetUserInfo(const std::string& handle, std::set<std::string>& user_info_tokens);

  static int GetUserInfo(const std::string& handle,
                         std::string& name, std::string& description, std::string& url,
                         std::string& age, std::string& gender, std::string& language,
                         std::string& location, std::string& time_zone,
                         std::string& city, std::string& state, std::string& country,
                         std::string& user_info);

  static int GetUserInfo(const std::string& handle,
                         std::string& name, std::string& description, std::string& url,
                         std::string& age, std::string& gender, std::string& language,
                         std::string& location, std::string& time_zone,
                         std::string& city, std::string& state, std::string& country,
                         std::set<std::string>& user_info_tokens);

  static int GetUserInfo(const std::string& handle,
                         unsigned char* locations_buffer, const unsigned int locations_buffer_len,
                         unsigned int& locations_len, unsigned int& locations_count,
                         std::set<std::string>& user_info_tokens);

  static int GetFollowers(const std::string& handle, std::set<std::string>& followers);

  static int GetFollowerTweets(const std::string& handle,
                        std::set<std::string>& tweets,
                        const unsigned int required_count=15);
};

}

#endif // _INAGIST_API_TWITTER_API_H_
