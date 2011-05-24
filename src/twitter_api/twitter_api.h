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
};

}

#endif // _INAGIST_API_TWITTER_API_H_
