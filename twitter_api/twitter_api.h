#ifndef _INAGIST_API_TWITTER_API_H_
#define _INAGIST_API_TWITTER_API_H_

#include <set>
#include <string>

namespace inagist_api {

class TwitterAPI {
 public:
  TwitterAPI();
  ~TwitterAPI();
  int GetPublicTimeLine(std::set<std::string>& tweets);
  int GetUserTimeLine(const std::string& user_id, std::set<std::string>& tweets);
};

}

#endif // _INAGIST_API_TWITTER_API_H_
