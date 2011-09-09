#ifndef _INAGIST_API_H_
#define _INAGIST_API_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <set>

namespace inagist_api {

class InagistAPI {
 public:
  InagistAPI();
  virtual ~InagistAPI();
  static int GetTrendingTweets(const std::string& handle, std::set<std::string>& tweets);
  static int GetArchievedTweets(const std::string& handle, std::set<std::string>& tweets);
  static int GetTweetsFromUrl(const std::string& url, std::set<std::string>& tweets);

 private:

  //DISALLOW_COPY_AND_ASSIGN(InagistAPI);
};

} // inagist_api

#endif // _INAGIST_API_H_
