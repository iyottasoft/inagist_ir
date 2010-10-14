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

namespace inagist_dashboard {

class FollowAnalyser {
 public:
  FollowAnalyser();
  ~FollowAnalyser();
  int GetFollowers(std::string handle, std::set<std::string> &followers);
  int Init(std::string root_dir);
 private:
  std::string m_follower_maps_dir;
  std::string m_follower_maps_index_file;
  int ReadFollowers(std::string handle, std::set<std::string> &followers);
};

} // inagist_dashboard

#endif // _INAGIST_DASHBOARD_FOLLOW_ANALYSER_H_
