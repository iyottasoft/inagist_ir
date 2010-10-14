#include "follow_analyser.h"
#include <cstdio>

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: %s <user_name>\n", argv[0]);
    return -1;
  }

  inagist_dashboard::FollowAnalyser fa;
  std::set<std::string> followers;
  int num_followers = 0;
  if ((num_followers = fa.GetFollowers(argv[1], followers)) < 0) {
    std::cout << "ERROR: could not get followers\n" << std::endl;
  } else {
    std::cout << num_followers << std::endl;
  }

  return 0;
}
