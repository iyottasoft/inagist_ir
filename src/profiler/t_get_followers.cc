#include "follow_analyser.h"
#include <cstdio>

int main(int argc, char *argv[]) {

  if (argc != 3) {
    printf("Usage: %s <user_name> <output_file_name>\n", argv[0]);
    return -1;
  }

  std::string user_name = argv[1];
  std::string output_file_name = argv[2];

  inagist_dashboard::FollowAnalyser fa;
  std::set<std::string> followers;
  int num_followers = 0;
  if ((num_followers = fa.GetFollowers(argv[1], output_file_name, followers)) < 0) {
    std::cout << "ERROR: could not get followers\n" << std::endl;
  } else {
    std::cout << num_followers << std::endl;
  }

  if (num_followers == 0) {
    std::cout << "No followers found for user " << user_name << std::endl;
    return 0;
  }

  std::ofstream ofs(output_file_name.c_str());
  if (!ofs) {
    std::cout << "ERROR: could not open file " << output_file_name << std::endl;
    ofs.open("/tmp/followers.txt");
    if (!ofs) {
      std::cout << "ERROR: couldn't even write to a default output file /tmp/followers.txt\n";
      return -1;
    }
  }
  std::set<std::string>::iterator set_iter;
  for (set_iter = followers.begin(); set_iter != followers.end(); set_iter++) {
    ofs << *set_iter << std::endl;
  }
  ofs.close();
  followers.clear();

  return 0;
}
