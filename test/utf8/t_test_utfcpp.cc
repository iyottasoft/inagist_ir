#include "utf8.h"
#include <iostream>
#include <set>
#include <fstream>
#include <cassert>
#include <cstring>
#include "twitter_api.h"
#include "test_utfcpp.h"

using namespace std;

int main(int argc, char *argv[]) {

  if (argc > 2) {
    std::cout << "Usage: " << argv[0] << " <handle>\n";
    return -1;
  }

  std::string bin_location = std::string(argv[0]);
  std::string::size_type loc = bin_location.find("bin", 0);
  std::string root_dir;
  if (loc == std::string::npos) {
    std::cout << "ERROR: could not find bin location\n" << std::endl;
    return -1;
  } else {
    root_dir = std::string(bin_location, 0, loc);
  }

  // get top tweets from twitter api
  std::set<std::string> tweets;
  int num_docs = 0;
  if (argc == 1) {
    inagist_api::TwitterAPI tapi;
    if ((num_docs = tapi.GetPublicTimeLine(tweets)) < 0) {
      std::cout << "Error: could not get trending tweets from inagist\n";
      return -1;
    }
  } else {
    std::cout << "this feature has not been implemented yet\n";
    return -1;
  }

  std::set<std::string>::iterator set_iter;
  std::string tweet;
  char text[1024];
  memset(text, '\0', 1024);
  char script_buffer[4];
  memset(script_buffer, '\0', 4);
  std::string script;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    tweet = *set_iter;
    strcpy(text, tweet.c_str());
    test_detect_script(text, 1024, script_buffer, 4);
    std::cout << text << std::endl << script_buffer << std::endl;
  }
  tweets.clear();

/*
  ifstream ifs("ta.txt", ios::in | ios::binary);
  if (ifs.is_open()) {
    cout << "error\n";
  }
  int cp = 0;
  string line;
  string::iterator it;
  while (getline(ifs, line)) {
    it = line.begin();
    while (it != line.end()) {
      string::iterator end = line.end();
      cp = utf8::next(line.begin(), *end);
      cout << cp << endl;
      it += cp;
    }
  }

  char twochars[] = "\xe6\x97\xa5\xd1\x88";
  //char twochars[] = "abcde";
  //char twochars[] = "\x61";
  char* w = twochars;
  int cp = utf8::next(w, twochars + 6);
  std::cout << cp << std::endl;
  //assert (cp == 0x65e5);
  //assert (w == twochars + 3);
  cp = utf8::next(w, twochars + 6);
  std::cout << cp << std::endl;

  return 0;
*/

}
