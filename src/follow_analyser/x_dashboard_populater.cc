#include "twitter_searcher.h"
#include "follow_analyser.h"
#include <iostream>
#include <string>
#include <cstdio>

int main(int argc, char *argv[]) {

  if (argc != 4) {
    printf("Usage: %s <root_dir> <handle> <time_stamp>\n", argv[0]);
    return -1;
  }

  std::string root_dir = argv[1];
  std::string handle = argv[2];
  std::string time_stamp = argv[3];
  std::string url;
  std::set<std::string> tweeters;
  std::set<std::string>::iterator tweeter_iter;
  int ret_value = 0;

  inagist_dashboard::TwitterSearcher ts;
  if (ts.Init(root_dir) < 0) {
    std::cout << "Error: could not initialize twitter searcher" << std::endl;
    return -1;
  }
  // tweets followed by handle 
  // url = std::string("http://search.twitter.com/search.json?q=from:agarwalji+OR+from:livemintuid+OR+from:hootsuite+OR+from:sonyshetty+OR+from:mint_ed+OR+from:_shika+OR+from:pogue+OR+from:mint_lounge+OR+from:sriana+OR+from:medianama+OR+from:yogeshpatel+OR+from:omniprasan+OR+from:davosfeed+OR+from:priyaramani+OR+from:cnbctv18news+OR+from:sidin+OR+from:twitter+OR+from:guykawasaki+OR+from:timoreilly+OR+from:quixotic+OR+from:kamla");
  // if ((ret_value = ts.Search(url, root_dir, "followees", tweeters)) < 0)
  //  std::cout << "Error: followees" << std::endl;

  // tweets by handle
  tweeters.clear();
  url = std::string("http://search.twitter.com/search.json?q=from:" +  handle/* + "&rpp=100"*/);
  if ((ret_value = ts.Search(url, root_dir, handle, time_stamp, tweeters)) < 0) {
    std::cout << "Error: could not get tweets for " + handle << std::endl;
    return -1;
  }
  std::cout << ret_value << " tweets by " + handle << std::endl;

  // tweets of the followers
  inagist_dashboard::FollowAnalyser fa;
  fa.Init(root_dir);
  if (fa.GetFollowers(handle, tweeters) < 0) {
    std::cout << "Error: could not find " + handle + "'s followers\n";
    return -1;
  }

  ret_value = 0;
  int value = 0;
  for (tweeter_iter = tweeters.begin(); tweeter_iter != tweeters.end(); tweeter_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from:" + *tweeter_iter/* + "&rpp=100"*/);
    std::set<std::string> followers;
    if ((value = ts.Search(url, root_dir, handle + "_followers", time_stamp, followers)) < 0)
      std::cout << "Error: could not get tweets for " + handle + "'s followers" << std::endl;
    else
      ret_value += value;
  }
  std::cout << ret_value << " tweets by " + handle + "'s followers\n";

  return 0;

  // tweets in response to handle
  url = std::string("http://search.twitter.com/search.json?q=\%40" + handle);
  std::cout << "responses" << std::endl;
  tweeters.clear();
  if ((ret_value = ts.Search(url, root_dir, handle, time_stamp, tweeters)) < 0)
    std::cout << "Error: responses" << std::endl;
  else
    std::cout << ret_value << " tweets\n";

  // tweets by people who respond to the handle
  std::cout << "responders" << std::endl;
  ret_value = 0;
  for (tweeter_iter = tweeters.begin(); tweeter_iter != tweeters.end(); tweeter_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from\%3A" + *tweeter_iter + "+-\%40" + handle);
    std::set<std::string> commenters;
    if ((ret_value += ts.Search(url, root_dir, handle, time_stamp, commenters)) < 0)
      std::cout << "Error: commenters" << std::endl;
  }
  std::cout << ret_value << " tweets\n";

  std::cout << "tweets that refer" << std::endl;
  // tweets that refer to handle
  tweeters.clear();
  url = std::string("http://search.twitter.com/search.json?q=" + handle + "+-from\%3A" + handle + "+-to\%3A" + handle + "+-\%40" + handle);
  if ((ret_value = ts.Search(url, root_dir, handle, time_stamp, tweeters)) < 0)
    std::cout << "Error: references" << std::endl;
  else
    std::cout << ret_value << " tweets\n";

  // tweets by people who respond to the handle
  std::cout << "referers" << std::endl;
  ret_value = 0;
  for (tweeter_iter = tweeters.begin(); tweeter_iter != tweeters.end(); tweeter_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from\%3A" + *tweeter_iter + "+-\%40" + handle);
    std::set<std::string> referers;
    if ((ret_value = ts.Search(url, root_dir, handle, time_stamp, referers)) < 0)
      std::cout << "Error: referers" << std::endl;
  }
  std::cout << ret_value << " tweets\n";

  ts.DeInit();

  return 0;
}
