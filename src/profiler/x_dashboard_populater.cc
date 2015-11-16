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

  inagist_dashboard::FollowAnalyser fa;
  if (fa.Init(root_dir) < 0)
    return -1;

  int ret_value = 0;
  std::string tweets_file_name = root_dir + "/tweets_by_" + handle + "." + time_stamp + ".txt";
  std::string named_entities_file_name = root_dir + "/named_entities_by_" + handle + "." + time_stamp + ".txt";

  ret_value = fa.GetKeywords(handle, tweets_file_name, named_entities_file_name);
  if (ret_value < 0)
    std::cout << "Error: could not get tweets for " + handle << std::endl;
  else
    std::cout << ret_value << " tweets by " + handle << std::endl;

  // tweets of the followers
  std::set<std::string> followers;
  if (fa.GetFollowers(handle, followers) < 0) {
    std::cout << "Error: could not find " + handle + "'s followers\n";
    return -1;
  }

  tweets_file_name = root_dir + "/tweets_by_" + handle + "_followers." + time_stamp + ".txt";
  named_entities_file_name = root_dir + "/named_entities_by_" + handle + "_followers." + time_stamp + ".txt";
  std::string scripts_tweeters_map_file_name = root_dir + "/scripts_tweeters_map_for_" + handle + "_followers." + time_stamp + ".txt";
  std::string named_entities_tweeters_map_file_name = root_dir + "/named_entities_tweeters_map_for_" + handle + "_followers." + time_stamp + ".txt";

  ret_value = fa.GetKeywordsFromFollowers(followers,
                                          tweets_file_name,
                                          named_entities_file_name,
                                          scripts_tweeters_map_file_name,
                                          named_entities_tweeters_map_file_name);
  if (ret_value < 0)
    std::cout << "Error: could not get named_entities from " << handle << "'s followers\n";
  else
    std::cout << ret_value << " tweets by " + handle + "'s followers\n";
  followers.clear();

  // mentions
  std::set<std::string> mentioners;
  tweets_file_name = root_dir + "/tweets_at_" + handle + "." + time_stamp + ".txt";
  named_entities_file_name = root_dir + "/named_entities_at_" + handle + "." + time_stamp + ".txt";
  named_entities_tweeters_map_file_name = root_dir + "/named_entities_tweeters_map_for_" + handle + "_mentions." + time_stamp + ".txt";

  ret_value = fa.GetKeywordsFromMentions(handle, mentioners, tweets_file_name, named_entities_file_name, named_entities_tweeters_map_file_name);
  if (ret_value < 0)
    std::cout << "Error: could not get tweets at " + handle << std::endl;
  else
    std::cout << ret_value << " tweets at " + handle << std::endl;

  // tweetes of the mentioners
  tweets_file_name = root_dir + "/tweets_by_" + handle + "_mentioners." + time_stamp + ".txt";
  named_entities_file_name = root_dir + "/named_entities_by_" + handle + "_mentioners." + time_stamp + ".txt";
  scripts_tweeters_map_file_name = root_dir + "/scripts_tweeters_map_for_" + handle + "_mentioners." + time_stamp + ".txt";
  named_entities_tweeters_map_file_name = root_dir + "/named_entities_tweeters_map_for_" + handle + "_mentioners." + time_stamp + ".txt";

  ret_value = fa.GetKeywordsFromFollowers(mentioners,
                                          tweets_file_name,
                                          named_entities_file_name,
                                          scripts_tweeters_map_file_name,
                                          named_entities_tweeters_map_file_name);
  if (ret_value < 0)
    std::cout << "Error: could not get named_entities from " << handle << "'s mentioners\n";
  else
    std::cout << ret_value << " tweets by " + handle + "'s mentioners\n";
  mentioners.clear();

  return 0;

/*
  std::string url;
  // tweets followed by handle 
  // url = std::string("http://search.twitter.com/search.json?q=from:agarwalji+OR+from:livemintuid+OR+from:hootsuite+OR+from:sonyshetty+OR+from:mint_ed+OR+from:_shika+OR+from:pogue+OR+from:mint_lounge+OR+from:sriana+OR+from:medianama+OR+from:yogeshpatel+OR+from:omniprasan+OR+from:davosfeed+OR+from:priyaramani+OR+from:cnbctv18news+OR+from:sidin+OR+from:twitter+OR+from:guykawasaki+OR+from:timoreilly+OR+from:quixotic+OR+from:kamla");
  // if ((ret_value = ts.Search(url, root_dir, "followees", tweeters)) < 0)
  //  std::cout << "Error: followees" << std::endl;

  // tweets by handle
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
    std::set<std::string> mentioners;
    if ((ret_value += ts.Search(url, root_dir, handle, time_stamp, mentioners)) < 0)
      std::cout << "Error: mentioners" << std::endl;
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
*/
}
