#include <iostream>
#include <cstring>
#include "keywords_extract.h"
#include "keywords_manager.h"
#include "twitcurl.h"

int main(int argc, char *argv[]) {
  twitCurl twitterObj;

  std::string user_name = "worldnewsgist";
  std::string password = "EspressoAmericano2010";

  if (argc == 2) {
    std::cout << "Enter twitter username\n";
    std::cin >> user_name;
    std::cout << "password?\n";
    std::cin >> password;
  } else {
    twitterObj.setTwitterUsername(user_name);
    twitterObj.setTwitterPassword(password);
  }

  std::string temp_str;
  std::string reply_message;
  if (twitterObj.timelinePublicGet()) {
  //if (twitterObj.timelineFriendsGet()) {
    twitterObj.getLastWebResponse(reply_message);
    char *tweet_start = strstr((char *) reply_message.c_str(), "<text>");
    char *tweet_end = NULL;
    if (tweet_start) {
      tweet_start+=6; 
      tweet_end = strstr(tweet_start, "</text>");
    }
    char buffer[1024];
    inagist_trends::KeywordsExtract ke;
    if (ke.Init("./data/tweets.txt", "./data/static_data/stopwords.txt", "./data/static_data/dictionary.txt", NULL, "./data/output.txt") < 0) {
      std::cerr << "ERROR: couldn't initialize\n";
      return -1;
    }
    inagist_trends::KeywordsManager km;
    std::set<std::string> keywords_set;
    while (tweet_start && tweet_end && tweet_start < tweet_end) {
      temp_str = std::string(tweet_start, tweet_end - tweet_start);
      std::cout << temp_str << std::endl;
      memset(buffer, 0, 1024);
      strcpy(buffer, temp_str.c_str());
      ke.GetKeywords(buffer, keywords_set);
      ke.PrintKeywords(keywords_set);
      km.PopulateFreqMap(keywords_set);
      keywords_set.clear();
      tweet_start = strstr((char *) tweet_end, "<text>");
      if (!tweet_start)
        break;
      tweet_start+=6; 
      tweet_end = strstr((char *) tweet_start, "</text>");
      if (!tweet_end)
        break;
    }
    tweet_start = NULL;
    tweet_end = NULL;
    memset(buffer, 0, 1024);
    km.PrintFreqMap();
    //std::cout << reply_message << std::endl;
  } else {
    std::cout << "ERROR: could not connect to twitter" << std::endl;
  }
  return 0;
}
