#include "follow_analyser.h"
#include <iostream>
#include <fstream>
#include <ostream>
#include <cstring>
#include "JSON.h"
#include "keywords_manager.h"

namespace inagist_dashboard {

FollowAnalyser::FollowAnalyser() {
  m_follower_maps_dir.clear();
  m_follower_maps_index_file.clear();
}

FollowAnalyser::~FollowAnalyser() {
  m_follower_maps_dir.clear();
  m_follower_maps_index_file.clear();
  m_twitter_searcher.DeInit();
}

int FollowAnalyser::Init(std::string root_dir) {
  m_follower_maps_dir = root_dir;
  m_follower_maps_index_file = root_dir + "/handle_followers_map.txt";
  if (m_twitter_searcher.Init(root_dir) < 0) {
    std::cout << "Error: could not initialize twitter searcher" << std::endl;
    return -1;
  }
  return 0;
}

int FollowAnalyser::ReadFollowers(std::string handle, std::set<std::string> &followers) {
  std::string file_name = m_follower_maps_dir + "/" + handle + "_followers.txt";
  std::ifstream ifs(file_name.c_str());
  if (!ifs) {
    std::cout << "Error: could not read followers set" << std::endl;
    return -1;
  }
  
  std::string temp_handle;
  while (getline(ifs, temp_handle)) {
    followers.insert(temp_handle);
  }
  ifs.close();

  return followers.size();
}

// gets a set keywords
// writes tweets and idf of keywords to files
// uses TwitterSearcher class to get the tweets and the keywords
// uses KeywordsManager class to get the idf of keywords
int FollowAnalyser::GetKeywords(const std::string& handle,
                                const std::string& tweets_file_name,
                                const std::string& keywords_file_name) {

  int ret_value = 0;
  std::set<std::string> keywords_set;
  std::set<std::string> unused_set;
  std::map<std::string, std::string> unused_map_1;
  std::map<std::string, std::string> unused_map_2;
  inagist_trends::KeywordsManager keywords_manager;
  std::string url;

  std::ofstream tweets_file_stream(tweets_file_name.c_str());
  url = std::string("http://search.twitter.com/search.json?q=from:" + handle/* + "&rpp=100"*/);
  if ((ret_value = m_twitter_searcher.Search(url, tweets_file_stream, unused_set, keywords_set, unused_map_1, unused_map_2)) < 0) {
    std::cout << "Error: could not get tweets for " << handle << std::endl;
  } else {
    keywords_manager.PopulateFreqMap(keywords_set);
  }
  tweets_file_stream.close();

  keywords_manager.CalculateIDF(ret_value, keywords_file_name.c_str());

  return ret_value;
}

// gets a set of followers
// writes tweets and idf of keywords to files
// uses TwitterSearcher class to get the tweets and the keywords
// uses KeywordsManager class to get the idf of keywords
int FollowAnalyser::GetKeywordsFromFollowers(const std::set<std::string>& followers,
                                             const std::string& tweets_file_name,
                                             const std::string& keywords_file_name,
                                             const std::string& scripts_tweeters_map_file_name,
                                             const std::string& keywords_tweeters_map_file_name) {

  int ret_value = 0;
  int num_docs = 0;
  std::set<std::string> keywords_set;
  std::set<std::string> unused_set;
  std::set<std::string>::iterator set_iter;
  std::map<std::string, std::string> scripts_tweeters_map;
  std::map<std::string, std::string> keywords_tweeters_map;
  inagist_trends::KeywordsManager keywords_manager;
  std::string url;
  std::map<std::string, std::string>::iterator map_iter;

  std::ofstream tweets_file_stream(tweets_file_name.c_str());
  for (set_iter = followers.begin(); set_iter != followers.end(); set_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from:" + *set_iter/* + "&rpp=100"*/);
    if ((num_docs = m_twitter_searcher.Search(url, tweets_file_stream, unused_set, keywords_set, scripts_tweeters_map, keywords_tweeters_map)) < 0) {
      std::cout << "Error: could not get tweets for " << *set_iter << std::endl;
    } else {
      // TODO (balaji) - this whole keywords manager thingy can be implemented here. will save some pain
      keywords_manager.PopulateFreqMap(keywords_set);
      keywords_set.clear();
      ret_value += num_docs;
    }
    usleep(100000);
  }
  tweets_file_stream.close();

  // write scripts map to file
  std::ofstream scripts_tweeters_map_file_stream(scripts_tweeters_map_file_name.c_str());
  for (map_iter = scripts_tweeters_map.begin(); map_iter != scripts_tweeters_map.end(); map_iter++) {
    scripts_tweeters_map_file_stream << map_iter->first << " = " << map_iter->second << std::endl;
  }
  scripts_tweeters_map_file_stream.close();
  scripts_tweeters_map.clear();

  // write keywords map to file
  std::ofstream keywords_tweeters_map_file_stream(keywords_tweeters_map_file_name.c_str());
  for (map_iter = keywords_tweeters_map.begin(); map_iter != keywords_tweeters_map.end(); map_iter++) {
    keywords_tweeters_map_file_stream << map_iter->first << " = " << map_iter->second << std::endl;
  }
  keywords_tweeters_map_file_stream.close();
  keywords_tweeters_map.clear();

  keywords_manager.CalculateIDF(ret_value, keywords_file_name.c_str());

  return ret_value;
}

int FollowAnalyser::GetKeywordsFromMentions(const std::string& handle,
                                            std::set<std::string>& mentioners,
                                            const std::string& tweets_file_name,
                                            const std::string& keywords_file_name,
                                            const std::string& keywords_tweeters_map_file_name) {

  int ret_value = 0;
  int num_docs = 0;
  std::set<std::string> keywords_set;
  std::map<std::string, std::string> scripts_tweeters_map;
  std::map<std::string, std::string> keywords_tweeters_map;
  inagist_trends::KeywordsManager keywords_manager;
  std::string url;

  std::ofstream tweets_file_stream(tweets_file_name.c_str());
  url = std::string("http://search.twitter.com/search.json?q=from\%3A" + handle + "+-\%40" + handle);
  if ((num_docs = m_twitter_searcher.Search(url, tweets_file_stream, mentioners, keywords_set, scripts_tweeters_map, keywords_tweeters_map)) < 0) {
    std::cout << "Error: could not get tweets for " << handle << std::endl;
  } else {
    // TODO (balaji) - this whole keywords manager thingy can be implemented here. will save some pain
    keywords_manager.PopulateFreqMap(keywords_set);
    keywords_set.clear();
    ret_value += num_docs;
  }
  tweets_file_stream.close();

  // write keywords map to file
  std::map<std::string, std::string>::iterator map_iter;
  std::ofstream keywords_tweeters_map_file_stream(keywords_tweeters_map_file_name.c_str());
  for (map_iter = keywords_tweeters_map.begin(); map_iter != keywords_tweeters_map.end(); map_iter++) {
    keywords_tweeters_map_file_stream << map_iter->first << " = " << map_iter->second << std::endl;
  }
  keywords_tweeters_map_file_stream.close();
  keywords_tweeters_map.clear();

  keywords_manager.CalculateIDF(ret_value, keywords_file_name.c_str());

  return ret_value;
}

int FollowAnalyser::GetFollowers(std::string handle, std::set<std::string>& followers) {
  bool followers_list_exists = false;
  std::ifstream ifs(m_follower_maps_index_file.c_str());
  if (ifs) {
    std::string temp_handle;
    while (getline(ifs, temp_handle)) {
      if (handle.compare(temp_handle) == 0) {
        followers_list_exists = true;
        std::cout << "Follower list already exists for " + handle << std::endl;
        break;
      }
    }
  }
  ifs.close();
  if (followers_list_exists) {
    return ReadFollowers(handle, followers);
  }

  std::string file_name = m_follower_maps_dir + "/" + handle + "_followers.txt";
  int num_followers = 0;
  if ((num_followers = m_twitter_searcher.GetFollowers(handle, followers)) <= 0) {
    if (num_followers < 0)
      std::cout << "Error: could not get followers form TwitterSearcher" << std::endl;
    return -1;
  }

  std::ofstream ofs;
  ofs.open(file_name.c_str());
  if (!ofs) {
    std::set<std::string>::iterator set_iter;
    for (set_iter = followers.begin(); set_iter != followers.end(); set_iter++) {
      ofs << *set_iter << std::endl;
    }
    std::cout << "Error: could not open " << file_name << std::endl;
    return -1;
  } else {
    ofs.close();
  }

  if (num_followers > 0) {
    ofs.open(m_follower_maps_index_file.c_str(), std::ios::app);
    if (!ofs)
      std::cout << "Error: could not open " + m_follower_maps_index_file << std::endl;
    ofs << handle << std::endl;
    ofs.close();
  }
  std::cout << num_followers << "  followers found for " + handle << std::endl;
  return num_followers;
}

}
