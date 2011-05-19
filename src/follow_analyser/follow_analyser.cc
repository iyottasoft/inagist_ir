#include "follow_analyser.h"
#include <iostream>
#include <fstream>
#include <ostream>
#include <cstring>
#include <map>
#include "JSON.h"
#include "keywords_manager.h"

namespace inagist_dashboard {

FollowAnalyser::FollowAnalyser() {
  m_follower_maps_dir.clear();
  m_follower_maps_index_file.clear();
  memset(m_buffer, '\0', MAX_BUFFER_SIZE);
}

FollowAnalyser::~FollowAnalyser() {
  m_follower_maps_dir.clear();
  m_follower_maps_index_file.clear();
  m_twitter_searcher.DeInit();
  memset(m_buffer, '\0', MAX_BUFFER_SIZE);
}

int FollowAnalyser::Init(std::string root_dir) {
  m_follower_maps_dir = root_dir;
  m_follower_maps_index_file = root_dir + "/handle_followers_map.txt";
  if (m_twitter_searcher.Init(root_dir) < 0) {
    std::cout << "ERROR: could not initialize twitter searcher" << std::endl;
    return -1;
  }
  if (m_keytuples_extracter.Init("./data/static_data/stopwords.txt",
                              "./data/static_data/dictionary.txt",
                              "./data/static_data/unsafe_dictionary.txt") < 0) {
    std::cerr << "ERROR: couldn't initialize\n";
    return -1; 
  }
  return 0;
}

int FollowAnalyser::ReadFollowers(std::string handle, std::set<std::string> &followers) {
  std::string file_name = m_follower_maps_dir + "/" + handle + "_followers.txt";
  std::ifstream ifs(file_name.c_str());
  if (!ifs) {
    std::cout << "ERROR: could not read followers set" << std::endl;
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
  std::multimap<std::string, std::string> tweets_map;
  std::string url;

  std::ofstream tweets_file_stream(tweets_file_name.c_str());
  url = std::string("http://search.twitter.com/search.json?q=from:" + handle/* + "&rpp=100"*/);
  if ((ret_value = m_twitter_searcher.Search(url, tweets_file_stream, tweets_map)) < 0) {
    std::cout << "ERROR: could not get tweets for " << handle << std::endl;
    return ret_value;
  }
  tweets_file_stream.close();

  std::multimap<std::string, std::string>::iterator multimap_iter;
  std::string script;
  std::string safe_status;
  for (multimap_iter = tweets_map.begin(); multimap_iter != tweets_map.end(); multimap_iter++) {
    strcpy(m_buffer, (char *) multimap_iter->second.c_str());
    if (m_keytuples_extracter.GetKeywords((char *) m_buffer, safe_status, script, keywords_set) < 0) {
      std::cout << "ERROR: could not get keywords for\n" << m_buffer << std::endl;
    }
  }
  tweets_map.clear();

  inagist_trends::KeywordsManager keywords_manager;
  keywords_manager.PopulateFreqMap(keywords_set);

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
  std::multimap<std::string, std::string> tweets_map;
  std::multimap<std::string, std::string>::iterator multimap_iter;
  std::string script;

  std::ofstream tweets_file_stream(tweets_file_name.c_str());
  for (set_iter = followers.begin(); set_iter != followers.end(); set_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from:" + *set_iter/* + "&rpp=100"*/);
    if ((num_docs = m_twitter_searcher.Search(url, tweets_file_stream, tweets_map)) < 0) {
      std::cout << "ERROR: could not get tweets for " << *set_iter << std::endl;
    } else {
      for (multimap_iter = tweets_map.begin(); multimap_iter != tweets_map.end(); multimap_iter++) {
        strcpy(m_buffer, multimap_iter->second.c_str());
        script = multimap_iter->first;
        m_keytuples_extracter.GetKeywords((char *) m_buffer, script, keywords_set, scripts_tweeters_map, keywords_tweeters_map);
      }
      tweets_map.clear();
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
  std::multimap<std::string, std::string> tweets_map;
  std::set<std::string> keywords_set;
  std::map<std::string, std::string> scripts_tweeters_map;
  std::map<std::string, std::string> keywords_tweeters_map;
  std::string url;
  std::string script;

  std::ofstream tweets_file_stream(tweets_file_name.c_str());
  url = std::string("http://search.twitter.com/search.json?q=to\%3A" + handle);
  if ((num_docs = m_twitter_searcher.Search(url, tweets_file_stream, tweets_map)) < 0) {
    std::cout << "ERROR: could not get tweets for " << handle << std::endl;
  }
  tweets_file_stream.close();
  if (num_docs < 0)
    return -1;
  
  std::multimap<std::string, std::string>::iterator multimap_iter;
  for (multimap_iter = tweets_map.begin(); multimap_iter != tweets_map.end(); multimap_iter++) {
    mentioners.insert(multimap_iter->first);
    strcpy(m_buffer, multimap_iter->second.c_str());
    script = multimap_iter->first;
    m_keytuples_extracter.GetKeywords((char *) m_buffer, script, keywords_set, scripts_tweeters_map, keywords_tweeters_map);
  }
  tweets_map.clear();

  // TODO (balaji) - this whole keywords manager thingy can be implemented here. will save some pain
  inagist_trends::KeywordsManager keywords_manager;
  keywords_manager.PopulateFreqMap(keywords_set);
  keywords_set.clear();

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

int FollowAnalyser::GetFollowers(const std::string& handle, std::set<std::string>& followers) {

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

  int num_followers = GetFollowers(handle, file_name, followers);

  if (num_followers > 0) {
    std::ofstream ofs;
    ofs.open(m_follower_maps_index_file.c_str(), std::ios::app);
    if (!ofs)
      std::cout << "ERROR: could not open " + m_follower_maps_index_file << std::endl;
    ofs << handle << std::endl;
    ofs.close();
  }
  std::cout << num_followers << "  followers found for " + handle << std::endl;

  return num_followers;
}

int FollowAnalyser::GetFollowers(const std::string& handle,
                                 const std::string& output_file_name,
                                 std::set<std::string>& followers) {

  int num_followers = 0;
  if ((num_followers = m_twitter_searcher.GetFollowers(handle, followers)) <= 0) {
    if (num_followers < 0)
      std::cout << "ERROR: could not get followers from TwitterSearcher" << std::endl;
    return -1;
  }

  if (num_followers == 0)
    return 0;

  std::ofstream ofs;
  ofs.open(output_file_name.c_str());
  if (!ofs) {
    std::cout << "ERROR: could not open " << output_file_name << std::endl;
    return -1;
  } else {
    std::set<std::string>::iterator set_iter;
    for (set_iter = followers.begin(); set_iter != followers.end(); set_iter++) {
      ofs << *set_iter << std::endl;
    }
    ofs.close();
  }

  return num_followers;
}

} // namespace inagist dashboard
