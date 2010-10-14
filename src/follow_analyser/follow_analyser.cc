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
}

int FollowAnalyser::Init(std::string root_dir) {
  m_follower_maps_dir = root_dir;
  m_follower_maps_index_file = root_dir + "/handle_followers_map.txt";
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

// gets a set of followers
// writes tweets and idf of keywords to files
// uses TwitterSearcher class to get the tweets and the keywords
// uses KeywordsManager class to get the idf of keywords
int FollowAnalyser::GetKeywordsFromFollowers(inagist_dashboard::TwitterSearcher* twitter_searcher,
                                             const std::set<std::string>& followers,
                                             const std::string& tweets_file_name,
                                             const std::string& keywords_file_name) {

  int ret_value = 0;
  int num_docs = 0;
  std::set<std::string> keywords_set;
  std::set<std::string> unused_set;
  std::set<std::string>::iterator iter;
  inagist_trends::KeywordsManager keywords_manager;
  std::string url;

  std::ofstream tweets_file_stream(tweets_file_name.c_str());
  for (iter = followers.begin(); iter != followers.end(); iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from:" + *iter/* + "&rpp=100"*/);
    if ((num_docs = twitter_searcher->Search(url, tweets_file_stream, unused_set, keywords_set)) < 0) {
      std::cout << "Error: could not get tweets for " << *iter << std::endl;
    } else {
      keywords_manager.PopulateFreqMap(keywords_set);
      ret_value += num_docs;
    }
  }
  tweets_file_stream.close();

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

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string temp_str;
  std::string reply_message;
  std::string cursor = "-1";
  int num_followers = 0;
  std::ofstream ofs;

  bool ret_value = true;
  std::string file_name = m_follower_maps_dir + "/" + handle + "_followers.txt";
  ofs.open(file_name.c_str());
  if (!ofs) {
    std::cout << "Error: could not open " << file_name << std::endl;
  } else {
    while (ret_value) {
      std::string url = "http://twitter.com/statuses/followers/" + handle + ".json?cursor=" + cursor;
      ret_value = curl_request_maker.GetTweets(url.c_str());
  
      if (ret_value) {
        curl_request_maker.GetLastWebResponse(reply_message);
        if (reply_message.size() > 0) {
          // the response is in json format
          JSONValue *json_value = JSON::Parse(reply_message.c_str());
          if (!json_value || false == json_value->IsObject()) {
            std::cout << "Error: curl reply not a json object\n";
            break;
          } else {
            // to be specific, the response is a json array
            JSONObject t_o = json_value->AsObject(); 
            if (t_o.find("users") != t_o.end() && t_o["users"]->IsArray()) {
              JSONArray tweet_array = t_o["users"]->AsArray();
              JSONObject tweet_object;
              for (unsigned int i=0; i < tweet_array.size(); i++) {
                num_followers++;
                JSONValue *tweet_value = tweet_array[i];
                if (false == tweet_value->IsObject()) {
                  std::cout << "ERROR: tweet_value is not an object" << std::endl;
                } else {
                  tweet_object = tweet_value->AsObject();
    
                  // now lets work on the json object thus obtained
                  if (tweet_object.find("screen_name") != tweet_object.end() && tweet_object["screen_name"]->IsString()) {
                    followers.insert(tweet_object["screen_name"]->AsString());
                    ofs << tweet_object["screen_name"]->AsString() << std::endl;
                  }
                }
              }
            }
            if (t_o.find("next_cursor_str") != t_o.end() && t_o["next_cursor_str"]->IsString()) {
              cursor = t_o["next_cursor_str"]->AsString();
              if (cursor.compare("0") == 0) {
                break;
              }
            } else {
              std::cout << "could not find next_cursor_str" << std::endl;
              break;
            }
          }
          delete json_value;
        }
      }
    }
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
