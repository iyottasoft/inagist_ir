#include "twitter_searcher.h"

#include <iostream>
#include <fstream>
#include <cstring>
#include "JSON.h"

namespace inagist_api {

TwitterSearcher::TwitterSearcher() {
}

TwitterSearcher::~TwitterSearcher() {
  DeInit();
}

int TwitterSearcher::Init(std::string root_dir) {
  m_search_data_dir = root_dir;
  m_search_data_history_file = root_dir + "/search_data_history.txt";
  std::ifstream ifs(m_search_data_history_file.c_str());
  if (ifs) {
    std::string temp_url;
    std::string last_search_max_id;
    while (getline(ifs, temp_url)) {
      if (getline(ifs, last_search_max_id))
        m_search_data_map[temp_url] = last_search_max_id;
    }
  }
  ifs.close();

  return 0;
}

int TwitterSearcher::DeInit() {
  if (m_search_data_map.empty())
    return 0;
  std::ofstream ofs(m_search_data_history_file.c_str());
  if (ofs) {
    std::map<std::string, std::string>::iterator map_iter;
    for (map_iter = m_search_data_map.begin(); map_iter != m_search_data_map.end(); map_iter++) {
      ofs << map_iter->first << std::endl;
      ofs << map_iter->second << std::endl;
    }
  }
  ofs.close();
  m_search_data_map.clear();
  return 0;
}

int TwitterSearcher::GetTweetsFromUser(const std::string& user_name,
                                       std::set<std::string>& tweets) {
  std::string url = "http://search.twitter.com/search.json?q=from:" + user_name + "&rpp=100";
  int ret_value = 0;
  if ((ret_value = GetTweetsFromSearchUrl(url, tweets)) < 0) {
    std::cout << "Error: could not get tweets for " << user_name << std::endl;
  }

  return ret_value;
}

int TwitterSearcher::GetTweetsFromUsers(const std::set<std::string>& handles,
                                        std::set<std::string>& tweets) {
  std::set<std::string>::iterator set_iter;
  for (set_iter = handles.begin(); set_iter != handles.end(); set_iter++) {
    if (GetTweetsFromUser(*set_iter, tweets) < 0) {
      std::cout << "ERROR: could not get tweets for user " << *set_iter << std::endl;
    }
  }
  return tweets.size();
}

int TwitterSearcher::GetTweetsFromSearchUrl(const std::string& url,
                                            std::set<std::string>& tweets) {

#ifdef DEBUG
  std::cout << url << std::endl;
#endif
  int num_docs = 0;

  bool ret_value;
  ret_value = m_curl_request_maker.GetTweets(url.c_str());

  if (ret_value) {
    std::string reply_message;
    m_curl_request_maker.GetLastWebResponse(reply_message);

    if (reply_message.size() <= 0) {
      m_curl_request_maker.GetLastCurlError(reply_message);
    }

    if (reply_message.size() > 0) {
      JSONValue *json_value = JSON::Parse(reply_message.c_str());
      if (!json_value || (false == json_value->IsObject())) {
        std::cout << "ERROR: JSON::Parse failed for query: " << url << std::endl;
      } else {
        std::string tweet;
        JSONObject tweet_o = json_value->AsObject();
        if (tweet_o.find("results") != tweet_o.end() && tweet_o["results"]->IsArray()) {
          JSONArray tweet_array = tweet_o["results"]->AsArray();
          for (unsigned int i=0; i < tweet_array.size(); i++) {
            JSONValue *tweet_value = tweet_array[i];
            if (false == tweet_value->IsObject()) {
              std::cout << "ERROR: tweet_value is not an object" << std::endl;
            } else {
              std::string tweeter = "unknown";
              JSONObject tweet_object = tweet_value->AsObject();
#ifdef DEBUG
              if (tweet_object.find("from_user") != tweet_object.end() && tweet_object["from_user"]->IsString()) {
                std::cout << tweet_object["from_user"]->AsString() << ": "; 
              }
#endif
              if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
                tweet = tweet_object["text"]->AsString(); 
                tweets.insert(tweet);
#ifdef DEBUG
                std::cout << tweet << std::endl;
#endif
                ++num_docs;
              }
            }
          }
        }

        if (tweet_o.find("max_id_str") != tweet_o.end() && tweet_o["max_id_str"]->IsString()) {
          std::string last_search_max_id = tweet_o["max_id_str"]->AsString();
          if (last_search_max_id.size() <= 0)
            std::cout << "max id value is empty\n";
        } else {
          std::cout << "max id field not found in twitter response\n";
        }
      }
      delete json_value;
    }
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
    return 0;
  }

  return num_docs;
}

int TwitterSearcher::Search(const std::string& url,
                            std::ofstream &tweets_file_stream,
                            std::multimap<std::string, std::string> &tweets_map) {

  std::string temp_url = url;
  std::string last_search_max_id = m_search_data_map[temp_url];
  if (last_search_max_id.size() > 0)
    temp_url += "&since_id=" + last_search_max_id;

  //std::cout << url << std::endl;
  int num_docs = 0;

  bool ret_value;
  ret_value = m_curl_request_maker.GetTweets(temp_url.c_str());

  if (ret_value) {
    std::string reply_message;
    m_curl_request_maker.GetLastWebResponse(reply_message);

    if (reply_message.size() <= 0) {
      m_curl_request_maker.GetLastCurlError(reply_message);
    }

    if (reply_message.size() > 0) {
      JSONValue *json_value = JSON::Parse(reply_message.c_str());
      if (!json_value || (false == json_value->IsObject())) {
        std::cout << "ERROR: JSON::Parse failed for query: " << temp_url << std::endl;
      } else {
        std::string script;
        std::string tweet;
        std::set<std::string> unused_keyphrases_set;
        JSONObject tweet_o = json_value->AsObject();
        if (tweet_o.find("results") != tweet_o.end() && tweet_o["results"]->IsArray()) {
          JSONArray tweet_array = tweet_o["results"]->AsArray();
          for (unsigned int i=0; i < tweet_array.size(); i++) {
            JSONValue *tweet_value = tweet_array[i];
            if (false == tweet_value->IsObject()) {
              std::cout << "ERROR: tweet_value is not an object" << std::endl;
            } else {
              std::string tweeter = "unknown";
              JSONObject tweet_object = tweet_value->AsObject();
              if (tweet_object.find("from_user") != tweet_object.end() && tweet_object["from_user"]->IsString()) {
                tweeter = tweet_object["from_user"]->AsString();
              }
              if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
                tweet = tweet_object["text"]->AsString(); 
                tweets_map.insert(std::pair<std::string, std::string> (tweeter, tweet));
                tweets_file_stream << tweet << std::endl;
                tweets_file_stream.flush();
                ++num_docs;
              }
            }
          }
        }

        last_search_max_id = "";
        if (tweet_o.find("max_id_str") != tweet_o.end() && tweet_o["max_id_str"]->IsString()) {
          last_search_max_id = tweet_o["max_id_str"]->AsString();
        } else {
          std::cout << "max id field not found in twitter response\n";
        }
        if (last_search_max_id.size() <= 0)
          std::cout << "max id value is empty\n";
        m_search_data_map[url] = last_search_max_id;
      }
      delete json_value;
    }
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
    return 0;
  }

  return num_docs;
}

int TwitterSearcher::GetRetweetStatsForUser(const std::string& handle,
                                            const std::string& output_file_name) {

  std::string url = std::string("http://search.twitter.com/search.json?q=from:" + handle + "&rpp=100");

#ifdef DEBUG
  std::cout << url << std::endl;
#endif
  int num_docs = 0;

  bool ret_value;
  ret_value = m_curl_request_maker.GetTweets(url.c_str());

  if (ret_value) {
    std::string reply_message;
    m_curl_request_maker.GetLastWebResponse(reply_message);

    if (reply_message.size() <= 0) {
      m_curl_request_maker.GetLastCurlError(reply_message);
    }

    if (reply_message.size() > 0) {
      std::ofstream ofs(output_file_name.c_str());
      if (!ofs) {
        std::cout << "ERROR: could not open file " << output_file_name << std::endl;
        return -1;
      }
      JSONValue *json_value = JSON::Parse(reply_message.c_str());
      if (!json_value || (false == json_value->IsObject())) {
        std::cout << "ERROR: JSON::Parse failed for query: " << url << std::endl;
      } else {
        std::string tweet;
        std::set<std::string> tweets;
        JSONObject tweet_o = json_value->AsObject();
        if (tweet_o.find("results") != tweet_o.end() && tweet_o["results"]->IsArray()) {
          JSONArray tweet_array = tweet_o["results"]->AsArray();
          for (unsigned int i=0; i < tweet_array.size(); i++) {
            JSONValue *tweet_value = tweet_array[i];
            if (false == tweet_value->IsObject()) {
              std::cout << "ERROR: tweet_value is not an object" << std::endl;
            } else {
              std::string tweeter = "unknown";
              JSONObject tweet_object = tweet_value->AsObject();
#ifdef DEBUG
              if (tweet_object.find("from_user") != tweet_object.end() && tweet_object["from_user"]->IsString()) {
                std::cout << tweet_object["from_user"]->AsString() << ": "; 
              }
#endif
              if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
                tweet = tweet_object["text"]->AsString(); 
                tweets.insert(tweet);
#ifdef DEBUG
                std::cout << tweet << std::endl;
#endif
                ++num_docs;
              }
              if (tweet_object.find("id_str") != tweet_object.end() && tweet_object["id_str"]->IsString()) {
                tweet += " " + tweet_object["id_str"]->AsString(); 
                ofs << tweet << std::endl;
              }
            }
          }
        }

        if (tweet_o.find("max_id_str") != tweet_o.end() && tweet_o["max_id_str"]->IsString()) {
          std::string last_search_max_id = tweet_o["max_id_str"]->AsString();
          if (last_search_max_id.size() <= 0)
            std::cout << "max id value is empty\n";
        } else {
          std::cout << "max id field not found in twitter response\n";
        }
      }
      ofs.close();
      delete json_value;
    }
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
    return 0;
  }

  return num_docs;

}

int TwitterSearcher::GetFollowers(const std::string& handle, std::set<std::string>& followers) {

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string temp_str;
  std::string reply_message;
  std::string cursor = "-1";
  int num_followers = 0;

  bool ret_value = true;
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
  return num_followers;
}

}
