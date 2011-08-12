#include "twitter_searcher.h"
#include "curl_request_maker.h"

#include <iostream>
#include <fstream>
#include <cstring>
#include "JSON.h"

#ifdef DEBUG
#if DEBUG>0
#define TS_DEBUG DEBUG
#endif
#endif
// #define TS_DEBUG 0

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

int TwitterSearcher::Get100TweetsFromUser(const std::string& user_name, std::set<std::string>& tweets) {
  std::string url = "http://search.twitter.com/search.json?q=from:" + user_name + "&rpp=100";
  int ret_value = 0;
  if ((ret_value = GetTweetsFromSearchUrl(url, tweets)) < 0) {
    std::cout << "Error: could not get tweets for " << user_name << std::endl;
  }

  return ret_value;
}

int TwitterSearcher::GetTweetsFromUser(const std::string& user_name, std::set<std::string>& tweets) {
  std::string url = "http://search.twitter.com/search.json?q=from:" + user_name;
  int ret_value = 0;
  if ((ret_value = GetTweetsFromSearchUrl(url, tweets)) < 0) {
    std::cout << "Error: could not get tweets for " << user_name << std::endl;
  }

  return ret_value;
}

int TwitterSearcher::GetTweetsFromSearchUrl(const std::string& url, std::set<std::string>& tweets) {

#ifdef TS_DEBUG
  if (TS_DEBUG > 1) {
    std::cout << url << std::endl;
  }
#endif // TS_DEBUG
  int num_docs = 0;

  bool ret_value = false;
  CurlRequestMaker curl_request_maker;
  if ((ret_value = curl_request_maker.GetTweets(url.c_str())) == false) {
    std::cout << "ERROR: couldn't get tweets for url: " << url << std::endl;
    return -1;
  }

  if (ret_value) {
    std::string reply_message;
    curl_request_maker.GetLastWebResponse(reply_message);

    if (reply_message.size() <= 0) {
      curl_request_maker.GetLastCurlError(reply_message);
    }

    if (reply_message.size() > 0) {
      JSONValue *json_value = JSON::Parse(reply_message.c_str());
      if (!json_value || (false == json_value->IsObject())) {
        std::cout << "ERROR: JSON::Parse failed for query: " << url << std::endl;
      } else {
#ifdef TS_DEBUG
        if (TS_DEBUG > 2) {
          std::cout << reply_message << std::endl;
        }
#endif // TS_DEBUG
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
#ifdef TS_DEBUG
              if (TS_DEBUG > 2) {
                if (tweet_object.find("from_user") != tweet_object.end() && tweet_object["from_user"]->IsString()) {
                  std::cout << tweet_object["from_user"]->AsString() << ": "; 
                }
              }
#endif // TS_DEBUG
              if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
                tweet = tweet_object["text"]->AsString(); 
                tweets.insert(tweet);
#ifdef TS_DEBUG
                if (TS_DEBUG > 1) {
                  std::cout << tweet << std::endl;
                }
#endif // TS_DEBUG
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
          std::cout << tweet << std::endl;
          delete json_value;
          return num_docs;
        }
      }
      delete json_value;
    }
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
  CurlRequestMaker curl_request_maker;
  ret_value = curl_request_maker.GetTweets(temp_url.c_str());

  if (ret_value) {
    std::string reply_message;
    curl_request_maker.GetLastWebResponse(reply_message);

    if (reply_message.size() <= 0) {
      curl_request_maker.GetLastCurlError(reply_message);
      std::cout << "Twitter Search Error: " << reply_message << std::endl;
    }

    if (reply_message.size() > 0) {
      JSONValue *json_value = JSON::Parse(reply_message.c_str());
      if (!json_value || (false == json_value->IsObject())) {
        std::cout << "ERROR: JSON::Parse failed for query: " << temp_url << std::endl;
      } else {
#ifdef TS_DEBUG
        if (TS_DEBUG > 2) {
          std::cout << reply_message << std::endl;
        }
#endif // TS_DEBUG
        std::string script;
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

int TwitterSearcher::Search(const std::string& query,
                            std::set<std::string>& tweets) {
  std::string url = "http://search.twitter.com/search.json?q='" + query + "'";
  int ret_value = 0;
  if ((ret_value = GetTweetsFromSearchUrl(url, tweets)) < 0) {
    std::cout << "Error: could not get tweets for query: " << query << std::endl;
  }
  return ret_value;
}

}
