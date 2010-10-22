#include "twitter_searcher.h"

#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <set>
#include "JSON.h"
#include "curl_request_maker.h"

namespace inagist_dashboard {

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

  if (m_keywords_extract.Init("./data/static_data/stopwords.txt", "./data/static_data/dictionary.txt") < 0) {
    std::cerr << "ERROR: couldn't initialize\n";
    return -1; 
  }

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

int TwitterSearcher::Search(const std::string& url,
                            std::ofstream &tweets_file_stream,
                            std::set<std::string> &tweeters_set,
                            std::set<std::string> &keywords_set,
                            std::map<std::string, std::string> &scripts_tweeters_map,
                            std::map<std::string, std::string> &keywords_tweeters_map) {

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
                tweeters_set.insert(tweeter);
              }
              if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
                tweet = tweet_object["text"]->AsString(); 
                tweets_file_stream << tweet << std::endl;
                tweets_file_stream.flush();
                strcpy(m_buffer, (char *) tweet.c_str());
                m_buffer[tweet.size()] = '\0';
                m_keywords_extract.GetKeywords(m_buffer, tweeter, keywords_set, scripts_tweeters_map, keywords_tweeters_map);
                unused_keyphrases_set.clear();
                ++num_docs;
              }
            }
          }
        }

        last_search_max_id = "";
        if (tweet_o.find("max_id") != tweet_o.end() && tweet_o["max_id"]->IsString()) {
          last_search_max_id = tweet_o["max_id"]->AsString();
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

}
