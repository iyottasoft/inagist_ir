#include "twitter_searcher.h"

#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <set>
#include "JSON.h"
#include "curl_request_maker.h"
#include "keywords_extract.h"
#include "keywords_manager.h"

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

int TwitterSearcher::Search(std::string url, std::string root_dir, std::string label, std::string time_stamp, std::set<std::string> &tweeters) {

  std::string original_url = url;
  std::string last_search_max_id = m_search_data_map[url];
  if (last_search_max_id.size() > 0)
    url += "&since_id=" + last_search_max_id;


  //std::cout << url << std::endl;

  inagist_trends::KeywordsExtract ke;
  if (ke.Init("./data/static_data/stopwords.txt", "./data/static_data/dictionary.txt") < 0) {
    std::cerr << "ERROR: couldn't initialize\n";
    return -1; 
  }
  inagist_trends::KeywordsManager km;
  inagist_api::CurlRequestMaker curl_request_maker;

  char buffer[1024];
  std::string temp_str;
  std::string reply_message;
  std::set<std::string> keywords_set;
  std::set<std::string> keyphrases_set;
  std::set<std::string> commenters;

  bool ret_value;
  std::set<std::string>::iterator iter;
  std::ofstream ofs;
  std::string file_name;
  int num_docs = 0;
  std::string script;

  ret_value = curl_request_maker.GetTweets(url.c_str());

  if (ret_value) {
    reply_message = "";
    curl_request_maker.GetLastWebResponse(reply_message);
    if (reply_message.size() <= 0) {
      curl_request_maker.GetLastCurlError(reply_message);
    }
    if (reply_message.size() > 0) {
      JSONValue *json_value = JSON::Parse(reply_message.c_str());
      if (!json_value) {
        std::cout << "ERROR: JSON::Parse failed for query: " << url << std::endl;
      } else {
        keywords_set.clear();
        keyphrases_set.clear();
        km.Clear();
        num_docs = 0;
        JSONObject tweet_o = json_value->AsObject();
        JSONArray tweet_array = tweet_o["results"]->AsArray();
        file_name = root_dir + "/tweets_by_" + label + "." + time_stamp + ".txt";
        ofs.open(file_name.c_str(), std::ios_base::app);
        for (unsigned int i=0; i < tweet_array.size(); i++) {
          // don't know if array element shud again be treated as json value
          // but, what the heck. lets put it as value and then get the object
          JSONValue *tweet_value = tweet_array[i];
          if (false == tweet_value->IsObject())
            std::cout << "ERROR: tweet_value is not an object" << std::endl;
          JSONObject tweet_object = tweet_value->AsObject();

          // now lets work on the json object thus obtained
          if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
            //std::cout << tweet_object["text"]->AsString().c_str() << std::endl;
            //std::cout.flush();
            strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
            ofs << buffer << std::endl;
            ke.GetKeywords(buffer, script, keywords_set, keyphrases_set);
            km.PopulateFreqMap(keywords_set);
            //km.PopulateFreqMap(keyphrases_set);
            keywords_set.clear();
            keyphrases_set.clear();
            memset(buffer, 0, 1024);
            ++num_docs;
          }
          if (tweet_object.find("from_user") != tweet_object.end() && tweet_object["from_user"]->IsString()) {
            tweeters.insert(tweet_object["from_user"]->AsString());
          }
        }
        ofs.close();

        last_search_max_id = "";
        if (tweet_o.find("max_id") != tweet_o.end() && tweet_o["max_id"]->IsString()) {
          last_search_max_id = tweet_o["max_id"]->AsString();
        } else {
          std::cout << "max id field not found in twitter response\n";
        }
        if (last_search_max_id.size() <= 0)
          std::cout << "max id value is empty\n";
        m_search_data_map[original_url] = last_search_max_id;

        file_name = root_dir + "/keywords_by_" + label + "." + time_stamp + ".txt";
        km.CalculateIDF(num_docs, file_name.c_str());
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
