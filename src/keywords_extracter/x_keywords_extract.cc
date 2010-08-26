#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include "JSON.h"
#include "curl_request_maker.h"
#include "keywords_extract.h"
#include "keywords_manager.h"

int main(int argc, char *argv[]) {

  if (argc != 5) {
    std::cout << "usage: " << argv[0] << " <stopwords file> <dictionary file> <username> <keywords output file>\n";
    exit(0);
  }

  inagist_trends::KeywordsExtract ke;
  if (ke.Init(argv[1], argv[2], NULL, "./data/tweets.txt", argv[4]) < 0) {
    std::cerr << "ERROR: couldn't initialize KeywordsExtract class\n";
    return -1; 
  }

  inagist_trends::KeywordsManager km;
  inagist_api::CurlRequestMaker curl_request_maker;

  char buffer[1024];
  std::string temp_str;
  std::string reply_message;
  std::set<std::string> keywords_set;
  // get top tweets from inagist api

  std::string url = std::string("http://inagist.com/api/v1/get_archived_tweets?userid=") + std::string(argv[3]);
  //bool ret_value = curl_request_maker.GetArchievedTweets();
  bool ret_value = curl_request_maker.GetTweets(url.c_str());
  if (ret_value) {
    curl_request_maker.GetLastWebResponse(reply_message);
    // the response is in json format
    JSONValue *json_value = JSON::Parse(reply_message.c_str());
    if (!json_value) {
      std::cout << "ERROR: JSON::Parse failed\n";
    } else {
      unsigned int num_docs = 0;
      // to be specific, the response is a json array
      JSONArray tweet_array = json_value->AsArray();

      for (unsigned int i=0; i < tweet_array.size(); i++) {
        // don't know if array element shud again be treated as json value
        // but, what the heck. lets put it as value and then get the object
        JSONValue *tweet_value = tweet_array[i];
        if (false == tweet_value->IsObject())
          std::cout << "ERROR: tweet_value is not an object" << std::endl;
        JSONObject tweet_object = tweet_value->AsObject();

        // now lets work on the json object thus obtained
        if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
          strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
          ke.GetKeywords(buffer, keywords_set);
          km.PopulateFreqMap(keywords_set);
          keywords_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
      }
      km.CalculateIDF(num_docs, argv[4]);
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get live tweets" << std::endl;
  }

  return 0;
}
