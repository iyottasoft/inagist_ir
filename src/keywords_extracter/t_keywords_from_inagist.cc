#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include "JSON.h"
#include "curl_request_maker.h"
#include "keywords_extract.h"
#include "keywords_manager.h"

int main(int argc, char *argv[]) {
  inagist_trends::KeywordsExtract ke;
  if (ke.Init("./data/tweets.txt", "./data/static_data/stopwords.txt", "./data/static_data/dictionary.txt", NULL, "./data/static_data/output.txt") < 0) {
    std::cerr << "ERROR: couldn't initialize\n";
    return -1; 
  }
  inagist_trends::KeywordsManager km;
  inagist_api::CurlRequestMaker curl_request_maker;

  char buffer[1024];
  std::string temp_str;
  std::string reply_message;
  std::set<std::string> keywords_set;
  // get top tweets from inagist api

  bool ret_value;
  if (argc == 2) {
    std::string url = std::string("http://inagist.com/api/v1/get_archived_tweets?userid=") + std::string(argv[1]);
    ret_value = curl_request_maker.GetTweets(url.c_str());
  } else {
    ret_value = curl_request_maker.GetTopTweets();
  }

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
          std::cout << tweet_object["text"]->AsString().c_str() << std::endl;
          std::cout.flush();
          strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
          ke.GetKeywords(buffer, keywords_set);
          ke.PrintKeywords(keywords_set);
          km.PopulateFreqMap(keywords_set);
          keywords_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
        /*
        if (tweet_object.find(L"user") != tweet_object.end() && tweet_object[L"user"]->IsObject()) {
          std::wcout << tweet_object[L"user"]->Stringify().c_str() << std::endl;
          std::wcout.flush();
        }
        */
      }
      km.PrintFreqMap();
      std::cout << "Num Tweets: " << num_docs << std::endl;
      km.CalculateIDF(num_docs);
      km.PrintEntityIDFs();
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  return 0;
}
