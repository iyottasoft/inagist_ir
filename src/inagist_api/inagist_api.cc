#include "inagist_api.h"
#include <iostream>
#include "JSON.h"
#include "curl_request_maker.h" 

namespace inagist_api {

InagistAPI::InagistAPI() {
}

InagistAPI::~InagistAPI() {
}

int InagistAPI::GetArchievedTweets(const std::string& handle, std::set<std::string>& tweets) {
  std::string url = std::string("http://inagist.com/api/v1/get_archived_tweets?userid=") + handle;
  return GetTweetsFromUrl(url, tweets);
}

int InagistAPI::GetTrendingTweets(const std::string& handle, std::set<std::string>& tweets) {
  std::string url = std::string("http://inagist.com/api/v1/get_top_tweets?limit=1&userid=") + handle;
  return GetTweetsFromUrl(url, tweets);
}

int InagistAPI::GetTweetsFromUrl(const std::string& url, std::set<std::string>& tweets) {

  inagist_api::CurlRequestMaker curl_request_maker;

  if (!curl_request_maker.GetTweets((const char*) url.c_str())) {
    std::cout << "ERROR: couldn't get docs from inagist" << std::endl;
    return -1;
  }

  std::string reply_message;
  int num_docs = 0;
  curl_request_maker.GetLastWebResponse(reply_message);
  // the response is in json format
  JSONValue *json_value = JSON::Parse(reply_message.c_str());
  if (!json_value) {
    std::cout << "ERROR: JSON::Parse failed\n";
  } else {
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
        //std::cout << tweet_object["text"]->AsString().c_str() << std::endl;
        //std::cout.flush();
        tweets.insert(tweet_object["text"]->AsString());
        ++num_docs;
      }
      /*
      if (tweet_object.find(L"user") != tweet_object.end() && tweet_object[L"user"]->IsObject()) {
        std::wcout << tweet_object[L"user"]->Stringify().c_str() << std::endl;
        std::wcout.flush();
      }
      */
    }
  }
  delete json_value;

  return num_docs;
}

} // namespace inagist_api
