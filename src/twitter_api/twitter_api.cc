#include "twitter_api.h"
#include "curl_request_maker.h"
#include "JSONValue.h"
#include <iostream>

// TODO (balaji) using twitcurl - third party code. this should be done away with
// use our curl_request_maker instead

namespace inagist_api {

TwitterAPI::TwitterAPI() {
}

TwitterAPI::~TwitterAPI() {
}

int TwitterAPI::GetPublicTimeLine(std::set<std::string>& tweets) {
  inagist_api::CurlRequestMaker curl_request_maker;

  int num_docs = 0;

  std::string url = "http://twitter.com/statuses/public_timeline.json";
  std::string reply_message;
  // get top tweets from inagist api
  if (curl_request_maker.GetTweets(url.c_str()) < 0) {
    std::cout << "ERROR: couldn't get top tweets" << std::endl;
  } else {
    curl_request_maker.GetLastWebResponse(reply_message);
    // the response is in json format
    JSONValue *json_value = JSON::Parse(reply_message.c_str());
    if (!json_value) {
      std::cout << "ERROR: JSON::Parse failed\n";
    } else {
      // to be specific, the response is a json array
      JSONArray tweet_array = json_value->AsArray();
      JSONValue *tweet_value;

      for (unsigned int i=0; i < tweet_array.size(); i++) {
        // don't know if array element shud again be treated as json value
        // but, what the heck. lets put it as value and then get the object
        tweet_value = tweet_array[i];
        if (false == tweet_value->IsObject())
          std::cout << "ERROR: tweet_value is not an object" << std::endl;
        JSONObject tweet_object = tweet_value->AsObject();

        // now lets work on the json object thus obtained
        if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
          tweets.insert(tweet_object["text"]->AsString());
          num_docs++;
        }
        //delete tweet_value;
      }
    }
    delete json_value;
  }

  return num_docs;
}

}
