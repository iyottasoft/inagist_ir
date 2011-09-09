#include <iostream>
#include <sstream>
#include <cstring>
#include <string>
#include "curl_request_maker.h"
#include "JSON.h"

int main(int argc, char *argv[]) {
  inagist_api::CurlRequestMaker curl_request_maker;

  std::string temp_str;
  std::string reply_message;
  // get top tweets from inagist api
  if (curl_request_maker.GetTopTweets()) {
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
          std::cout << tweet_object["text"]->AsString().c_str() << std::endl;
          std::cout.flush();
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
  } else {
    std::cout << "ERROR: couldn't get top tweets" << std::endl;
  }

  return 0;
}
