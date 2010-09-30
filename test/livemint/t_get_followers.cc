#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <set>
#include "JSON.h"
#include "curl_request_maker.h"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: %s <user_name>\n", argv[0]);
    return -1;
  }

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string temp_str;
  std::string reply_message;
  std::set<std::string> followers;
  std::string cursor = "-1";

  bool ret_value = true;
  while (ret_value) {
    std::string url = "http://twitter.com/statuses/followers/" + std::string(argv[1]) + ".json?cursor=" + cursor;
    ret_value = curl_request_maker.GetTweets(url.c_str());
  
    if (ret_value) {
      curl_request_maker.GetLastWebResponse(reply_message);
      // the response is in json format
      JSONValue *json_value = JSON::Parse(reply_message.c_str());
      if (!json_value) {
        break;
      } else {
        // to be specific, the response is a json array
        JSONObject t_o = json_value->AsObject(); 
        JSONArray tweet_array = t_o["users"]->AsArray();
        JSONObject tweet_object;
        for (unsigned int i=0; i < tweet_array.size(); i++) {
          // don't know if array element shud again be treated as json value
          // but, what the heck. lets put it as value and then get the object
          JSONValue *tweet_value = tweet_array[i];
          if (false == tweet_value->IsObject())
            std::cout << "ERROR: tweet_value is not an object" << std::endl;
          tweet_object = tweet_value->AsObject();
  
          // now lets work on the json object thus obtained
          if (tweet_object.find("screen_name") != tweet_object.end() && tweet_object["screen_name"]->IsString()) {
            std::cout << tweet_object["screen_name"]->AsString().c_str() << std::endl;
          }
        }
        if (t_o.find("next_cursor_str") != t_o.end() && t_o["next_cursor_str"]->IsString()) {
          cursor = t_o["next_cursor_str"]->AsString();
          if (cursor.compare("0") == 0)
            break;
        } else {
          break;
        }
      }
      delete json_value;
    }
  }

  return 0;
}
