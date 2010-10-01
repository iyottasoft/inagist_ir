#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include "curl_request_maker.h"
#include "JSON.h"
#include "utf8.h"

#define MAX_BUFFER_LEN 1024

int main(int argc, char *argv[]) { 
  char buffer[MAX_BUFFER_LEN];
  memset(buffer, '\0', MAX_BUFFER_LEN);

  std::string temp_str;
  std::string reply_message;
  // get top tweets from inagist api
  inagist_api::CurlRequestMaker curl_request_maker;

  bool ret_value;
  if (argc == 2) {
    //std::string url = std::string("http://inagist.com/api/v1/get_archived_tweets?userid=") + std::string(argv[1]);
    //std::string url = std::string("http://inagist.com/api/v1/get_top_tweets?userid=") + std::string(argv[1]) + std::string("&limit=5&ham=24");
    std::string url = std::string("http://inagist.com/api/v1/get_top_tweets?limit=1&userid=") + std::string(argv[1]);
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
          strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
          std::cout << "Output: " << buffer << std::endl;
          int length = strlen(buffer);
          int cp = 0;
          char* ptr = buffer;
          char* ptr1 = NULL;
          while (ptr && *ptr != '\0') {
            ptr1 = ptr;
            cp = utf8::next(ptr, buffer + length);
            if (cp < 0x7F) {
              std::cout << *ptr1;
            } else {
              std::cout << cp;
            }
          }
          memset(buffer, 0, 1024);
          ++num_docs;
        }
        std::cout << std::endl;
      }
      std::cout << "Num Tweets: " << num_docs << std::endl;
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  return 0;
}
