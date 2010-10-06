#include <iostream>
#include <cstring>
#include <string>
#include <set>
#include "keywords_extract.h"
#include "curl_request_maker.h"
#include "JSON.h"
#include "utf8.h"

int main(int argc, char *argv[]) {

  char buffer[1024];
  std::string temp_str;
  std::string script;
  std::string script_temp;
  int script_count = 0;
  std::string reply_message;
  // get top tweets from inagist api
  inagist_api::CurlRequestMaker curl_request_maker;
  inagist_trends::KeywordsExtract ke;

  bool ret_value;
  std::string url; 
  if (argc == 2) {
    url = "http://search.twitter.com/search.json?q=from:" +  std::string(argv[1]);
    ret_value = curl_request_maker.GetTweets(url.c_str());

    if (ret_value) {
      curl_request_maker.GetLastWebResponse(reply_message);
      // the response is in json format
      JSONValue *json_value = JSON::Parse(reply_message.c_str());
      if (!json_value) {
        std::cout << "ERROR: JSON::Parse failed\n";
      } else {
        unsigned int num_docs = 0;
        JSONObject tweet_o = json_value->AsObject();
        JSONArray tweet_array = tweet_o["results"]->AsArray();

        for (unsigned int i=0; i < tweet_array.size(); i++) {
          JSONValue *tweet_value = tweet_array[i];
          if (false == tweet_value->IsObject())
            std::cout << "ERROR: tweet_value is not an object" << std::endl;
          JSONObject tweet_object = tweet_value->AsObject();

          // now lets work on the json object thus obtained
          if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
            script = "en";
            strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
            std::cout << buffer << std::endl;
            int length = strlen(buffer);
            int cp = 0;
            char* ptr = buffer;
            char* ptr1 = NULL;
            while (ptr && *ptr != '\0') {
              ptr1 = ptr;
              try {
                cp = utf8::next(ptr, buffer + length);
                if (cp > 0x7F) {
                  if (ke.DetectScript(cp, script_temp) > 0) {
                    script_count++;
                    script = script_temp;
                  }
                }
              } catch (...) {
                std::cout << "some utfcpp exception" << std::endl;
                ptr++;
              }
            }
            memset(buffer, 0, 1024);
            ++num_docs;
            std::cout << "Script: " << script << std::endl;
          }
          //std::cout << std::endl;
        }
        //std::cout << "Num Tweets: " << num_docs << std::endl;
      }
      delete json_value;
    } else {
      std::cout << "ERROR: couldn't get tweets" << std::endl;
    }
  }
  else {
    url = "http://twitter.com/statuses/public_timeline.json";
    ret_value = curl_request_maker.GetTweets(url.c_str());

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
            script = "en";
            strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
            std::cout << buffer << std::endl;
            int length = strlen(buffer);
            int cp = 0;
            char* ptr = buffer;
            char* ptr1 = NULL;
            while (ptr && *ptr != '\0') {
              ptr1 = ptr;
              try {
                cp = utf8::next(ptr, buffer + length);
                if (cp > 0x7F) {
                  if (ke.DetectScript(cp, script_temp) > 0) {
                    script_count++;
                    script = script_temp;
                  }
                }
              } catch (...) {
                std::cout << "some utfcpp exception" << std::endl;
                ptr++;
              }
            }
            memset(buffer, 0, 1024);
            ++num_docs;
            std::cout << "Script: " << script << std::endl;
          }
          //std::cout << std::endl;
        }
        //std::cout << "Num Tweets: " << num_docs << std::endl;
      }
      delete json_value;
    } else {
      std::cout << "ERROR: couldn't get tweets" << std::endl;
    }
  }
  return 0;
}
