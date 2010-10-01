#include <iostream>
#include <fstream>
#include <cstring>
#include <string>
#include <set>
#include "JSON.h"
#include "curl_request_maker.h"
#include "keywords_extract.h"
#include "keywords_manager.h"

int main(int argc, char *argv[]) {

  if (argc != 2) {
    printf("Usage: %s <root_dir>\n", argv[0]);
    return -1;
  }

  std::string root_dir = argv[1];

  inagist_trends::KeywordsExtract ke;
  if (ke.Init("./data/static_data/stopwords.txt", "./data/static_data/dictionary.txt", NULL, "./data/tweets.txt", "./data/static_data/output.txt") < 0) {
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
  std::string url;
  std::set<std::string>::iterator iter;
  std::ofstream ofs;
  std::string file_name;

  // tweets by livemint

  url = std::string("http://search.twitter.com/search.json?q=from:livemint+OR+from:sidin+OR+from:priyaramani+OR+from:mint_ed");
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
      file_name = root_dir + "/keywords_live_mint.txt";
      ofs.open(file_name.c_str());
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
          ke.GetKeywords(buffer, keywords_set, keyphrases_set);
          for (iter=keywords_set.begin(); iter != keywords_set.end(); iter++)
            ofs << *iter << std::endl;
          for (iter=keyphrases_set.begin(); iter != keyphrases_set.end(); iter++)
            ofs << *iter << std::endl;
          keywords_set.clear();
          keyphrases_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
        if (tweet_object.find("from_user") != tweet_object.end() && tweet_object["from_user"]->IsString()) {
          commenters.insert(tweet_object["from_user"]->AsString());
        }
      }
      ofs.close();
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  // tweets in response to livemint

  url = std::string("http://search.twitter.com/search.json?q=\%40livemint+OR+\%40sidin+OR+\%40priyaramani+OR+\%40mint_ed");
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

      file_name = root_dir + "/keywords_in_response.txt";
      ofs.open(file_name.c_str());
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
          ke.GetKeywords(buffer, keywords_set, keyphrases_set);
          for (iter=keywords_set.begin(); iter != keywords_set.end(); iter++)
            ofs << *iter << std::endl;
          for (iter=keyphrases_set.begin(); iter != keyphrases_set.end(); iter++)
            ofs << *iter << std::endl;
          keywords_set.clear();
          keyphrases_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
        if (tweet_object.find("from_user") != tweet_object.end() && tweet_object["from_user"]->IsString()) {
          commenters.insert(tweet_object["from_user"]->AsString());
        }
      }
      ofs.close();
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  // tweets by people who respond to livemint tweeters

  file_name = root_dir + "/keywords_from_commenters.txt";
  ofs.open(file_name.c_str());
  std::set<std::string>::iterator commenter_iter;
  for (commenter_iter = commenters.begin(); commenter_iter != commenters.end(); commenter_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from\%3A" + *commenter_iter);
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
        // don't know if array element shud again be treated as json value
        // but, what the heck. lets put it as value and then get the object
        JSONValue *tweet_value = tweet_array[i];
        if (false == tweet_value->IsObject())
          std::cout << "ERROR: tweet_value is not an object" << std::endl;
        JSONObject tweet_object = tweet_value->AsObject();

        // now lets work on the json object thus obtained
        if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
          //std::cout << tweet_object["text"]->AsString().c_str() << std::endl;
          strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
          ke.GetKeywords(buffer, keywords_set, keyphrases_set);
          for (iter=keywords_set.begin(); iter != keywords_set.end(); iter++)
            ofs << *iter << std::endl;
          for (iter=keyphrases_set.begin(); iter != keyphrases_set.end(); iter++)
            ofs << *iter << std::endl;
          keywords_set.clear();
          keyphrases_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
      }
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }
  }
  ofs.close();

  // tweets that refer to livemint
  std::set<std::string> referers;

  url = std::string("http://search.twitter.com/search.json?q=livemint+-from:livemint+-to:livemint+-%40livemint");
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

      file_name = root_dir + "/keywords_refer_live_mint.txt";
      ofs.open(file_name.c_str());
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
          ke.GetKeywords(buffer, keywords_set, keyphrases_set);
          for (iter=keywords_set.begin(); iter != keywords_set.end(); iter++)
            ofs << *iter << std::endl;
          for (iter=keyphrases_set.begin(); iter != keyphrases_set.end(); iter++)
            ofs << *iter << std::endl;
          keywords_set.clear();
          keyphrases_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
        if (tweet_object.find("from_user") != tweet_object.end() && tweet_object["from_user"]->IsString()) {
          referers.insert(tweet_object["from_user"]->AsString());
        }
      }
      ofs.close();
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  // tweets by people who respond to livemint tweeters

  file_name = root_dir + "/keywords_from_referers.txt";
  ofs.open(file_name.c_str());
  std::set<std::string>::iterator referer_iter;
  for (referer_iter = referers.begin(); referer_iter != referers.end(); referer_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from\%3A" + *referer_iter);
    ret_value = curl_request_maker.GetTweets(url.c_str());

  if (ret_value) {
    std::cout << ret_value << std::endl;
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
          ke.GetKeywords(buffer, keywords_set, keyphrases_set);
          for (iter=keywords_set.begin(); iter != keywords_set.end(); iter++)
            ofs << *iter << std::endl;
          for (iter=keyphrases_set.begin(); iter != keyphrases_set.end(); iter++)
            ofs << *iter << std::endl;
          keywords_set.clear();
          keyphrases_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
      }
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }
  }
  ofs.close();

  return 0;
}
