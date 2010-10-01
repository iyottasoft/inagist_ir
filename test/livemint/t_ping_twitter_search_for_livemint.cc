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
  std::string url;
  std::set<std::string>::iterator iter;
  std::ofstream ofs;
  std::string file_name;

  //tweets followed by livemint

  url = std::string("http://search.twitter.com/search.json?q=from:agarwalji+OR+from:livemintuid+OR+from:hootsuite+OR+from:sonyshetty+OR+from:mint_ed+OR+from:_shika+OR+from:pogue+OR+from:mint_lounge+OR+from:sriana+OR+from:medianama+OR+from:yogeshpatel+OR+from:omniprasan+OR+from:davosfeed+OR+from:priyaramani+OR+from:cnbctv18news+OR+from:sidin+OR+from:twitter+OR+from:guykawasaki+OR+from:timoreilly+OR+from:quixotic+OR+from:kamla");
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
      file_name = root_dir + "/tweets_for_live_mint.txt";
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
          ofs << buffer << std::endl;
          ke.GetKeywords(buffer, keywords_set, keyphrases_set);
          km.PopulateFreqMap(keywords_set);
          km.PopulateFreqMap(keyphrases_set);
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
      file_name = root_dir + "/keywords_for_live_mint.txt";
      km.CalculateIDF(num_docs, file_name.c_str());
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  // tweets by livemint
  url = std::string("http://search.twitter.com/search.json?q=from:livemint");
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
      file_name = root_dir + "/tweets_by_live_mint.txt";
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
          ofs << buffer << std::endl;
          ke.GetKeywords(buffer, keywords_set, keyphrases_set);
          km.PopulateFreqMap(keywords_set);
          km.PopulateFreqMap(keyphrases_set);
          keywords_set.clear();
          keyphrases_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
      }
      ofs.close();
      file_name = root_dir + "/keywords_by_live_mint.txt";
      km.CalculateIDF(num_docs, file_name.c_str());
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  // tweets in response to livemint

  url = std::string("http://search.twitter.com/search.json?q=\%40livemint");
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

      file_name = root_dir + "/tweets_in_response.txt";
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
          ofs << buffer << std::endl;
          ke.GetKeywords(buffer, keywords_set, keyphrases_set);
          km.PopulateFreqMap(keywords_set);
          km.PopulateFreqMap(keyphrases_set);
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
      file_name = root_dir + "/keywords_in_response.txt";
      km.CalculateIDF(num_docs, file_name.c_str());
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  // tweets by people who respond to livemint tweeters

  unsigned int num_docs = 0;
  file_name = root_dir + "/tweets_from_commenters.txt";
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
          ofs << buffer << std::endl;
          ke.GetKeywords(buffer, keywords_set);
          km.PopulateFreqMap(keywords_set);
          keywords_set.clear();
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
  file_name = root_dir + "/keywords_from_commenters.txt";
  km.CalculateIDF(num_docs, file_name.c_str());

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

      file_name = root_dir + "/tweets_refer_live_mint.txt";
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
          ofs << buffer << std::endl;
          km.PopulateFreqMap(keywords_set);
          km.PopulateFreqMap(keyphrases_set);
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
      file_name = root_dir + "/keywords_refer_live_mint.txt";
      km.CalculateIDF(num_docs, file_name.c_str());
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  // tweets by people who respond to livemint tweeters

  file_name = root_dir + "/tweets_from_referers.txt";
  ofs.open(file_name.c_str());
  std::set<std::string>::iterator referer_iter;
  for (referer_iter = referers.begin(); referer_iter != referers.end(); referer_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from\%3A" + *referer_iter);
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
          //std::cout.flush();
          strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
          ke.GetKeywords(buffer, keywords_set);
          ofs << buffer << std::endl;
          km.PopulateFreqMap(keywords_set);
          keywords_set.clear();
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
  file_name = root_dir + "/keywords_from_referers.txt";
  km.CalculateIDF(num_docs, file_name.c_str());

  return 0;
}
