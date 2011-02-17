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

  if (argc != 3) {
    printf("Usage: %s <root_dir> <handle>\n", argv[0]);
    return -1;
  }

  std::string root_dir = argv[1];
  std::string handle = argv[2];

  inagist_trends::KeywordsExtract ke;
  if (ke.Init("./data/static_data/stopwords.txt",
              "./data/static_data/dictionary.txt",
              "./data/static_data/unsafe_dictionary.txt") < 0) {
    std::cerr << "ERROR: couldn't initialize\n";
    return -1; 
  }
  inagist_trends::KeywordsManager km;
  inagist_api::CurlRequestMaker curl_request_maker;

  char buffer[1024];
  std::string temp_str;
  std::string reply_message;
  std::set<std::string> keywords_set;
  std::set<std::string> hashtags_set;
  std::set<std::string> keyphrases_set;
  std::set<std::string> commenters;

  bool ret_value;
  std::string url;
  std::set<std::string>::iterator iter;
  std::ofstream ofs;
  std::string file_name;
  unsigned int num_docs = 0;
  std::string safe_status;
  std::string script;

  //tweets followed by handle 
/*
  url = std::string("http://search.twitter.com/search.json?q=from:agarwalji+OR+from:livemintuid+OR+from:hootsuite+OR+from:sonyshetty+OR+from:mint_ed+OR+from:_shika+OR+from:pogue+OR+from:mint_lounge+OR+from:sriana+OR+from:medianama+OR+from:yogeshpatel+OR+from:omniprasan+OR+from:davosfeed+OR+from:priyaramani+OR+from:cnbctv18news+OR+from:sidin+OR+from:twitter+OR+from:guykawasaki+OR+from:timoreilly+OR+from:quixotic+OR+from:kamla");
  ret_value = curl_request_maker.GetTweets(url.c_str());

  keywords_set.clear();
  keyphrases_set.clear();
  km.Clear();
  if (ret_value) {
    curl_request_maker.GetLastWebResponse(reply_message);
    // the response is in json format
    JSONValue *json_value = JSON::Parse(reply_message.c_str());
    if (!json_value) {
      std::cout << "ERROR: JSON::Parse failed\n";
    } else {
      num_docs = 0;
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
          ke.GetKeywords(buffer, script, keywords_set, keyphrases_set);
          km.PopulateFreqMap(keywords_set);
          //km.PopulateFreqMap(keyphrases_set);
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
*/

  // tweets by handle
  url = std::string("http://search.twitter.com/search.json?q=from:" +  handle + "&rpp=100");
  ret_value = curl_request_maker.GetTweets(url.c_str());

  keywords_set.clear();
  keyphrases_set.clear();
  km.Clear();
  if (ret_value) {
    curl_request_maker.GetLastWebResponse(reply_message);
    // the response is in json format
    JSONValue *json_value = JSON::Parse(reply_message.c_str());
    if (!json_value) {
      std::cout << "ERROR: JSON::Parse failed\n";
    } else {
      num_docs = 0;
      JSONObject tweet_o = json_value->AsObject();
      JSONArray tweet_array = tweet_o["results"]->AsArray();
      file_name = root_dir + "/tweets_by_" + handle + ".txt";
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
          ke.GetKeywords(buffer, safe_status, script, keywords_set, hashtags_set, keyphrases_set);
          km.PopulateFreqMap(keywords_set);
          //km.PopulateFreqMap(keyphrases_set);
          keywords_set.clear();
          keyphrases_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
        if (tweet_object.find("id") != tweet_object.end() && tweet_object["id"]->IsString()) {
         std::string tweet_id = tweet_object["id"]->Stringify().c_str();
         std::cout << tweet_id << std::endl;
        }
    
      }
      ofs.close();
      file_name = root_dir + "/keywords_by_" + handle + ".txt";
      km.CalculateIDF(num_docs, file_name.c_str());
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

return 0;

  // tweets in response to handle

  url = std::string("http://search.twitter.com/search.json?q=\%40" + handle);
  ret_value = curl_request_maker.GetTweets(url.c_str());

  keywords_set.clear();
  keyphrases_set.clear();
  km.Clear();
  if (ret_value) {
    curl_request_maker.GetLastWebResponse(reply_message);
    // the response is in json format
    JSONValue *json_value = JSON::Parse(reply_message.c_str());
    if (!json_value) {
      std::cout << "ERROR: JSON::Parse failed\n";
    } else {
      num_docs = 0;
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
          ke.GetKeywords(buffer, safe_status, script, keywords_set, hashtags_set, keyphrases_set);
          km.PopulateFreqMap(keywords_set);
          //km.PopulateFreqMap(keyphrases_set);
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

  // tweets by people who respond to the handle

  file_name = root_dir + "/tweets_from_commenters.txt";
  ofs.open(file_name.c_str());
  std::set<std::string>::iterator commenter_iter;
  for (commenter_iter = commenters.begin(); commenter_iter != commenters.end(); commenter_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from\%3A" + *commenter_iter + "+-\%40" + handle);
    ret_value = curl_request_maker.GetTweets(url.c_str());

  keywords_set.clear();
  keyphrases_set.clear();
  km.Clear();
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
          ke.GetKeywords(buffer, script, keywords_set);
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

  // tweets that refer to handle
  std::set<std::string> referers;
  std::string referer;

  url = std::string("http://search.twitter.com/search.json?q=" + handle + "+-from\%3A" + handle + "+-to\%3A" + handle + "+-\%40" + handle);
  ret_value = curl_request_maker.GetTweets(url.c_str());

  keywords_set.clear();
  keyphrases_set.clear();
  km.Clear();
  if (ret_value) {
    curl_request_maker.GetLastWebResponse(reply_message);
    // the response is in json format
    JSONValue *json_value = JSON::Parse(reply_message.c_str());
    if (!json_value) {
      std::cout << "ERROR: JSON::Parse failed\n";
    } else {
      num_docs = 0;
      JSONObject tweet_o = json_value->AsObject();
      JSONArray tweet_array = tweet_o["results"]->AsArray();

      file_name = root_dir + "/tweets_refer_" + handle + ".txt";
      ofs.open(file_name.c_str());
      for (unsigned int i=0; i < tweet_array.size(); i++) {
        JSONValue *tweet_value = tweet_array[i];
        if (false == tweet_value->IsObject())
          std::cout << "ERROR: tweet_value is not an object" << std::endl;
        JSONObject tweet_object = tweet_value->AsObject();

        // now lets work on the json object thus obtained
        if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
          strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
          ke.GetKeywords(buffer, safe_status, script, keywords_set, hashtags_set, keyphrases_set);
          ofs << buffer << std::endl;
          km.PopulateFreqMap(keywords_set);
          //km.PopulateFreqMap(keyphrases_set);
          keywords_set.clear();
          keyphrases_set.clear();
          memset(buffer, 0, 1024);
          ++num_docs;
        }
        if (tweet_object.find("from_user") != tweet_object.end() && tweet_object["from_user"]->IsString()) {
          referer = tweet_object["from_user"]->AsString();
          commenter_iter = commenters.find(referer);
          if (commenter_iter == commenters.end())
            referers.insert(referer);
        }
      }
      ofs.close();
      file_name = root_dir + "/keywords_refer_" + handle + ".txt";
      km.CalculateIDF(num_docs, file_name.c_str());
    }
    delete json_value;
  } else {
    std::cout << "ERROR: couldn't get tweets" << std::endl;
  }

  // tweets by people who respond to the handle

  num_docs = 0;
  file_name = root_dir + "/tweets_from_referers.txt";
  ofs.open(file_name.c_str());
  std::set<std::string>::iterator referer_iter;
  keywords_set.clear();
  keyphrases_set.clear();
  km.Clear();
  for (referer_iter = referers.begin(); referer_iter != referers.end(); referer_iter++) {
    url = std::string("http://search.twitter.com/search.json?q=from\%3A" + *referer_iter + "+-" + handle + "+-from\%3A" + handle + "+-to\%3A" + handle + "+-\%40" + handle);
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
          //std::cout.flush();
          strcpy(buffer, (char *) tweet_object["text"]->Stringify().c_str());
          ke.GetKeywords(buffer, script, keywords_set);
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
