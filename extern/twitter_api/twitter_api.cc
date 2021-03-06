#include "twitter_api.h"
#include "curl_request_maker.h"
#include "JSONValue.h"
#include <iostream>

#ifdef DEBUG
#if DEBUG>0
#define TA_DEBUG DEBUG
#endif
#endif
//#define TA_DEBUG 1

namespace inagist_api {

TwitterAPI::TwitterAPI() {
}

TwitterAPI::~TwitterAPI() {
}

int TwitterAPI::GetPublicTimeLine(std::set<std::string>& tweets) {
  inagist_api::CurlRequestMaker curl_request_maker;

  int num_docs = 0;

  std::string curl = "http://api.twitter.com/1/statuses/public_timeline.json?include_entities=1";
  std::string reply_message;
  // get top tweets from inagist api
  if (curl_request_maker.GetTweets(curl.c_str()) < 0) {
    std::cout << "ERROR: couldn't get top tweets" << std::endl;
  } else {
    curl_request_maker.GetLastWebResponse(reply_message);
    // the response is in json format
    JSONValue *json_value = JSON::Parse(reply_message.c_str());
    if (!json_value) {
      std::cout << "ERROR: JSON::Parse failed\n";
    } else {
      JSONObject json_obj = json_value->AsObject();
      if (json_obj.find("error") != json_obj.end() && json_obj["error"]->IsString()) {
        std::string error = json_obj["error"]->AsString();
        std::cout << "Twitter Error: " << error << std::endl;
        delete json_value;
        return -1;
      }

      // to be specific, the response is a json array
      JSONArray tweet_array = json_value->AsArray();
      JSONValue *tweet_value;
      std::string tweet;
      std::string url;
      std::string expanded_url;
      size_t pos = 0;

      for (unsigned int i=0; i < tweet_array.size(); i++) {
        // don't know if array element shud again be treated as json value
        // but, what the heck. lets put it as value and then get the object
        tweet_value = tweet_array[i];
        if (false == tweet_value->IsObject()) {
          std::cout << "ERROR: tweet_value is not an object" << std::endl;
          continue;
        }
        JSONObject tweet_object = tweet_value->AsObject();

        // now lets work on the json object thus obtained
        if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
          // this was easy, found the text of the tweet
          tweet = tweet_object["text"]->AsString();

          // now lets find the urls in the tweet, if any - to replace shortened urls with expanded ones
          if ((tweet_object.find("entities") != tweet_object.end()) && tweet_object["entities"]->IsObject()) {
            JSONObject entities_object = tweet_object["entities"]->AsObject();
            if ((entities_object.find("urls") != entities_object.end()) && entities_object["urls"]->IsArray()) {
              JSONArray url_array = entities_object["urls"]->AsArray();
              for (unsigned int j=0; j < url_array.size(); j++) {
                JSONObject url_object = url_array[j]->AsObject();
                if ((url_object.find("url") != url_object.end()) && url_object["url"]->IsString()) {
                  url = url_object["url"]->AsString();
                }
                if (!url.empty()) {
                  if (url_object.find("expanded_url") != url_object.end() &&
                      url_object["expanded_url"]->IsString()) {
                    expanded_url = url_object["expanded_url"]->AsString();
                  }
                  if ((pos = tweet.find(url)) != std::string::npos) {
                    tweet.replace(pos, url.length(), expanded_url.c_str(), expanded_url.length());
                  }
                }
              }
            }
          }

          tweets.insert(tweet);
          num_docs++;
        }
        //delete tweet_value;
      }
    }
    delete json_value;
  }

  return num_docs;
}

int TwitterAPI::GetTweets(std::string& user_name, std::set<std::string>& tweets, bool expand_urls) {

  if (!expand_urls) {
    return GetUserTimeLine(user_name, tweets);
  }

  inagist_api::CurlRequestMaker curl_request_maker;

  int num_docs = 0;

  std::string curl;
  if (!user_name.empty()) {
    curl = "http://api.twitter.com/1/statuses/user_timeline.json?include_entities=1";
    curl += "&screen_name=" + user_name;
  } else {
    curl = "http://api.twitter.com/1/statuses/public_timeline.json?include_entities=1";
  }

  std::string reply_message;
  std::string url;
  std::string expanded_url;
  std::string real_url;

  // get top tweets from inagist api
  if (curl_request_maker.GetTweets(curl.c_str()) < 0) {
    std::cout << "ERROR: couldn't get top tweets" << std::endl;
  } else {
    curl_request_maker.GetLastWebResponse(reply_message);
    // the response is in json format
    JSONValue *json_value = JSON::Parse(reply_message.c_str());
    if (!json_value) {
      std::cout << "ERROR: JSON::Parse failed\n";
    } else {
      JSONObject json_obj = json_value->AsObject();
      if (json_obj.find("error") != json_obj.end() && json_obj["error"]->IsString()) {
        std::string error = json_obj["error"]->AsString();
        std::cout << "Twitter Error: " << error << std::endl;
        delete json_value;
        return -1;
      }

      // to be specific, the response is a json array
      JSONArray tweet_array = json_value->AsArray();
      JSONValue *tweet_value;
      std::string tweet;
      size_t pos = 0;

      for (unsigned int i=0; i < tweet_array.size(); i++) {
        // don't know if array element shud again be treated as json value
        // but, what the heck. lets put it as value and then get the object
        tweet_value = tweet_array[i];
        if (false == tweet_value->IsObject()) {
          std::cout << "ERROR: tweet_value is not an object" << std::endl;
          continue;
        }
        JSONObject tweet_object = tweet_value->AsObject();

        // now lets work on the json object thus obtained
        if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
          // this was easy, found the text of the tweet
          tweet = tweet_object["text"]->AsString();

          // now lets find the urls in the tweet, if any - to replace shortened urls with expanded ones
          if ((tweet_object.find("entities") != tweet_object.end()) && tweet_object["entities"]->IsObject()) {
            JSONObject entities_object = tweet_object["entities"]->AsObject();
            if ((entities_object.find("urls") != entities_object.end()) && entities_object["urls"]->IsArray()) {
              JSONArray url_array = entities_object["urls"]->AsArray();
              for (unsigned int j=0; j < url_array.size(); j++) {
                JSONObject url_object = url_array[j]->AsObject();
                if ((url_object.find("url") != url_object.end()) && url_object["url"]->IsString()) {
                  url = url_object["url"]->AsString();
                }
                if (!url.empty()) {
                  if (url_object.find("expanded_url") != url_object.end() &&
                      url_object["expanded_url"]->IsString()) {
                    expanded_url = url_object["expanded_url"]->AsString();
                    if (curl_request_maker.GetLongURL(expanded_url, real_url) < 0) {
                      std::cout << "ERROR: couldn't get top tweets" << std::endl;
                    }
                  }
                  if ((pos = tweet.find(url)) != std::string::npos) {
                    tweet.replace(pos, url.length(), real_url.c_str(), real_url.length());
                  }
                }
              }
            }
          }

          tweets.insert(tweet);
          num_docs++;
        }
        //delete tweet_value;
      }
    }
    delete json_value;
  }

  return num_docs;
}

int TwitterAPI::GetUserTimeLine(const std::string& user_name, std::set<std::string>& tweets) {

  if (user_name.empty()) {
    return GetPublicTimeLine(tweets);
  }

  inagist_api::CurlRequestMaker curl_request_maker;

  int num_docs = 0;

  std::string url = "http://api.twitter.com/1/statuses/user_timeline.json?screen_name=" + user_name;
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

      JSONObject json_obj = json_value->AsObject();
      if (json_obj.find("error") != json_obj.end() && json_obj["error"]->IsString()) {
        std::string error = json_obj["error"]->AsString();
        std::cout << "Twitter Error: " << error << std::endl;
        delete json_value;
        return -1;
      }

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

int TwitterAPI::GetLists(const std::string& user_name,
                         std::map<std::string, std::string>& list_id_name_map) {

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string reply_message;
  std::string cursor = "-1";

  bool ret_value = true;
  std::string list_name;
  while (ret_value) {
    std::string url = "http://api.twitter.com/1/screen_name=" + user_name + "/lists.json?cursor=" + cursor;
    ret_value = curl_request_maker.GetTweets(url.c_str());

    if (ret_value) {
      curl_request_maker.GetLastWebResponse(reply_message);
      if (reply_message.size() > 0) {
        // the response is in json format
        JSONValue *json_value = JSON::Parse(reply_message.c_str());
        if (!json_value || false == json_value->IsObject()) {
          std::cout << "Error: curl reply not a json object\n";
          break;
        } else {

          JSONObject json_obj = json_value->AsObject();
          if (json_obj.find("error") != json_obj.end() && json_obj["error"]->IsString()) {
            std::string error = json_obj["error"]->AsString();
            std::cout << "Twitter Error: " << error << std::endl;
            delete json_value;
            return -1;
          }

          // to be specific, the response is a json array
          JSONObject t_o = json_value->AsObject(); 
          if (t_o.find("lists") != t_o.end() && t_o["lists"]->IsArray()) {
            JSONArray list_array = t_o["lists"]->AsArray();
            JSONObject list_object;
            for (unsigned int i=0; i < list_array.size(); i++) {
              JSONValue *list_value = list_array[i];
              if (false == list_value->IsObject()) {
                std::cout << "ERROR: list_value is not an object" << std::endl;
              } else {
                list_object = list_value->AsObject();
                // now lets work on the json object thus obtained
                // first we'll get list name then id. this code will go for a toss, if that is not the case
                if (list_object.find("name") != list_object.end() && list_object["name"]->IsString()) {
                  list_name = list_object["name"]->AsString();
                }
                if (list_object.find("id_str") != list_object.end() && list_object["id_str"]->IsString()) {
                  if (list_name.length() < 1) {
                    std::cerr << "ERROR: invalid list name. json read out of order\n";
                  } else {
                    list_id_name_map.insert(std::pair<std::string, std::string>(list_object["id_str"]->AsString(), list_name));
                    list_name.clear();
                  }
                }
              }
            }
          }
          if (t_o.find("next_cursor_str") != t_o.end() && t_o["next_cursor_str"]->IsString()) {
            cursor = t_o["next_cursor_str"]->AsString();
            if (cursor.compare("0") == 0) {
              break;
            }
          } else {
            std::cout << "could not find next_cursor_str" << std::endl;
            break;
          }
        }
        delete json_value;
      }
    }
  }

  return list_id_name_map.size();

}

int TwitterAPI::GetListMembers(const std::string& user_name,
                                const std::string& list_id,
                                std::set<std::string>& members) {

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string reply_message;
  std::string cursor = "-1";

  bool ret_value = true;
  while (ret_value) {
    std::string url = "http://api.twitter.com/1/" + user_name + "/" \
                      + list_id + "/members.json?cursor=" + cursor;
    ret_value = curl_request_maker.GetTweets(url.c_str());

    if (ret_value) {
      curl_request_maker.GetLastWebResponse(reply_message);
      if (reply_message.size() > 0) {
        // the response is in json format
        JSONValue *json_value = JSON::Parse(reply_message.c_str());
        if (!json_value || false == json_value->IsObject()) {
          std::cout << "Error: curl reply not a json object\n";
          break;
        } else {

          JSONObject json_obj = json_value->AsObject();
          if (json_obj.find("error") != json_obj.end() && json_obj["error"]->IsString()) {
            std::string error = json_obj["error"]->AsString();
            std::cout << "Twitter Error: " << error << std::endl;
            delete json_value;
            return -1;
          }

          // to be specific, the response is a json array
          JSONObject t_o = json_value->AsObject(); 
          if (t_o.find("users") != t_o.end() && t_o["users"]->IsArray()) {
            JSONArray user_array = t_o["users"]->AsArray();
            JSONObject user_object;
            for (unsigned int i=0; i < user_array.size(); i++) {
              JSONValue *user_value = user_array[i];
              if (false == user_value->IsObject()) {
                std::cout << "ERROR: user_value is not an object" << std::endl;
              } else {
                user_object = user_value->AsObject();
                // now lets work on the json object thus obtained
                if (user_object.find("screen_name") != user_object.end() && user_object["screen_name"]->IsString()) {
                  members.insert(user_object["screen_name"]->AsString());
                } else {
                  std::cerr << "ERROR: screen_name not found\n";
                }
              }
            }
          }
          if (t_o.find("next_cursor_str") != t_o.end() && t_o["next_cursor_str"]->IsString()) {
            cursor = t_o["next_cursor_str"]->AsString();
            if (cursor.compare("0") == 0) {
              break;
            }
          } else {
            std::cout << "could not find next_cursor_str" << std::endl;
            break;
          }
        }
        delete json_value;
      }
    }
  }

  return members.size();
}

int TwitterAPI::GetUserInfo(const std::string& handle, std::string& user_info) {

  std::set<std::string> user_info_tokens;
  if (GetUserInfo(handle, user_info_tokens) < 0) {
    std::cerr << "ERROR: could not get user info tokens for: " << handle << std::endl;
    return -1;
  } else {
    if (user_info_tokens.empty())
      return 0;
    std::set<std::string>::iterator set_iter;
    set_iter = user_info_tokens.begin();
    user_info.assign(*set_iter);
    set_iter++;
    for (; set_iter != user_info_tokens.end(); set_iter++) {
      user_info += " ";
      user_info += *set_iter;
    }
  }

  return 0;
}

int TwitterAPI::GetUserInfo(const std::string& handle,
                            std::string& name, std::string& description, std::string& url,
                            std::string& age, std::string& gender, std::string& language,
                            std::string& location, std::string& time_zone,
                            std::string& city, std::string& state, std::string& country,
                            std::string& user_info) {

  std::set<std::string> user_info_tokens;

  int num_tokens = 0;
  if ((num_tokens = GetUserInfo(handle, name, description, url,
                               age, gender, language,
                               location, time_zone,
                               city, state, country,
                               user_info_tokens) < 0)) {
    return -1;
  }

  if (user_info_tokens.empty())
    return 0;

  std::set<std::string>::iterator set_iter;
  set_iter = user_info_tokens.begin();
  user_info.assign(*set_iter);
  if (user_info_tokens.size() > 1) {
    set_iter++;
    for (; set_iter != user_info_tokens.end(); set_iter++) {
      user_info += " ";
      user_info += *set_iter;
    }
  }
  user_info_tokens.clear();

  return num_tokens;
}

int TwitterAPI::GetUserInfo(const std::string& handle,
                            std::string& name, std::string& description, std::string& url,
                            std::string& age, std::string& gender, std::string& language,
                            std::string& location, std::string& time_zone,
                            std::string& city, std::string& state, std::string& country,
                            std::set<std::string>& user_info_tokens) {

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string reply_message;
  std::string cursor = "-1";

  bool ret_value = true;
  std::string request_url = "http://api.twitter.com/1/users/show.json?screen_name=" + handle;
  ret_value = curl_request_maker.GetTweets(request_url.c_str());
  if (ret_value) {
    curl_request_maker.GetLastWebResponse(reply_message);
    if (reply_message.size() > 0) {

#ifdef TA_DEBUG
      if (TA_DEBUG > 4) {
          std::cout << std::endl;
          std::cout << reply_message << std::endl;
          std::cout << std::endl;
      }
#endif

      JSONValue* json_value = JSON::Parse(reply_message.c_str());
      if (false == json_value->IsObject()) {
        std::cout << "ERROR: json value obtained for user info not an object\n";
      } else {

        JSONObject json_obj = json_value->AsObject();
        if (json_obj.find("error") != json_obj.end() && json_obj["error"]->IsString()) {
          std::string error = json_obj["error"]->AsString();
          std::cout << "Twitter Error: " << error << std::endl;
          delete json_value;
          return -1;
        }

        JSONObject json_object = json_value->AsObject();
        if (json_object.find("name") != json_object.end() && json_object["name"]->IsString()) {
          name.assign(json_object["name"]->AsString());
          user_info_tokens.insert(name);
        }
        if (json_object.find("description") != json_object.end() && json_object["description"]->IsString()) {
          description.assign(json_object["description"]->AsString());
          user_info_tokens.insert(description);
        }
        if (json_object.find("url") != json_object.end() && json_object["url"]->IsString()) {
          url.assign(json_object["url"]->AsString());
          user_info_tokens.insert(url);
        }
        if (json_object.find("lang") != json_object.end() && json_object["lang"]->IsString()) {
          language.assign(json_object["lang"]->AsString());
          user_info_tokens.insert(language);
        }
        if (json_object.find("location") != json_object.end() && json_object["location"]->IsString()) {
          location.assign(json_object["location"]->AsString());
          user_info_tokens.insert(location);
        }
        if (json_object.find("time_zone") != json_object.end() && json_object["time_zone"]->IsString()) {
          time_zone.assign(json_object["time_zone"]->AsString());
          user_info_tokens.insert(time_zone);
        }
      }
      delete json_value;
    }
  }

#ifdef TA_DEBUG
  if (TA_DEBUG > 1) {
    std::set<std::string>::iterator set_iter;
    for (set_iter = user_info_tokens.begin(); set_iter != user_info_tokens.end(); set_iter++) {
      std::cout << *set_iter << std::endl;
    }
  }
#endif

  return user_info_tokens.size();
}

int TwitterAPI::GetUserInfo(const std::string& handle,
                            std::set<std::string>& user_info_tokens) {

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string reply_message;
  std::string cursor = "-1";

  bool ret_value = true;
  std::string url = "http://api.twitter.com/1/users/show.json?screen_name=" + handle;
  ret_value = curl_request_maker.GetTweets(url.c_str());
  if (ret_value) {
    curl_request_maker.GetLastWebResponse(reply_message);
    if (reply_message.size() > 0) {
      JSONValue* json_value = JSON::Parse(reply_message.c_str());
      if (false == json_value->IsObject()) {
        std::cout << "ERROR: json value obtained for user info not an object\n";
      } else {
        JSONObject json_object = json_value->AsObject();
        if (json_object.find("name") != json_object.end() && json_object["name"]->IsString()) {
          user_info_tokens.insert(json_object["name"]->AsString());
        }
        if (json_object.find("description") != json_object.end() && json_object["description"]->IsString()) {
          user_info_tokens.insert(json_object["description"]->AsString());
        }
        if (json_object.find("url") != json_object.end() && json_object["url"]->IsString()) {
          user_info_tokens.insert(json_object["url"]->AsString());
        }
      }
      delete json_value;
    }
  }

  return user_info_tokens.size();
}

int TwitterAPI::GetUserInfo(const std::string& handle,
                            unsigned char* locations_buffer, const unsigned int locations_buffer_len,
                            unsigned int& locations_len, unsigned int& locations_count,
                            std::set<std::string>& user_info_tokens) {

  if (!locations_buffer) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  locations_buffer[0] = '\0';
  locations_len = 0;
  locations_count = 0;

  std::string name;
  std::string description;
  std::string url;
  std::string age;
  std::string gender;
  std::string language;
  std::string location;
  std::string time_zone;
  std::string city;
  std::string state;
  std::string country;

  int num_tokens = 0;
  if ((num_tokens = GetUserInfo(handle, name, description, url,
                               age, gender, language,
                               location, time_zone,
                               city, state, country,
                               user_info_tokens) < 0)) {
    return -1;
  }

  if (user_info_tokens.empty())
    return 0;

  if (!location.empty()) {
    strcpy((char *) locations_buffer, location.c_str());
    locations_len += location.size();
    strcpy((char *) locations_buffer + locations_len, "|");
    locations_len += 1;
    locations_count += 1;
  }

  if (!time_zone.empty()) {
    strcpy((char *) locations_buffer + locations_len, time_zone.c_str());
    locations_len += time_zone.size();
    strcpy((char *) locations_buffer + locations_len, "|");
    locations_len += 1;
    locations_count += 1;
  }

#ifdef TA_DEBUG
  if (TA_DEBUG > 1) {
    if (locations_count > 0) {
      std::cout << "locations: " << locations_buffer << std::endl;
    } else {
      std::cout << "WARNING: no location found for user: " << handle << std::endl;
    }
  }
#endif

  return user_info_tokens.size();
}

int TwitterAPI::GetListStatuses(const std::string& user_name,
                                const std::string& list_name,
                                std::set<std::string>& tweets) {

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string reply_message;
  std::string cursor = "-1";

  bool ret_value = true;
    std::string url = "http://api.twitter.com/1/" + user_name + "/lists/" \
                      + list_name + "/statuses.json";
    ret_value = curl_request_maker.GetTweets(url.c_str());

    if (ret_value) {
      curl_request_maker.GetLastWebResponse(reply_message);
      if (reply_message.size() > 0) {
        // the response is in json format
        JSONValue *json_value = JSON::Parse(reply_message.c_str());
        if (json_value->IsArray()) {
          JSONArray tweet_array = json_value->AsArray();
          JSONObject tweet_object;
          for (unsigned int i=0; i < tweet_array.size(); i++) {
            JSONValue *tweet_value = tweet_array[i];
            if (false == tweet_value->IsObject()) {
              std::cout << "ERROR: tweet_value is not an object" << std::endl;
            } else {
              tweet_object = tweet_value->AsObject();
              // now lets work on the json object thus obtained
              if (tweet_object.find("text") != tweet_object.end() && tweet_object["text"]->IsString()) {
                tweets.insert(tweet_object["text"]->AsString());
              }
            }
          }
        }
        delete json_value;
      }
    }

  return tweets.size();
}

int TwitterAPI::GetFollowers(const std::string& handle, std::set<std::string>& followers) {

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string temp_str;
  std::string reply_message;
  std::string cursor = "-1";
  int num_followers = 0;

  bool ret_value = true;
  while (ret_value) {
    std::string url = "http://api.twitter.com/1/followers/ids.json?screen_name=" + handle + "&cursor=" + cursor;
    //std::string url = "http://twitter.com/statuses/followers/" + handle + ".json?cursor=" + cursor;
    ret_value = curl_request_maker.GetTweets(url.c_str());

    if (ret_value) {
      curl_request_maker.GetLastWebResponse(reply_message);
      if (reply_message.size() > 0) {
        // the response is in json format
        JSONValue *json_value = JSON::Parse(reply_message.c_str());
        if (!json_value || false == json_value->IsObject()) {
          std::cout << "Error: curl reply not a json object\n";
          break;
        } else {
          // to be specific, the response is a json array
          JSONObject t_o = json_value->AsObject(); 
          if (t_o.find("ids") != t_o.end() && t_o["ids"]->IsArray()) {
            JSONArray tweet_array = t_o["ids"]->AsArray();
            JSONObject tweet_object;
            for (unsigned int i=0; i < tweet_array.size(); i++) {
              num_followers++;
              //JSONValue *tweet_value = tweet_array[i];
              std::string follower_id_str = tweet_array[i]->AsString();
              unsigned long follower_id = tweet_array[i]->AsLongLong();
              std::cout << "follower_id: " << follower_id << std::endl;
              /*
              if (false == tweet_value->IsObject()) {
                std::cout << "ERROR: tweet_value is not an object" << std::endl;
              } else {
                tweet_object = tweet_value->AsObject();
                // now lets work on the json object thus obtained
                if (tweet_object.find("screen_name") != tweet_object.end() && tweet_object["screen_name"]->IsString()) {
                  followers.insert(tweet_object["screen_name"]->AsString());
                }
              }
              */
            }
          }
          if (t_o.find("next_cursor_str") != t_o.end() && t_o["next_cursor_str"]->IsString()) {
            cursor = t_o["next_cursor_str"]->AsString();
            if (cursor.compare("0") == 0) {
              break;
            }
          } else {
            std::cout << "could not find next_cursor_str" << std::endl;
            break;
          }
        }
        delete json_value;
      }
    }
  }
  return num_followers;
}

int TwitterAPI::GetFollowerTweets(const std::string& handle,
                                       std::set<std::string>& tweets,
                                       const unsigned int required_count) {

  inagist_api::CurlRequestMaker curl_request_maker;

  std::string temp_str;
  std::string reply_message;
  std::string cursor = "-1";
  int num_followers = 0;
  std::string follower_id;
  std::string follower;
  bool flag = false;
//  unsigned int num_tweets = 0;
  bool ret_value = true;

  while (!flag && ret_value) {

    std::string url = "http://api.twitter.com/1/followers/ids.json?screen_name=" + handle + "&cursor=" + cursor;
#ifdef TA_DEBUG
    if (TA_DEBUG > 1) {
      std::cout << "Url: " << url << std::endl;
    }
#endif // TA_DEBUG

    ret_value = curl_request_maker.GetTweets(url.c_str());

    if (!ret_value) {
#ifdef TA_DEBUG
      std::cerr << "ERROR: curl request failed\n";
#endif // TA_DEBUG
      break;
    }

    curl_request_maker.GetLastWebResponse(reply_message);
#ifdef TA_DEBUG
    if (TA_DEBUG > 2) {
      std::cout << reply_message << std::endl;
    }
#endif // TA_DEBUG
    if (reply_message.size() > 0) {
      // the response is in json format
      JSONValue *json_value = JSON::Parse(reply_message.c_str());
      if (!json_value || false == json_value->IsObject()) {
#ifdef TA_DEBUG
        std::cout << "ERROR: curl reply not a json object\n";
#endif // TA_DEBUG
        break;
      } else {
        JSONObject json_obj = json_value->AsObject();
        if (json_obj.find("error") != json_obj.end() && json_obj["error"]->IsString()) {
          std::string error = json_obj["error"]->AsString();
          std::cout << "Twitter Error: " << error << std::endl;
          break;
        }
        // to be specific, the response is a json array
        JSONArray json_array = json_value->AsArray();
        for (unsigned int i=0; i < json_array.size(); i++) {
          follower_id = json_array[i]->AsString();
#ifdef TA_DEBUG
          if (TA_DEBUG > 2) {
            std::cout << "follower id: " << follower_id << std::endl;
          }
#endif // TA_DEBUG
          num_followers++;
          /*
              num_tweets += GetUserTimeLine(follower, tweets);
              if (num_tweets > required_count) {
                flag = true;
                break;
              }
          */
        }
/*
        if (t_o.find("next_cursor_str") != t_o.end() && t_o["next_cursor_str"]->IsString()) {
          cursor = t_o["next_cursor_str"]->AsString();
          if (cursor.compare("0") == 0) {
            break;
          }
        } else {
          std::cout << "could not find next_cursor_str" << std::endl;
          break;
        }
*/
        delete json_value;
      }
    }
    break;
  } // while

  return num_followers;
}

} // namespace
