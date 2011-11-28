#include "profiler.h"
#include <cstring>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include "inagist_api.h"
#include "twitter_api.h"
#include "twitter_searcher.h"
#include "string_utils.h"
#include "func_utils.h"

#ifdef DEBUG
#if DEBUG>0
#define PROFILE_DEBUG DEBUG
#endif
#endif

// #define PROFILE_DEBUG 3

#define MAX_CLASS_NAME 32
#define MAX_LIST_LEN  255
#define ULTIMATE_BUFFER_LEN 10240

namespace inagist_dashboard {

bool SortFunction (double i,double j) {
  return (i < j);
}

Profiler::Profiler() {

#ifdef PROFILE_DEBUG
#if PROFILE_DEBUG>0
  SetDebugLevel(PROFILE_DEBUG);
#endif
#endif

}

Profiler::~Profiler() {
  m_gist.Clear();
#ifdef PROFILE_DEBUG
if (m_debug_level > 3) {
  std::cout << "Profiler Destructor\n";
}
#endif // PROFILE_DEBUG
}

int Profiler::Init(const char* gist_maker_config) {

  if (!gist_maker_config) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif
    return -1;
  }

  if (m_gist_maker.Init(gist_maker_config) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not initialize gist maker\n";
#endif
    return -1;
  }

  return 0;
}

int Profiler::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;

  m_gist_maker.SetDebugLevel(debug_level);
  return 0;
}

// given a twitter handle this calls other function to produce a corpus, classify and
// then write the corpus to the output file.
// the text classifier must have already been initialized before this is called
int Profiler::GetProfile(const std::string& twitter_handle,
                      std::set<std::string>& locations
#ifdef LANG_ENABLED
                      , std::set<std::string>& self_languages
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                      , std::set<std::string>& self_text_classes
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                      , std::set<std::string>& self_location_classes
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                      //std::map<std::string, std::string>& self_text_class_contributors_map,
                      , std::set<std::string>& self_text_class_contributors
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
                      , std::set<std::string>& others_languages
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                      , std::set<std::string>& others_text_classes
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                      , std::set<std::string>& others_location_classes
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                      //std::map<std::string, std::string>& others_text_class_contributors_map,
                      , std::set<std::string>& others_text_class_contributors
#endif // TEXT_CLASSIFICATION_ENABLED
                      , std::set<std::string>& features
                      , const std::string& profile_name) {

  unsigned char locations_buffer[MAX_BUFFER_LEN];
  locations_buffer[0] = '\0';
  unsigned int locations_buffer_len = MAX_BUFFER_LEN;
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;

  char self_lang_classes_buffer[MAX_BUFFER_LEN];
  self_lang_classes_buffer[0] = '\0';
  unsigned int self_lang_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int self_lang_classes_len = 0;
  unsigned int self_lang_classes_count = 0;

  char self_text_classes_buffer[MAX_BUFFER_LEN];
  self_text_classes_buffer[0] = '\0';
  unsigned int self_text_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int self_text_classes_len = 0;
  unsigned int self_text_classes_count = 0;

#ifdef LOCATION_ENABLED
  char self_location_classes_buffer[MAX_BUFFER_LEN];
  self_location_classes_buffer[0] = '\0';
  unsigned int self_location_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int self_location_classes_len = 0;
  unsigned int self_location_classes_count = 0;
#endif // LOCATION_ENABLED

  unsigned char self_text_class_contributors_buffer[ULTIMATE_BUFFER_LEN];
  self_text_class_contributors_buffer[0] = '\0';
  unsigned int self_text_class_contributors_buffer_len = ULTIMATE_BUFFER_LEN;
  unsigned int self_text_class_contributors_len = 0;
  unsigned int self_text_class_contributors_count = 0;

  char others_lang_classes_buffer[MAX_BUFFER_LEN];
  others_lang_classes_buffer[0] = '\0';
  unsigned int others_lang_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int others_lang_classes_len = 0;
  unsigned int others_lang_classes_count = 0;

  char others_text_classes_buffer[MAX_BUFFER_LEN];
  others_text_classes_buffer[0] = '\0';
  unsigned int others_text_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int others_text_classes_len = 0;
  unsigned int others_text_classes_count = 0;

#ifdef LOCATION_ENABLED
  char others_location_classes_buffer[MAX_BUFFER_LEN];
  others_location_classes_buffer[0] = '\0';
  unsigned int others_location_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int others_location_classes_len = 0;
  unsigned int others_location_classes_count = 0;
#endif // LOCATION_ENABLED

  unsigned char others_text_class_contributors_buffer[ULTIMATE_BUFFER_LEN];
  others_text_class_contributors_buffer[0] = '\0';
  unsigned int others_text_class_contributors_buffer_len = ULTIMATE_BUFFER_LEN;
  unsigned int others_text_class_contributors_len = 0;
  unsigned int others_text_class_contributors_count = 0;

  unsigned char features_buffer[MAX_BUFFER_LEN];
  features_buffer[0] = '\0';
  unsigned int features_buffer_len = MAX_BUFFER_LEN;
  unsigned int features_len = 0;
  unsigned int features_count = 0;

  std::set<std::string> scripts;
  int corpus_size = 0;

  // genrate profile
  if ((corpus_size = GetProfile(twitter_handle.c_str(), twitter_handle.length(),
                             locations_buffer, locations_buffer_len,
                             locations_len, locations_count,
#ifdef LANG_ENABLED
                             self_lang_classes_buffer, self_lang_classes_buffer_len,
                             self_lang_classes_len, self_lang_classes_count,
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                             self_text_classes_buffer, self_text_classes_buffer_len,
                             self_text_classes_len, self_text_classes_count,
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                             self_location_classes_buffer, self_location_classes_buffer_len,
                             self_location_classes_len, self_location_classes_count,
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                             self_text_class_contributors_buffer,
                             self_text_class_contributors_buffer_len,
                             self_text_class_contributors_len,
                             self_text_class_contributors_count,
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
                             others_lang_classes_buffer, others_lang_classes_buffer_len,
                             others_lang_classes_len, others_lang_classes_count,
                             others_text_classes_buffer, others_text_classes_buffer_len,
                             others_text_classes_len, others_text_classes_count,
#endif // LANG_ENABLED
#ifdef LOCATION_ENABLED
                             others_location_classes_buffer, others_location_classes_buffer_len,
                             others_location_classes_len, others_location_classes_count,
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                             others_text_class_contributors_buffer,
                             others_text_class_contributors_buffer_len,
                             others_text_class_contributors_len,
                             others_text_class_contributors_count,
#endif // TEXT_CLASSIFICATION_ENABLED
                             features_buffer, features_buffer_len,
                             features_len, features_count,
                             profile_name.c_str())) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not generate profile\n";
#endif
    return -1;
  }

  //profanity_status = std::string(profanity_status_buffer);
  if ((inagist_utils::PipeListToSet(locations_buffer, locations)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate locations\n";
#endif
  }
  locations_buffer[0] = '\0';

#ifdef LANG_ENABLED
  if ((inagist_utils::PipeListToSet((unsigned char*) self_lang_classes_buffer,
                                            self_languages)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate languages\n";
#endif
  }
  self_lang_classes_buffer[0] = '\0';
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  if ((inagist_utils::PipeListToSet((unsigned char*) self_text_classes_buffer,
                                            self_text_classes)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate self_text_classes\n";
#endif
  }
  self_text_classes_buffer[0] = '\0';

  if ((inagist_utils::PipeListToSet((unsigned char*) self_text_class_contributors_buffer,
                                            self_text_class_contributors)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate self_text_class_contributors\n";
#endif
  }
  self_text_class_contributors_buffer[0] = '\0';
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LOCATION_ENABLED
  if ((inagist_utils::PipeListToSet((unsigned char*) self_location_classes_buffer,
                                            self_location_classes)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate self_text_classes\n";
#endif
  }
  self_location_classes_buffer[0] = '\0';
#endif // LOCATION_ENABLED

#ifdef LANG_ENABLED
  if ((inagist_utils::PipeListToSet((unsigned char*) others_lang_classes_buffer,
                                            others_languages)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate languages\n";
#endif
  }
  others_lang_classes_buffer[0] = '\0';
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  if ((inagist_utils::PipeListToSet((unsigned char*) others_text_classes_buffer,
                                            others_text_classes)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate others_text_classes\n";
#endif
  }
  others_text_classes_buffer[0] = '\0';

  if ((inagist_utils::PipeListToSet((unsigned char*) others_text_class_contributors_buffer,
                                            others_text_class_contributors)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate others_text_class_contributors\n";
#endif
  }
  others_text_class_contributors_buffer[0] = '\0';
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LOCATION_ENABLED
  if ((inagist_utils::PipeListToSet((unsigned char*) others_location_classes_buffer,
                                            others_location_classes)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate others_text_classes\n";
#endif
  }
  others_location_classes_buffer[0] = '\0';
#endif // LOCATION_ENABLED

  if ((inagist_utils::PipeListToSet((unsigned char*) features_buffer,
                                    features)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate features\n";
#endif
  }
  features_buffer[0] = '\0';

  return 0;
}

int Profiler::GetProfile(const char* twitter_handle, unsigned int twitter_handle_len,
      unsigned char* locations_buffer, const unsigned int locations_buffer_len,
      unsigned int& locations_len, unsigned int& locations_count
#ifdef LANG_ENABLED
      , char* self_lang_classes_buffer, const unsigned int self_lang_classes_buffer_len,
      unsigned int& self_lang_classes_len, unsigned int& self_lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , char* self_text_classes_buffer, const unsigned int self_text_classes_buffer_len,
      unsigned int& self_text_classes_len, unsigned int& self_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
      , char* self_location_classes_buffer, const unsigned int self_location_classes_buffer_len,
      unsigned int& self_location_classes_len, unsigned int& self_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , unsigned char* self_text_class_contributors_buffer,
      const unsigned int self_text_class_contributors_buffer_len,
      unsigned int& self_text_class_contributors_len,
      unsigned int& self_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
      , char* others_lang_classes_buffer, const unsigned int others_lang_classes_buffer_len,
      unsigned int& others_lang_classes_len, unsigned int& others_lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , char* others_text_classes_buffer, const unsigned int others_text_classes_buffer_len,
      unsigned int& others_text_classes_len, unsigned int& others_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
      , char* others_location_classes_buffer, const unsigned int others_location_classes_buffer_len,
      unsigned int& others_location_classes_len, unsigned int& others_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , unsigned char* others_text_class_contributors_buffer,
      const unsigned int others_text_class_contributors_buffer_len,
      unsigned int& others_text_class_contributors_len,
      unsigned int& others_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
      , unsigned char* features_buffer, const unsigned int features_buffer_len,
      unsigned int& features_len, unsigned int& features_count
      , const char* profile_name) {

  if (!twitter_handle || twitter_handle_len < 1) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: invalid input twitter handle\n";
#endif
    return -1;
  }

  if (!locations_buffer
#ifdef LANG_ENABLED
      || !self_lang_classes_buffer
      || !others_lang_classes_buffer
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      || !self_text_classes_buffer
      || !self_text_class_contributors_buffer
      || !others_text_classes_buffer
      || !others_text_class_contributors_buffer
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
      || !self_location_classes_buffer
      || !others_location_classes_buffer
#endif // LOCATION_ENABLED
      || !features_buffer
     ) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: invalid output buffer(s) for profiling\n";
#endif
    return -1;
  }

  locations_buffer[0] = '\0';
  locations_len = 0;
  locations_count = 0;
#ifdef LANG_ENABLED
  self_lang_classes_buffer[0] = '\0';
  self_lang_classes_len = 0;
  self_lang_classes_count = 0;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  self_text_classes_buffer[0] = '\0';
  self_text_classes_len = 0;
  self_text_classes_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
  self_location_classes_buffer[0] = '\0';
  self_location_classes_len = 0;
  self_location_classes_count = 0;
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  self_text_class_contributors_buffer[0] = '\0';
  self_text_class_contributors_len = 0;
  self_text_class_contributors_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
  others_lang_classes_buffer[0] = '\0';
  others_lang_classes_len = 0;
  others_lang_classes_count = 0;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  others_text_classes_buffer[0] = '\0';
  others_text_classes_len = 0;
  others_text_classes_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
  others_location_classes_buffer[0] = '\0';
  others_location_classes_len = 0;
  others_location_classes_count = 0;
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  others_text_class_contributors_buffer[0] = '\0';
  others_text_class_contributors_len = 0;
  others_text_class_contributors_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED
  features_buffer[0] = '\0';
  features_len = 0;
  features_count = 0;

  bool get_user_info = true;

  std::set<std::string> tweets;
  inagist_api::TwitterAPI twitter_api;
  inagist_api::TwitterSearcher twitter_searcher;

  int user_info_count = 0;
  unsigned int self_docs_count = 0;
  unsigned int others_docs_count = 0;

  if (get_user_info) {
    std::set<std::string> user_info_tokens;
    if ((user_info_count = inagist_api::TwitterAPI::GetUserInfo(twitter_handle,
                                                               locations_buffer, locations_buffer_len,
                                                               locations_len, locations_count,
                                                               user_info_tokens)) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "ERROR: could not get user info token for twitter_handle: " << twitter_handle << std::endl;
#endif
    } else {
      /*
      std::set<std::string>::iterator tokens_iter;
      for (tokens_iter = user_info_tokens.begin(); tokens_iter != user_info_tokens.end(); tokens_iter++) {
        if (corpus.find(*tokens_iter) != corpus.end()) {
          corpus[*tokens_iter] += 1;
        } else {
          corpus[*tokens_iter] = 1;
        }
      }
      corpus_size += user_info_tokens.size();
      */
      user_info_tokens.clear();
    }
  }

#ifdef PROFILE_DEBUG
  if (m_debug_level > 1) {
    std::cout << "self tweets:" << std::endl;
  }
#endif // PROFILE_DEBUG


  // latest tweets for this user using twitter search
  if (twitter_api.GetUserTimeLine(twitter_handle, tweets) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "WARNING: could not get latest tweets for user: " \
              << twitter_handle << std::endl;
#endif
  }

  if (twitter_searcher.Get100TweetsFromUser(twitter_handle, tweets) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "WARNING: could not search for 100 tweets from user: " \
              << twitter_handle << std::endl;
#endif
  }

  if (tweets.empty()) {
    return 0;
  }

  Profile* profile;
  CreateProfile(profile);

  self_docs_count = tweets.size();
  if (AddTextsToProfile(tweets, profile) < 0) {
    std::cerr << "ERROR: could not add texts to profile\n";
  }
  tweets.clear();

  // generate profile
  if (GetProfile(profile
#ifdef LANG_ENABLED
                 , self_lang_classes_buffer, self_lang_classes_buffer_len,
                 self_lang_classes_len, self_lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                 , self_text_classes_buffer, self_text_classes_buffer_len,
                 self_text_classes_len, self_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                 , self_location_classes_buffer, self_location_classes_buffer_len,
                 self_location_classes_len, self_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                 , self_text_class_contributors_buffer, self_text_class_contributors_buffer_len,
                 self_text_class_contributors_len, self_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
                 ) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "WARNING: could not generate profile for user: " << twitter_handle << std::endl;
#endif
  }
  profile->Clear();
  DeleteProfile(profile);

  // handling friends info below
#ifdef PROFILE_DEBUG
  if (m_debug_level > 1) {
    std::cout << "other's tweets:" << std::endl;
  }
#endif // PROFILE_DEBUG

  // since this is likely the first time profiling this handle,
  // get archieved data from inagist
  if (get_user_info) {
    // inagist trending tweets
    if (inagist_api::InagistAPI::GetTrendingTweets(twitter_handle, tweets) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "WARNING: could not get inagist trending tweets for user: " \
                << twitter_handle << std::endl;
#endif
    }

    // inagist archieves
    if (inagist_api::InagistAPI::GetArchievedTweets(twitter_handle, tweets) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "WARNING: could not get inagist trending tweets for user: " \
                << twitter_handle << std::endl;
#endif
    }
  }

  if (tweets.empty()) {
    return 0;
  }

  Profile* friends_profile;
  CreateProfile(friends_profile);
  others_docs_count = tweets.size();
  if (AddTextsToProfile(tweets, friends_profile) < 0) {
    std::cerr << "ERROR: could not add texts to friends profile\n";
  }
  tweets.clear();

  // generate profile
  if (GetProfile(friends_profile
#ifdef LANG_ENABLED
                 , others_lang_classes_buffer, others_lang_classes_buffer_len,
                 others_lang_classes_len, others_lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                 , others_text_classes_buffer, others_text_classes_buffer_len,
                 others_text_classes_len, others_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                 , others_location_classes_buffer, others_location_classes_buffer_len,
                 others_location_classes_len, others_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                 , others_text_class_contributors_buffer, others_text_class_contributors_buffer_len,
                 others_text_class_contributors_len, others_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
                ) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "WARNING: could not get profile from user's friends\n";
#endif
  }
  friends_profile->Clear();
  DeleteProfile(friends_profile);

#ifdef PROFILE_DEBUG
  if (m_debug_level > 1) {
    std::cout << "profile generated from " << self_docs_count \
              << " docs of user: " << twitter_handle \
              << " and " << others_docs_count << " docs of his/her friends" << std::endl;
  }
#endif

  return self_docs_count + others_docs_count; 
}

int Profiler::CreateProfile(Profile* &profile) {
  profile = (Profile*) new Profile();
  if (!profile) {
    std::cerr << "ERROR: could not create an instance of profile object\n";
    return -1;
  }
  profile->id = 1; // doesn't matter for now
  return 0;
}

int Profiler::DeleteProfile(Profile* &profile) {
  profile->Clear();
  delete profile;
  profile = (Profile*) NULL;
  return 0;
}

int Profiler::AddTextsToProfile(std::set<std::string>& texts, Profile* &profile) {

  unsigned char* text_buffer[MAX_BUFFER_LEN];
  unsigned int text_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_len = 0;

  std::set<std::string>::iterator set_iter;
  std::string text;
  for (set_iter = texts.begin(); set_iter != texts.end(); set_iter++) {
    text.assign(*set_iter);
    if (text.length() < 1)
      continue;
#ifdef PROFILE_DEBUG
    if (m_debug_level > 1) {
      std::cout << text << std::endl;
    }
#endif // PROFILE_DEBUG
    strcpy((char*) text_buffer, text.c_str());
    text_len = text.length();
    if (AddTextToProfile((const unsigned char*) text_buffer, text_buffer_len, text_len, profile) < 0) {
      std::cerr << "ERROR: could not add text to profile\n";
    }
  }
  text.clear();

  return 0;
}

int Profiler::AddTextToProfile(const unsigned char* text_buffer,
                               const unsigned int text_buffer_len,
                               const unsigned int text_len,
                               Profile* &profile,
                               const double& dynamic_score) {

  m_gist.Clear();

  // TODO (balaji) make GistMaker accept this struct
  if (m_gist_maker.MakeGist((unsigned char*) text_buffer, text_buffer_len, text_len,
                  (char*) m_gist.profanity_status_buffer, m_gist.profanity_status_buffer_len,
                  (char*) m_gist.scripts_buffer, m_gist.scripts_buffer_len,
                  (unsigned char*) m_gist.named_entities_buffer, m_gist.named_entities_buffer_len,
                  m_gist.named_entities_len, m_gist.named_entities_count,
                  (unsigned char*) m_gist.keywords_buffer, m_gist.keywords_buffer_len,
                  m_gist.keywords_len, m_gist.keywords_count,
                  (unsigned char*) m_gist.keyphrases_buffer, m_gist.keyphrases_buffer_len,
                  m_gist.keyphrases_len, m_gist.keyphrases_count
#ifdef LANG_ENABLED
                  , (char*) m_gist.lang_classes_buffer, m_gist.lang_classes_buffer_len,
                  m_gist.lang_classes_len, m_gist.lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                  , (char*) m_gist.text_classes_buffer, m_gist.text_classes_buffer_len,
                  m_gist.text_classes_len, m_gist.text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                  , (char*) m_gist.location_classes_buffer, m_gist.location_classes_buffer_len,
                  m_gist.location_classes_len, m_gist.location_classes_count
#endif // LOCATION_ENABLED
                  , m_gist.intent_valence
                  , m_gist.sentiment_valence
                 ) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not get named_entities\n";
#endif
    return -1;
  }

  // now make use of the gist
#ifdef LANG_ENABLED
  if (inagist_utils::PipeListToMap((unsigned char*) m_gist.lang_classes_buffer,
                                            profile->lang_classes_map) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: PipeListToMap failed for language\n";
#endif
  }
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  if (inagist_utils::PipeListToMap((unsigned char*) m_gist.text_classes_buffer,
                                            profile->text_classes_map) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: PipeListToMap failed\n";
#endif
  }
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LOCATION_ENABLED 
  if (inagist_utils::PipeListToMap((unsigned char*) m_gist.location_classes_buffer,
                                            profile->locations_map) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: PipeListToMap failed\n";
#endif
  }
#endif // LOCATION_ENABLED

  // TODO (balaji) don't send these crap from script_detection code.
  if (strlen(m_gist.scripts_buffer) > 0) {
    if (strcmp(m_gist.scripts_buffer, "en") != 0 && // this is legitimate though
        strcmp(m_gist.scripts_buffer, "UU") != 0 &&
        strcmp(m_gist.scripts_buffer, "RR") != 0 &&
        strcmp(m_gist.scripts_buffer, "XX") != 0 &&
        strcmp(m_gist.scripts_buffer, "xx") != 0) {
      std::string script = std::string(m_gist.scripts_buffer);
      if (profile->lang_classes_map.find(script) != profile->lang_classes_map.end()) {
        profile->lang_classes_map.insert(std::pair<std::string, double>(script, 1));
      } else {
        profile->lang_classes_map[script] += 1;
      }
    }
  }

  int text_weight = 0;
  if (dynamic_score != 0) {
    text_weight = dynamic_score;
  } else {
    if (strstr((char*) text_buffer, "RT ") == NULL) {
      text_weight += 2;
    }
  }

  int intent_weight = 0;
  if (m_gist.intent_valence > 3) {
    intent_weight = 5;
  }

  int sentiment_weight = 0;
  if (m_gist.sentiment_valence > 3) {
    sentiment_weight = 1;
  } else if (m_gist.sentiment_valence < 0) {
    sentiment_weight = -1;
  }

  // named entities
  int value = text_weight + intent_weight + sentiment_weight;
  value += 3;
  if (inagist_utils::PipeListToMap((unsigned char*) m_gist.named_entities_buffer,
                                   profile->features_map,
                                   value) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: PipeListToMap failed for named entities\n";
#endif
  }

  // keywords
  value = text_weight + intent_weight + sentiment_weight;
  value += 1;
  if (inagist_utils::PipeListToMap((unsigned char*) m_gist.keywords_buffer,
                                   profile->features_map,
                                   value) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: PipeListToMap failed for keywords\n";
#endif
  }

  std::set<std::string> text_classes_set;
  if (inagist_utils::PipeListToSet((unsigned char*) m_gist.text_classes_buffer,
                                            text_classes_set) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: PipeListToSet failed for text_classes_set\n";
#endif
  }

  std::set<std::string> named_entities_set;
  if (inagist_utils::PipeListToSet((unsigned char*) m_gist.named_entities_buffer,
                                            named_entities_set) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: PipeListToSet failed for named_entities_set\n";
#endif
  }

  std::set<std::string> keywords_set;
  if (inagist_utils::PipeListToSet((unsigned char*) m_gist.keywords_buffer,
                                            keywords_set) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: PipeListToSet failed for keywords_set\n";
#endif
  }

  std::string class_name;
  std::string key;
  std::set<std::string>::iterator class_iter;
  std::set<std::string>::iterator set_iter;
  for (class_iter = text_classes_set.begin(); class_iter != text_classes_set.end(); class_iter++) {
    class_name = *class_iter;
    for (set_iter = named_entities_set.begin(); set_iter != named_entities_set.end(); set_iter++) {
      key = *set_iter;
      profile->features_to_classes_map.insert(std::pair<std::string, std::string> (key, class_name));
    }
    for (set_iter = keywords_set.begin(); set_iter != keywords_set.end(); set_iter++) {
      key = *set_iter;
      profile->features_to_classes_map.insert(std::pair<std::string, std::string> (key, class_name));
    }
  }
  text_classes_set.clear();
  named_entities_set.clear();
  keywords_set.clear();

  m_gist.Clear();

  return 0;
}

int Profiler::GetProfile(Profile* &profile
#ifdef LANG_ENABLED
                         , char* lang_classes_buffer,
                         const unsigned int& lang_classes_buffer_len,
                         unsigned int& lang_classes_len,
                         unsigned int& lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                         , char* text_classes_buffer,
                         const unsigned int& text_classes_buffer_len,
                         unsigned int& text_classes_len,
                         unsigned int& text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                         , char* location_classes_buffer,
                         const unsigned int& location_classes_buffer_len,
                         unsigned int& location_classes_len,
                         unsigned int& location_classes_count
#endif // LOCATION_ENABLED
                         , unsigned char* features_buffer,
                         const unsigned int& features_buffer_len,
                         unsigned int& features_len,
                         unsigned int& features_count) {

  lang_classes_buffer[0] = '\0';
  lang_classes_len = 0;
  lang_classes_count = 0;
  text_classes_buffer[0] = '\0';
  text_classes_len = 0;
  text_classes_count = 0;
#ifdef LOCATION_ENABLED
  location_classes_buffer[0] = '\0';
  location_classes_len = 0;
  location_classes_count = 0;
#endif // LOCATION_ENABLED
  features_buffer[0] = '\0';
  features_len = 0;
  features_count = 0;

  unsigned int n = 1;
#ifdef LANG_ENABLED
  lang_classes_buffer[0] = '\0';
  lang_classes_len = 0;
  lang_classes_count = 0;
  if (inagist_utils::FindTopN(profile->lang_classes_map, n,
                              lang_classes_buffer, lang_classes_buffer_len,
                              lang_classes_len, lang_classes_count) < 0) {
    std::cerr << "ERROR: could not get top 3 elements\n";
  }

  std::string language;
  for (profile->map_iter = profile->scripts_map.begin();
       profile->map_iter != profile->scripts_map.end();
       profile->map_iter++) {
    language = profile->map_iter->first;
    inagist_utils::InsertIntoBuffer(language,
                                    (unsigned char*) lang_classes_buffer, lang_classes_buffer_len,
                                    lang_classes_len, lang_classes_count);
  }

#ifdef PROFILE_DEBUG
  if (m_debug_level > 2) {
    std::cout << "lang_classes_count: " << lang_classes_count << std::endl;
  }
#endif
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  n = 3;
  text_classes_buffer[0] = '\0';
  text_classes_len = 0;
  text_classes_count = 0;
  if (inagist_utils::FindTopN(profile->text_classes_map, n,
                              text_classes_buffer, text_classes_buffer_len,
                              text_classes_len, text_classes_count) < 0) {
    std::cerr << "ERROR: could not get top 3 elements\n";
  }

#ifdef PROFILE_DEBUG
  if (m_debug_level > 2) {
    std::cout << "text_classes_count: " << text_classes_count << std::endl;
    if (text_classes_buffer && text_classes_len > 0) {
      std::cout << "text_casses_buffer: " << text_classes_buffer << std::endl;
    }
  }
#endif
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LOCATION_ENABLED
  if (inagist_utils::MapToPipeList(profile->location_classes_map,
                                   (unsigned char*) location_classes_buffer, location_classes_buffer_len,
                                   location_classes_len, location_classes_count) < 0) {
    std::cerr << "ERROR: could not populate set elements to pipe separated list\n";
  }
  location_classes_map.clear();
#ifdef PROFILE_DEBUG
  if (m_debug_level > 2) {
    std::cout << "location_classes_count: " << location_classes_count << std::endl;
  }
#endif
#endif // LOCATION_ENABLED

  // change containers
  std::multimap<double, std::string> scores_features_map;
  std::vector<double> scores_vector;
  double score;
  std::string feature;
  std::string class_name;
  for (profile->map_iter = profile->features_map.begin();
       profile->map_iter != profile->features_map.end();
       profile->map_iter++) {

    // introduce class boost here
    // this one, merely adds a point or two if the feature belongs to the final text classes assigned to the profile
    feature = profile->map_iter->first;
    score = profile->map_iter->second;
    profile->range = profile->features_to_classes_map.equal_range(feature);
    for (profile->mmap_iter = profile->range.first;
         profile->mmap_iter != profile->range.second;
         profile->mmap_iter++) {
      class_name = profile->mmap_iter->second; 
      // TODO (balaji) following two classes cannot get any boost. recheck this logic later
      if (class_name.compare("IAB12 News") != 0 &&
          class_name.compare("IAB0 Uncategorized") != 0) {
        if (strstr(text_classes_buffer, class_name.c_str()) != NULL) {
          score += 2;
          break;
        }
      }
    }
    // end of class boost

    scores_vector.push_back(score);
    scores_features_map.insert(std::pair<double, std::string>(score, feature));
  }

  // sort
  std::sort(scores_vector.begin(), scores_vector.end(), SortFunction);

  // revert as needed
  std::vector<double>::iterator scores_iter;
  std::multimap<double, std::string>::iterator scores_features_iter;
  char score_feature_array[128];
  std::string score_feature_str;
  for (scores_iter = scores_vector.begin(); scores_iter != scores_vector.end(); scores_iter++) {
    score = *scores_iter;
    if ((scores_features_iter = scores_features_map.find(score)) != scores_features_map.end()) {
      score = (int) scores_features_iter->first;
      feature = scores_features_iter->second;
      if (feature.length() < 100 && feature.length() > 0) {
      memset(score_feature_array, '\0', 128);
      sprintf(score_feature_array, "%d %s", (int) score, feature.c_str());
      score_feature_str = std::string(score_feature_array);
      inagist_utils::InsertIntoBuffer(score_feature_str,
                                    (unsigned char*) features_buffer, features_buffer_len,
                                    features_len, features_count);
      }
      scores_features_map.erase(scores_features_iter);
    }
  }
  scores_features_map.clear();
  scores_vector.clear();

  return 0;
}

/*
int Profiler::MultiMapToPipeList(std::multimap<double, std::string>& map,
                  unsigned char* buffer, unsigned int buffer_len,
                  unsigned int& list_len, unsigned int& list_count) {

  if (!buffer) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  if (map.empty()) {
    return 0;
  }

  list_len = 0;
  list_count = 0;
  std::multimap<double, std::string>::iterator map_iter;
  unsigned char* ptr = buffer;
  std::string element;
  int temp_len = 0;
  for (map_iter = map.begin(); map_iter != map.end(); map_iter++) {
    element.assign((*map_iter).second);
    element += ",";
    if ((list_len + element.length() + 21) >= buffer_len) { // WTH?
      break;
    }
    strcpy((char *) ptr, element.c_str()); 
    ptr += element.length();
    temp_len = sprintf((char *) ptr, "%f", (*map_iter).first);
    ptr += temp_len;
    strcpy((char *) ptr, "|");
    ptr += 1;
    list_count++;
    list_len += element.length();
    list_len += temp_len;
    list_len += 1;
  }

  return list_count;
}

*/

} // namespace inagist_dashboard
