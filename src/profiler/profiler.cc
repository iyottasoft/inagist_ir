#include "profiler.h"
#include <cstring>
#include <cstdlib>
//#include <vector>
//#include <algorithm>
#include "corpus_manager.h"
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

Profiler::Profiler() {

#ifdef PROFILE_DEBUG
#if PROFILE_DEBUG>0
  SetDebugLevel(PROFILE_DEBUG);
#endif
#endif

}

Profiler::~Profiler() {
#ifdef RECSYS_ENABLED
  if (!m_recsys_input_map.empty()) {
    m_recsys_input_map.clear();
  }
  if (!m_recsys_input_class_map.empty()) {
    m_recsys_input_class_map.clear();
  }
#endif // RECSYS_ENABLED
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
int Profiler::Profile(const std::string& twitter_handle,
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
#ifdef RECSYS_ENABLED
                      , std::set<std::string>& recommendations
#endif // RECSYS_ENABLED
                      , const std::string& profile_name) {

  unsigned char locations_buffer[MAX_BUFFER_LEN];
  locations_buffer[0] = '\0';
  unsigned int locations_buffer_len = MAX_BUFFER_LEN;
  unsigned int locations_len = 0;
  unsigned int locations_count = 0;

  char self_languages_buffer[MAX_BUFFER_LEN];
  self_languages_buffer[0] = '\0';
  unsigned int self_languages_buffer_len = MAX_BUFFER_LEN;
  unsigned int self_languages_len = 0;
  unsigned int self_languages_count = 0;

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

  char others_languages_buffer[MAX_BUFFER_LEN];
  others_languages_buffer[0] = '\0';
  unsigned int others_languages_buffer_len = MAX_BUFFER_LEN;
  unsigned int others_languages_len = 0;
  unsigned int others_languages_count = 0;

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

  unsigned char recommendations_buffer[MAX_BUFFER_LEN];
  recommendations_buffer[0] = '\0';
  unsigned int recommendations_buffer_len = MAX_BUFFER_LEN;
  unsigned int recommendations_len = 0;
  unsigned int recommendations_count = 0;

  std::set<std::string> scripts;
  int corpus_size = 0;

  // genrate profile
  if ((corpus_size = Profile(twitter_handle.c_str(), twitter_handle.length(),
                             locations_buffer, locations_buffer_len,
                             locations_len, locations_count,
#ifdef LANG_ENABLED
                             self_languages_buffer, self_languages_buffer_len,
                             self_languages_len, self_languages_count,
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
                             others_languages_buffer, others_languages_buffer_len,
                             others_languages_len, others_languages_count,
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
#ifdef RECSYS_ENABLED
                             recommendations_buffer, recommendations_buffer_len,
                             recommendations_len, recommendations_count,
#endif // RECSYS_ENABLED
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
  if ((inagist_utils::PipeListToSet((unsigned char*) self_languages_buffer,
                                            self_languages)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate languages\n";
#endif
  }
  self_languages_buffer[0] = '\0';
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
  if ((inagist_utils::PipeListToSet((unsigned char*) others_languages_buffer,
                                            others_languages)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate languages\n";
#endif
  }
  others_languages_buffer[0] = '\0';
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

#ifdef RECSYS_ENABLED
  if ((inagist_utils::PipeListToSet((unsigned char*) recommendations_buffer,
                                    recommendations)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate recommendations\n";
#endif
  }
  recommendations_buffer[0] = '\0';
#endif // RECSYS_ENABLED

  return 0;
}

int Profiler::Profile(const char* twitter_handle, unsigned int twitter_handle_len,
      unsigned char* locations_buffer, const unsigned int locations_buffer_len,
      unsigned int& locations_len, unsigned int& locations_count
#ifdef LANG_ENABLED
      , char* self_languages_buffer, const unsigned int self_languages_buffer_len,
      unsigned int& self_languages_len, unsigned int& self_languages_count
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
      , char* others_languages_buffer, const unsigned int others_languages_buffer_len,
      unsigned int& others_languages_len, unsigned int& others_languages_count
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
#ifdef RECSYS_ENABLED
      , unsigned char* recommendations_buffer, const unsigned int recommendations_buffer_len,
      unsigned int& recommendations_len, unsigned int& recommendations_count
#endif // RECSYS_ENABLED
      , const char* profile_name) {

  if (!twitter_handle || twitter_handle_len < 1) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: invalid input twitter handle\n";
#endif
    return -1;
  }

  if (!locations_buffer
#ifdef LANG_ENABLED
      || !self_languages_buffer
      || !others_languages_buffer
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
#ifdef RECSYS_ENABLED
      || !recommendations_buffer
#endif // RECSYS_ENABLED
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
  self_languages_buffer[0] = '\0';
  self_languages_len = 0;
  self_languages_count = 0;
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
  others_languages_buffer[0] = '\0';
  others_languages_len = 0;
  others_languages_count = 0;
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
#ifdef RECSYS_ENABLED
  recommendations_buffer[0] = '\0';
  recommendations_len = 0;
  recommendations_count = 0;
#endif // RECSYS_ENABLED

  inagist_classifiers::Corpus corpus;

  bool get_user_info = true;
  /*
  unsigned int init_corpus_size = 0;
  if (profile_name && (strlen(profile_name) > 4)) {
    if ((init_corpus_size = inagist_classifiers::CorpusManager::LoadCorpus(profile_name, corpus)) < 0) {
      std::cerr << "ERROR: could not load corpus from file: " << profile_name << std::endl;
      return -1;
    }
    get_user_info = false;
  }
  */

  std::set<std::string> tweets;
#ifdef RECSYS_ENABLED
  std::set<std::string> self_text_classes_set;
  std::set<std::string> others_text_classes_set;
#endif // RECSYS_ENABLED
  inagist_api::TwitterAPI twitter_api;
  inagist_api::TwitterSearcher twitter_searcher;

  int count = 0;
  int user_info_count = 0;
  unsigned int self_docs_count = 0;
  unsigned int others_docs_count = 0;
  unsigned int corpus_size = 0;

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

#ifdef PROFILE_DEBUG
    if (m_debug_level > 1) {
      std::cout << "self tweets:" << std::endl;
    }
#endif // PROFILE_DEBUG
  if (!tweets.empty()) {
    // generate profile
    if ((count = CallMakeGist(tweets
#ifdef LANG_ENABLED
                    , self_languages_buffer, self_languages_buffer_len,
                    self_languages_len, self_languages_count
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
                    , corpus, corpus_size)) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "WARNING: could not generate corpus for user: " << twitter_handle << std::endl;
#endif
    } else {
      corpus_size += count;
    }

    self_docs_count = tweets.size();
    tweets.clear();

#ifdef RECSYS_ENABLED
    if ((count = inagist_utils::PipeListToSet((unsigned char*) self_text_classes_buffer,
                                              self_text_classes_set)) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "ERROR: PipeListToMap failed\n";
#endif
    }
#endif // RECSYS_ENABLED

    // write corpus to file
    /*
    if (profile_name) {
      if (inagist_classifiers::CorpusManager::UpdateCorpusFile(corpus, profile_name) < 0) {
        std::cerr << "ERROR: could not write corpus to file: " << profile_name << std::endl;
        return -1;
      }
    }
    */
  
    if (!corpus.empty()) {
      corpus.clear();
    }
  
#ifdef PROFILE_DEBUG
    if (PROFILE_DEBUG > 1) {
      std::cout << "corpus of size " << corpus_size \
                << " generated from " << self_docs_count \
                << " tweets of handle: " << twitter_handle << std::endl;
    }
#endif
  }

  // lets give a boost to the user's tweets over the tweets he/she follows
/*
#ifdef RECSYS_ENABLED
  std::map<std::string, double>::iterator map_iter;
  for (map_iter = m_recsys_input_map.begin(); map_iter != m_recsys_input_map.end(); map_iter++) {
    map_iter->second += 1;
  }
#endif // RECSYS_ENABLED
*/
#ifdef RECSYS_ENABLED
  if (GetRecommendations(self_text_classes_set,
                         (unsigned char*) self_text_class_contributors_buffer,
                         self_text_class_contributors_buffer_len,
                         self_text_class_contributors_len,
                         self_text_class_contributors_count) < 0) {
    std::cerr << "ERROR: could not get recommendations\n";
  }
  // self_text_classes_set.clear();
  m_recsys_input_map.clear();
  m_recsys_input_class_map.clear();
#endif // RECSYS_ENABLED

  // handling friends info below

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

#ifdef PROFILE_DEBUG
    if (m_debug_level > 1) {
      std::cout << "other's tweets:" << std::endl;
    }
#endif // PROFILE_DEBUG

    if (!tweets.empty()) {
      // generate profile
      if ((count = CallMakeGist(tweets
#ifdef LANG_ENABLED
                      , others_languages_buffer, others_languages_buffer_len,
                      others_languages_len, others_languages_count
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
                      , corpus, corpus_size)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "WARNING: could not get corpus from user's friends\n";
#endif
      } else {
        corpus_size += count;
      }
    }

#ifdef RECSYS_ENABLED
    if ((count = inagist_utils::PipeListToSet((unsigned char*) others_text_classes_buffer,
                                              others_text_classes_set)) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "ERROR: PipeListToMap failed\n";
#endif
    }
#endif // RECSYS_ENABLED

    others_docs_count = tweets.size();
    tweets.clear();

    if (!corpus.empty()) {
      corpus.clear();
    }
  }

/*
#ifdef RECSYS_ENABLED
  if (GetRecommendations(self_text_classes_set, recommendations_buffer, recommendations_buffer_len,
                         recommendations_len, recommendations_count) < 0) {
    std::cerr << "ERROR: could not get recommendations\n";
  }
  self_text_classes_set.clear();
  m_recsys_input_map.clear();
  m_recsys_input_class_map.clear();
#endif // RECSYS_ENABLED
*/
#ifdef RECSYS_ENABLED
  if (GetRecommendations(self_text_classes_set,
                         (unsigned char*) others_text_class_contributors_buffer,
                         others_text_class_contributors_buffer_len,
                         others_text_class_contributors_len,
                         others_text_class_contributors_count) < 0) {
    std::cerr << "ERROR: could not get recommendations\n";
  }
  self_text_classes_set.clear();
  others_text_classes_set.clear();
  m_recsys_input_map.clear();
  m_recsys_input_class_map.clear();
#endif // RECSYS_ENABLED

#ifdef PROFILE_DEBUG
  if (m_debug_level > 1) {
    std::cout << "corpus of size " << corpus_size \
              << " generated from " << self_docs_count \
              << " docs of user: " << twitter_handle \
              << " and " << others_docs_count << " docs of his/her friends" << std::endl;
  }
#endif

  return corpus_size; 
}

int Profiler::CallMakeGist(std::set<std::string>& tweets
#ifdef LANG_ENABLED
                      , char* lang_class_buffer, const unsigned int lang_class_buffer_len,
                      unsigned int& lang_class_len, unsigned int& lang_class_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                      , char* text_classes_buffer, const unsigned int text_classes_buffer_len,
                      unsigned int& text_classes_len, unsigned int& text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                      , char* location_classes_buffer, const unsigned int location_classes_buffer_len,
                      unsigned int& location_classes_len, unsigned int& location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                      , unsigned char* text_class_contributors_buffer,
                      const unsigned int text_class_contributors_buffer_len,
                      unsigned int& text_class_contributors_len,
                      unsigned int& text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
                      , inagist_classifiers::Corpus& corpus, unsigned int& corpus_size) {

  // TODO (balaji) - make these member variables

#ifdef LANG_ENABLED
  lang_class_buffer[0] = '\0';
  lang_class_len = 0;
  lang_class_count = 0;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  text_classes_buffer[0] = '\0';
  text_classes_len = 0;
  text_classes_count = 0;
  text_class_contributors_buffer[0] = '\0';
  text_class_contributors_len = 0;
  text_class_contributors_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
  location_classes_buffer[0] = '\0';
  location_classes_len = 0;
  location_classes_count = 0;
#endif // LOCATION_ENABLED

  unsigned char text_buffer[MAX_BUFFER_LEN];
  text_buffer[0] = '\0';
  unsigned int text_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_len = 0;

  char profanity_status_buffer[10];
  profanity_status_buffer[0] = '\0';
  unsigned int profanity_status_buffer_len = 10;

  char scripts_buffer[MAX_LIST_LEN];
  memset(scripts_buffer, 0, MAX_LIST_LEN);
  unsigned int scripts_buffer_len = MAX_LIST_LEN;

  unsigned char named_entities_buffer[MAX_BUFFER_LEN];
  named_entities_buffer[0] = '\0';
  unsigned int named_entities_buffer_len = MAX_BUFFER_LEN;
  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;

  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  keywords_buffer[0] = '\0';
  unsigned int keywords_buffer_len = MAX_BUFFER_LEN;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;

  unsigned char keyphrases_buffer[MAX_BUFFER_LEN];
  keyphrases_buffer[0] = '\0';
  unsigned int keyphrases_buffer_len = MAX_BUFFER_LEN;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;

/*
  unsigned char lang_class_words_buffer[MAX_BUFFER_LEN];
  lang_class_words_buffer[0] = '\0';
  unsigned int lang_class_words_buffer_len = MAX_BUFFER_LEN;
  unsigned int lang_class_words_len = 0;
  unsigned int lang_class_words_count = 0;
*/

/*
  char top_lang_classes_buffer[MAX_BUFFER_LEN];
  unsigned int top_lang_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int top_lang_classes_len = 0;
  unsigned int top_lang_classes_count = 0;
*/

/*
  unsigned char lang_class_contributors_buffer[ULTIMATE_BUFFER_LEN];
  lang_class_contributors_buffer[0] = '\0';
  unsigned int lang_class_contributors_buffer_len = ULTIMATE_BUFFER_LEN;
  unsigned int lang_class_contributors_len = 0;
  unsigned int lang_class_contributors_count = 0;

  unsigned char text_class_words_buffer[MAX_BUFFER_LEN];
  text_class_words_buffer[0] = '\0';
  unsigned int text_class_words_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_class_words_len = 0;
  unsigned int text_class_words_count = 0;
*/

  std::set<std::string> scripts_set;
  std::map<std::string, double> lang_classes_map;
  std::map<std::string, double> text_classes_map;
  // std::map<std::string, double> text_class_contributors_map;
  std::set<std::string> text_class_contributors;
#ifdef LOCATION_ENABLED
  std::map<std::string, double> location_classes_map;
#endif // LOCATION_ENABLED
  std::set<std::string>::iterator set_iter;
  std::string tweet;
  std::string language;
  int intent_valence = 0;
  int sentiment_valence = 0;

#ifdef RECSYS_ENABLED
  std::set<std::string> text_classes_set;
  std::set<std::string> named_entities_set;
  std::set<std::string> keywords_set;
#endif // RECSYS_ENABLED

  int count = 0;

  int ret_value = 0;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    tweet.clear();
    tweet.assign(*set_iter);
    if (tweet.length() < 1)
      continue;
    strcpy((char*) text_buffer, tweet.c_str());
    text_len = tweet.length();
#ifdef PROFILE_DEBUG
    if (m_debug_level > 1) {
      std::cout << tweet << std::endl;
    }
#endif // PROFILE_DEBUG
    if ((ret_value = m_gist_maker.MakeGist((unsigned char*) text_buffer, text_buffer_len, text_len,
                  (char*) profanity_status_buffer, profanity_status_buffer_len,
                  (char*) scripts_buffer, scripts_buffer_len,
                  (unsigned char*) named_entities_buffer, named_entities_buffer_len,
                  named_entities_len, named_entities_count,
                  (unsigned char*) keywords_buffer, keywords_buffer_len,
                  keywords_len, keywords_count,
                  (unsigned char*) keyphrases_buffer, keyphrases_buffer_len,
                  keyphrases_len, keyphrases_count
#ifdef LANG_ENABLED
                  , (char*) lang_class_buffer, lang_class_buffer_len,
                  lang_class_len, lang_class_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                  , (char*) text_classes_buffer, text_classes_buffer_len,
                  text_classes_len, text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                  , (char*) location_classes_buffer, location_classes_buffer_len,
                  location_classes_len, location_classes_count
#endif // LOCATION_ENABLED
                  , intent_valence
                  , sentiment_valence
                 )) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "ERROR: could not get named_entities\n";
#endif
    } else {

#ifdef LANG_ENABLED
      if ((count = inagist_utils::PipeListToMap((unsigned char*) lang_class_buffer, lang_classes_map)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: PipeListToMap failed\n";
#endif
      }
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
      if ((count = inagist_utils::PipeListToMap((unsigned char*) text_classes_buffer, text_classes_map)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: PipeListToMap failed\n";
#endif
      }
#endif // TEXT_CLASSIFICATION_ENABLED

      if ((count = inagist_utils::PipeListToSet((unsigned char*) named_entities_buffer,
                                                text_class_contributors)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: PipeListToMap failed\n";
#endif
      }

      if ((count = inagist_utils::PipeListToSet((unsigned char*) keywords_buffer,
                                                text_class_contributors)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: PipeListToMap failed\n";
#endif
      }

#ifdef LOCATION_ENABLED 
      if ((count = inagist_utils::PipeListToMap((unsigned char*) location_classes_buffer, location_classes_map)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: PipeListToMap failed\n";
#endif
      }
#endif // LOCATION_ENABLED

      // this is to purge duplicates
      /*
      if (lang_class_buffer && strlen(lang_class_buffer) > 0) {
        language.assign(lang_class_buffer);
        lang_classes_set.insert(language);
      }
      */

      // TODO (balaji) don't send these crap from script_detection code.
      if (strlen(scripts_buffer) > 0) {
        if (strcmp(scripts_buffer, "en") != 0 &&
            strcmp(scripts_buffer, "UU") != 0 &&
            strcmp(scripts_buffer, "RR") != 0 &&
            strcmp(scripts_buffer, "XX") != 0 &&
            strcmp(scripts_buffer, "xx") != 0) {
          scripts_set.insert(std::string(scripts_buffer));
        }
      }

#ifdef RECSYS_ENABLED
      if ((count = inagist_utils::PipeListToSet((unsigned char*) text_classes_buffer, text_classes_set)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: PipeListToMap failed\n";
#endif
      }

      if ((count = inagist_utils::PipeListToSet((unsigned char*) named_entities_buffer,
                                                named_entities_set)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: PipeListToMap failed\n";
#endif
      }

      if ((count = inagist_utils::PipeListToSet((unsigned char*) keywords_buffer,
                                                keywords_set)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: PipeListToMap failed\n";
#endif
      }

      if (TrainRecSys(tweet,
                      named_entities_set,
                      keywords_set,
                      text_classes_set,
                      intent_valence,
                      sentiment_valence) < 0) {
        std::cout << "ERROR: could not find RecSys input\n";
      }
      named_entities_set.clear();
      keywords_set.clear();
      text_classes_set.clear();
      intent_valence = 0;
      sentiment_valence = 0;
#endif // RECSYS_ENABLED

    }

    profanity_status_buffer[0] = '\0';
    scripts_buffer[0] = '\0';
#ifdef LANG_ENABLED
    lang_class_buffer[0] = '\0';
    lang_class_len = 0;
    lang_class_count = 0;
#endif // LANG_ENABLED
    named_entities_buffer[0] = '\0';
    keywords_buffer[0] = '\0';
    keyphrases_buffer[0] = '\0';
#ifdef TEXT_CLASSIFCATION_ENABLED
    text_classes_buffer[0] = '\0';
#endif // TEXT_CLASSIFCATION_ENABLED
#ifdef LOCATION_ENABLED
    location_classes_buffer[0] = '\0';
#endif // LOCATION_ENABLED
  }

/*
  if (inagist_utils::MapToPipeList(text_classes_map,
                                   (unsigned char*) text_classes_buffer, text_classes_buffer_len,
                                   text_classes_len, text_classes_count) < 0) {
    std::cerr << "ERROR: could not populate set elements to pipe separated list\n";
  }
*/

  unsigned int n = 1;
#ifdef LANG_ENABLED
  lang_class_buffer[0] = '\0';
  lang_class_len = 0;
  lang_class_count = 0;
  if (inagist_utils::FindTopN(lang_classes_map, n,
                              lang_class_buffer, lang_class_buffer_len,
                              lang_class_len, lang_class_count) < 0) {
    std::cerr << "ERROR: could not get top 3 elements\n";
  }
  lang_classes_map.clear();

  for (set_iter = scripts_set.begin(); set_iter != scripts_set.end(); set_iter++) {
    language = *set_iter;
    inagist_utils::InsertIntoBuffer(language,
                                    (unsigned char*) lang_class_buffer, lang_class_buffer_len,
                                    lang_class_len, lang_class_count);
  }
  scripts_set.clear();
#ifdef PROFILE_DEBUG
  if (m_debug_level > 2) {
    std::cout << "lang_class_count: " << lang_class_count << std::endl;
  }
#endif
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  n = 3;
  text_classes_buffer[0] = '\0';
  text_classes_len = 0;
  text_classes_count = 0;
  if (inagist_utils::FindTopN(text_classes_map, n,
                              text_classes_buffer, text_classes_buffer_len,
                              text_classes_len, text_classes_count) < 0) {
    std::cerr << "ERROR: could not get top 3 elements\n";
  }
  text_classes_map.clear();

#ifdef PROFILE_DEBUG
  if (m_debug_level > 2) {
    std::cout << "text_classes_count: " << text_classes_count << std::endl;
  }
#endif

  if ((count = inagist_utils::PipeListToMap((unsigned char*) text_classes_buffer, text_classes_map)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: PipeListToMap failed\n";
#endif
  }

  if (inagist_utils::SetToPipeList(text_class_contributors,
                                   (unsigned char*) text_class_contributors_buffer, text_class_contributors_buffer_len,
                                   text_class_contributors_len, text_class_contributors_count) < 0) {
    std::cerr << "ERROR: could not populate set elements to pipe separated list\n";
  }
  text_class_contributors.clear();
#ifdef PROFILE_DEBUG
  if (m_debug_level > 2) {
    std::cout << "text_class_contributors_count: " << text_class_contributors_count << std::endl;
  }
#endif
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LOCATION_ENABLED
  if (inagist_utils::MapToPipeList(location_classes_map,
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

  corpus_size = corpus.size();

  return corpus_size;
}

#ifdef RECSYS_ENABLED
// no training as such is happening here. its merely a data store at this point
int Profiler::TrainRecSys(std::string& tweet,
                          std::set<std::string>& named_entities_set,
                          std::set<std::string>& keywords_set,
                          std::set<std::string>& text_classes_set,
                          int intent_valence,
                          int sentiment_valence) {

  std::set<std::string>::iterator set_iter;
  std::set<std::string>::iterator class_iter;
  std::map<std::string, double>::iterator map_iter;
  std::string key;

  int tweet_valence = 0;
  if (tweet.find("RT ") == std::string::npos) {
    tweet_valence += 2;
  }

  int intent_weight = 0;
  if (intent_valence > 3) {
    intent_weight = 5;
  }

  int sentiment_weight = 0;
  if (sentiment_valence > 3) {
    sentiment_weight = 1;
  } else if (sentiment_valence < 0) {
    sentiment_weight = -1;
  }

  int value = intent_weight + sentiment_weight;

  // named entities
  value += 3;
  for (set_iter = named_entities_set.begin(); set_iter != named_entities_set.end(); set_iter++) {
    key = *set_iter;
    if ((map_iter = m_recsys_input_map.find(key)) != m_recsys_input_map.end()) {
      map_iter->second += value;
    } else {
      m_recsys_input_map.insert(std::pair<std::string, double> (key, value));
    }
  }

  value = intent_weight + sentiment_weight;
  value += 1;
  for (set_iter = keywords_set.begin(); set_iter != keywords_set.end(); set_iter++) {
    key = *set_iter;
    if ((map_iter = m_recsys_input_map.find(key)) != m_recsys_input_map.end()) {
      map_iter->second += value;
    } else {
      m_recsys_input_map.insert(std::pair<std::string, double> (key, value));
    }
  }

  std::string class_name;
  for (class_iter = text_classes_set.begin(); class_iter != text_classes_set.end(); class_iter++) {
    class_name = *class_iter;
    for (set_iter = named_entities_set.begin(); set_iter != named_entities_set.end(); set_iter++) {
      key = *set_iter;
      m_recsys_input_class_map.insert(std::pair<std::string, std::string> (class_name, key));
    }
    for (set_iter = keywords_set.begin(); set_iter != keywords_set.end(); set_iter++) {
      key = *set_iter;
      m_recsys_input_class_map.insert(std::pair<std::string, std::string> (class_name, key));
    }
  }

  return 0;
}
#endif // RECSYS_ENABLED

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

bool Profiler::SortFunction(std::string i, std::string j) { 
  return (i<j);
}
*/

#ifdef RECSYS_ENABLED
int Profiler::GetRecommendations(std::set<std::string>& text_classes_set,
                                 unsigned char* recommendations_buffer,
                                 const unsigned int& recommendations_buffer_len,
                                 unsigned int& recommendations_len,
                                 unsigned int& recommendations_count) {

  std::multimap<std::string, std::string>::iterator mmap_iter;
  std::map<std::string, double>::iterator map_iter;
  std::string class_name;
  std::string keyword;
  for (mmap_iter = m_recsys_input_class_map.begin();
       mmap_iter != m_recsys_input_class_map.end();
       mmap_iter++) {
    class_name = mmap_iter->first;
    if (text_classes_set.find(class_name) == text_classes_set.end()) {
      continue;
    }
    keyword = mmap_iter->second;
    if ((map_iter = m_recsys_input_map.find(keyword)) != m_recsys_input_map.end()) {
#ifdef PROFILE_DEBUG
      if (m_debug_level > 3) {
        std::cout << keyword << " belongs to " << class_name << std::endl;
      }
#endif // PROFILE_DEBUG
      if (class_name.compare("IAB12 News") != 0 &&
          class_name.compare("IAB0 Uncategorized") != 0) {
        map_iter->second += 2;
      }
    }
  }

  //std::multimap<double, std::string> recommendations_map;
  //std::vector<std::string> recommendations_vector;
  //std::vector<std::string>::iterator vector_iter;
  std::set<std::string> recommendations_set;
  std::set<std::string>::iterator set_iter;
  char score_reco_array[128];
  std::string score_reco_str;
  int score = 0;
  std::string reco;
  for (map_iter = m_recsys_input_map.begin();
       map_iter != m_recsys_input_map.end();
       map_iter++) {
#ifdef PROFILE_DEBUG
      if (m_debug_level > 3) {
        std::cout << map_iter->first << " : " << map_iter->second << std::endl;
      }
#endif // PROFILE_DEBUG
    //recommendations_map.insert(std::pair<double, std::string>(map_iter->second, map_iter->first));
    reco = map_iter->first;
    if (reco.length() > 100 || reco.length() < 1)
      continue;
    memset(score_reco_array, '\0', 128);
    score = (int) map_iter->second;
    if (score > 9)
      score = 9;
    sprintf(score_reco_array, "%d%s", score, reco.c_str());
    score_reco_str = std::string(score_reco_array);
    //vector_iter = recommendations_vector.begin();
    //recommendations_vector.insert(vector_iter, score_reco_str);
    recommendations_set.insert(score_reco_str);
  }
  //std::sort(recommendations_vector.begin(), recommendations_vector.end(), SortFunction);

  for (set_iter = recommendations_set.begin(); set_iter != recommendations_set.end();
       set_iter++) {
    // std::cout << *set_iter << std::endl;
    score_reco_str = *set_iter;
    score_reco_str.erase(0,1); 
    inagist_utils::InsertIntoBuffer(score_reco_str,
                                    (unsigned char*) recommendations_buffer, recommendations_buffer_len,
                                    recommendations_len, recommendations_count);
  }
  recommendations_set.clear();

/*
  for (vector_iter = recommendations_vector.begin(); vector_iter != recommendations_vector.end();
       vector_iter++) {
    std::cout << *vector_iter << std::endl;
  }
  recommendations_vector.clear();
*/

  /*
  if (MultiMapToPipeList(recommendations_map, recommendations_buffer, recommendations_buffer_len,
                         recommendations_len, recommendations_count) < 0) {
    std::cerr << "ERROR: could not write multimap to pipe list\n";
  }
  */
  //recommendations_map.clear();

  return 0;
}
#endif // RECSYS_ENABLED

} // namespace inagist_dashboard
