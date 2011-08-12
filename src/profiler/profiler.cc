#include "profiler.h"
#include <cstring>
#include <cstdlib>
#include "corpus_manager.h"
#include "inagist_api.h"
#include "twitter_api.h"
#include "twitter_searcher.h"
#include "string_utils.h"

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
}

int Profiler::Init(const char* keytuples_extracter_config,
                   const char* language_detection_config,
                   const char* self_text_classification_config) {

  if (!keytuples_extracter_config ||
      !language_detection_config ||
      !self_text_classification_config) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif
    return -1;
  }

  if (m_gist_maker.Init(keytuples_extracter_config,
                        language_detection_config,
                        self_text_classification_config) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not initialize gist maker\n";
#endif
    return -1;
  }

  return 0;
}

int Profiler::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  return 0;
}

// given a twitter handle this calls other function to produce a corpus, classify and
// then write the corpus to the output file.
// the text classifier must have already been initialized before this is called
int Profiler::Profile(const std::string& twitter_handle,
                      std::set<std::string>& locations,
                      std::set<std::string>& self_languages,
                      std::set<std::string>& self_text_classes,
                      std::set<std::string>& self_sub_classes,
                      std::map<std::string, std::string>& self_text_class_contributors_map,
                      std::set<std::string>& others_languages,
                      std::set<std::string>& others_text_classes,
                      std::set<std::string>& others_sub_classes,
                      std::map<std::string, std::string>& others_text_class_contributors_map,
                      std::string& intent,
                      std::string& sentiment,
                      std::set<std::string>& recommendations,
                      const std::string& profile_name) {

  char intent_buffer[10];
  intent_buffer[0] = '\0';
  unsigned int intent_buffer_len = 10;

  char sentiment_buffer[10];
  sentiment_buffer[0] = '\0';
  unsigned int sentiment_buffer_len = 10;

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

  char self_sub_classes_buffer[MAX_BUFFER_LEN];
  self_sub_classes_buffer[0] = '\0';
  unsigned int self_sub_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int self_sub_classes_len = 0;
  unsigned int self_sub_classes_count = 0;

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

  char others_sub_classes_buffer[MAX_BUFFER_LEN];
  others_sub_classes_buffer[0] = '\0';
  unsigned int others_sub_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int others_sub_classes_len = 0;
  unsigned int others_sub_classes_count = 0;

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
                             self_languages_buffer, self_languages_buffer_len,
                             self_languages_len, self_languages_count,
                             self_text_classes_buffer, self_text_classes_buffer_len,
                             self_text_classes_len, self_text_classes_count,
                             self_sub_classes_buffer, self_sub_classes_buffer_len,
                             self_sub_classes_len, self_sub_classes_count,
                             self_text_class_contributors_buffer,
                             self_text_class_contributors_buffer_len,
                             self_text_class_contributors_len,
                             self_text_class_contributors_count,
                             others_languages_buffer, others_languages_buffer_len,
                             others_languages_len, others_languages_count,
                             others_text_classes_buffer, others_text_classes_buffer_len,
                             others_text_classes_len, others_text_classes_count,
                             others_sub_classes_buffer, others_sub_classes_buffer_len,
                             others_sub_classes_len, others_sub_classes_count,
                             others_text_class_contributors_buffer,
                             others_text_class_contributors_buffer_len,
                             others_text_class_contributors_len,
                             others_text_class_contributors_count,
                             intent_buffer, intent_buffer_len,
                             sentiment_buffer, sentiment_buffer_len,
                             recommendations_buffer, recommendations_buffer_len,
                             recommendations_len, recommendations_count,
                             profile_name.c_str())) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not generate profile\n";
#endif
    return -1;
  }

  //safe_status = std::string(safe_status_buffer);
  if ((inagist_utils::PipeListToSet(locations_buffer, locations)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate locations\n";
#endif
  }
  locations_buffer[0] = '\0';
  if ((inagist_utils::PipeListToSet((unsigned char*) self_languages_buffer,
                                            self_languages)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate languages\n";
#endif
  }
  self_languages_buffer[0] = '\0';
  if ((inagist_utils::PipeListToSet((unsigned char*) self_text_classes_buffer,
                                            self_text_classes)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate self_text_classes\n";
#endif
  }
  self_text_classes_buffer[0] = '\0';
  if ((inagist_utils::PipeListToSet((unsigned char*) self_sub_classes_buffer,
                                            self_sub_classes)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate self_text_classes\n";
#endif
  }
  self_sub_classes_buffer[0] = '\0';
  if ((inagist_utils::PipeListToSet((unsigned char*) others_languages_buffer,
                                            others_languages)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate languages\n";
#endif
  }
  others_languages_buffer[0] = '\0';
  if ((inagist_utils::PipeListToSet((unsigned char*) others_text_classes_buffer,
                                            others_text_classes)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate others_text_classes\n";
#endif
  }
  others_text_classes_buffer[0] = '\0';
  if ((inagist_utils::PipeListToSet((unsigned char*) others_sub_classes_buffer,
                                            others_sub_classes)) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate others_text_classes\n";
#endif
  }
  others_sub_classes_buffer[0] = '\0';

  intent = std::string(intent_buffer);
  sentiment = std::string(sentiment_buffer);

  // unused in this call
  self_text_class_contributors_buffer[0] = '\0';
  others_text_class_contributors_buffer[0] = '\0';
  recommendations_buffer[0] = '\0';

  return 0;
}

int Profiler::Profile(const char* twitter_handle, unsigned int twitter_handle_len,
      unsigned char* locations_buffer, const unsigned int locations_buffer_len,
      unsigned int& locations_len, unsigned int& locations_count,
      char* self_languages_buffer, const unsigned int self_languages_buffer_len,
      unsigned int& self_languages_len, unsigned int& self_languages_count,
      char* self_text_classes_buffer, const unsigned int self_text_classes_buffer_len,
      unsigned int& self_text_classes_len, unsigned int& self_text_classes_count,
      char* self_sub_classes_buffer, const unsigned int self_sub_classes_buffer_len,
      unsigned int& self_sub_classes_len, unsigned int& self_sub_classes_count,
      unsigned char* self_text_class_contributors_buffer,
      const unsigned int self_text_class_contributors_buffer_len,
      unsigned int& self_text_class_contributors_len,
      unsigned int& self_text_class_contributors_count,
      char* others_languages_buffer, const unsigned int others_languages_buffer_len,
      unsigned int& others_languages_len, unsigned int& others_languages_count,
      char* others_text_classes_buffer, const unsigned int others_text_classes_buffer_len,
      unsigned int& others_text_classes_len, unsigned int& others_text_classes_count,
      char* others_sub_classes_buffer, const unsigned int others_sub_classes_buffer_len,
      unsigned int& others_sub_classes_len, unsigned int& others_sub_classes_count,
      unsigned char* others_text_class_contributors_buffer,
      const unsigned int others_text_class_contributors_buffer_len,
      unsigned int& others_text_class_contributors_len,
      unsigned int& others_text_class_contributors_count,
      char* intent_buffer, const unsigned int intent_buffer_len,
      char* sentiment_buffer, const unsigned int sentiment_buffer_len,
      unsigned char* recommendations_buffer, const unsigned int recommendations_buffer_len,
      unsigned int& recommendations_len, unsigned int& recommendations_count,
      const char* profile_name) {

  if (!twitter_handle || twitter_handle_len < 1) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: invalid input twitter handle\n";
#endif
    return -1;
  }

  if (!self_languages_buffer ||
      !self_text_classes_buffer ||
      !self_sub_classes_buffer ||
      !self_text_class_contributors_buffer ||
      !others_languages_buffer ||
      !others_text_classes_buffer ||
      !others_sub_classes_buffer ||
      !others_text_class_contributors_buffer ||
      !intent_buffer ||
      !sentiment_buffer ||
      !recommendations_buffer) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: invalid output buffer(s) for profiling\n";
#endif
    return -1;
  }

  locations_buffer[0] = '\0';
  locations_len = 0;
  locations_count = 0;
  self_languages_buffer[0] = '\0';
  self_languages_len = 0;
  self_languages_count = 0;
  self_text_classes_buffer[0] = '\0';
  self_text_classes_len = 0;
  self_text_classes_count = 0;
  self_sub_classes_buffer[0] = '\0';
  self_sub_classes_len = 0;
  self_sub_classes_count = 0;
  self_text_class_contributors_buffer[0] = '\0';
  self_text_class_contributors_len = 0;
  self_text_class_contributors_count = 0;
  others_languages_buffer[0] = '\0';
  others_languages_len = 0;
  others_languages_count = 0;
  others_text_classes_buffer[0] = '\0';
  others_text_classes_len = 0;
  others_text_classes_count = 0;
  others_sub_classes_buffer[0] = '\0';
  others_sub_classes_len = 0;
  others_sub_classes_count = 0;
  others_text_class_contributors_buffer[0] = '\0';
  others_text_class_contributors_len = 0;
  others_text_class_contributors_count = 0;
  intent_buffer[0] = '\0';
  sentiment_buffer[0] = '\0';
  recommendations_buffer[0] = '\0';
  recommendations_len = 0;
  recommendations_count = 0;

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

  if (!tweets.empty()) {
    // generate profile
    if ((count = GetGist(tweets,
                    self_languages_buffer, self_languages_buffer_len,
                    self_languages_len, self_languages_count,
                    self_text_classes_buffer, self_text_classes_buffer_len,
                    self_text_classes_len, self_text_classes_count,
                    self_sub_classes_buffer, self_sub_classes_buffer_len,
                    self_sub_classes_len, self_sub_classes_count,
                    self_text_class_contributors_buffer, self_text_class_contributors_buffer_len,
                    self_text_class_contributors_len, self_text_class_contributors_count,
                    intent_buffer, intent_buffer_len,
                    sentiment_buffer, sentiment_buffer_len,
                    corpus, corpus_size)) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "WARNING: could not generate corpus for user: " << twitter_handle << std::endl;
#endif
    } else {
        if (m_debug_level > 3) {
          std::cout << "self_text_class_contributors_buffer:" \
                    << self_text_class_contributors_buffer << std::endl;
        }
      corpus_size += count;
    }

    self_docs_count = tweets.size();
    tweets.clear();
  
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

    if (!tweets.empty()) {
      // generate profile
      if ((count = GetGist(tweets,
                      others_languages_buffer, others_languages_buffer_len,
                      others_languages_len, others_languages_count,
                      others_text_classes_buffer, others_text_classes_buffer_len,
                      others_text_classes_len, others_text_classes_count,
                      others_sub_classes_buffer, others_sub_classes_buffer_len,
                      others_sub_classes_len, others_sub_classes_count,
                      others_text_class_contributors_buffer, others_text_class_contributors_buffer_len,
                      others_text_class_contributors_len, others_text_class_contributors_count,
                      intent_buffer, intent_buffer_len,
                      sentiment_buffer, sentiment_buffer_len,
                      corpus, corpus_size)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "WARNING: could not get corpus from user's friends\n";
#endif
      } else {
#ifdef PROFILE_DEBUG
        if (m_debug_level > 3) {
          std::cout << "others_text_class_contributors_buffer:" \
                    << others_text_class_contributors_buffer << std::endl;
        }
        corpus_size += count;
#endif
      }
    }

    others_docs_count = tweets.size();
    tweets.clear();

    if (!corpus.empty()) {
      corpus.clear();
    }
  }

#ifdef PROFILE_DEBUG
  if (m_debug_level > 1) {
    std::cout << "corpus of size " << corpus_size \
              << " generated from " << self_docs_count \
              << " docs of user: " << twitter_handle \
              << " and " << others_docs_count << " of his/her friends" << std::endl;
  }
#endif

  return corpus_size; 
}

int Profiler::ProfileFromFile(const char* docs_file_name, unsigned int docs_file_name_len,
      unsigned char* locations_buffer, const unsigned int locations_buffer_len,
      unsigned int& locations_len, unsigned int& locations_count,
      char* self_languages_buffer, const unsigned int self_languages_buffer_len,
      unsigned int& self_languages_len, unsigned int& self_languages_count,
      char* self_text_classes_buffer, const unsigned int self_text_classes_buffer_len,
      unsigned int& self_text_classes_len, unsigned int& self_text_classes_count,
      char* self_sub_classes_buffer, const unsigned int self_sub_classes_buffer_len,
      unsigned int& self_sub_classes_len, unsigned int& self_sub_classes_count,
      unsigned char* self_text_class_contributors_buffer,
      const unsigned int self_text_class_contributors_buffer_len,
      unsigned int& self_text_class_contributors_len,
      unsigned int& self_text_class_contributors_count,
      char* intent_buffer, const unsigned int intent_buffer_len,
      char* sentiment_buffer, const unsigned int sentiment_buffer_len,
      unsigned char* recommendations_buffer, const unsigned int recommendations_buffer_len,
      unsigned int& recommendations_len, unsigned int& recommendations_count,
      const char* profile_name) {

  if (!docs_file_name || docs_file_name_len < 1) {
     std::cerr << "ERROR: invalid input\n";
     return -1;
  }

  strcpy((char *) locations_buffer, "bleh, not implemented yet!");
  locations_len = strlen((char *) locations_buffer);
  locations_count = 1;

  self_languages_buffer[0] = '\0';
  self_languages_len = 0;
  self_languages_count = 0;
  self_text_classes_buffer[0] = '\0';
  self_text_classes_len = 0;
  self_text_classes_count = 0;
  self_sub_classes_buffer[0] = '\0';
  self_sub_classes_len = 0;
  self_sub_classes_count = 0;
  self_text_class_contributors_buffer[0] = '\0';
  self_text_class_contributors_len = 0;
  self_text_class_contributors_count = 0;
  intent_buffer[0] = '\0';
  sentiment_buffer[0] = '\0';

  strcpy((char *) recommendations_buffer, "bleh, not implemented yet!");
  recommendations_len = strlen((char *) recommendations_buffer);
  recommendations_count = 1;

  std::ifstream ifs(docs_file_name);
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open input file: " << docs_file_name << std::endl;
    return -1;
  }
  std::set<std::string> tweets;
  std::string line;
  while (getline(ifs, line)) {
    tweets.insert(line);
  }
  ifs.close();

  if (tweets.empty()) {
    return 0;
  }

  inagist_classifiers::Corpus corpus;
  int init_corpus_size = 0;

  if (profile_name && (strlen(profile_name) > 4)) {
    if ((init_corpus_size = inagist_classifiers::CorpusManager::LoadCorpus(profile_name, corpus)) < 0) {
      std::cerr << "ERROR: could not load corpus from file: " << profile_name << std::endl;
      return -1;
    }
  }

  // genrate profile
  int count = 0;
  unsigned int corpus_size = 0;
  if ((count += GetGist(tweets,
                        self_languages_buffer, self_languages_buffer_len,
                        self_languages_len, self_languages_count,
                        self_text_classes_buffer, self_text_classes_buffer_len,
                        self_text_classes_len, self_text_classes_count,
                        self_sub_classes_buffer, self_sub_classes_buffer_len,
                        self_sub_classes_len, self_sub_classes_count,
                        self_text_class_contributors_buffer, self_text_class_contributors_buffer_len,
                        self_text_class_contributors_len, self_text_class_contributors_count,
                        intent_buffer, intent_buffer_len,
                        sentiment_buffer, sentiment_buffer_len,
                        corpus, corpus_size)) < 0) {
    std::cerr << "ERROR: could not generate profile\n";
    return -1;
  }

  tweets.clear();

  // write corpus to file
  if (inagist_classifiers::CorpusManager::UpdateCorpusFile(corpus, profile_name) < 0) {
    std::cerr << "ERROR: could not write corpus to file: " << profile_name << std::endl;
    return -1;
  }

#ifdef PROFILE_DEBUG
  if (PROFILE_DEBUG > 1) {
    std::cout << "corpus of size " << corpus_size \
              << " generated from " << tweets.size() \
              << " tweets in file: " << docs_file_name << std::endl;
  }
#endif

  if (!corpus.empty()) {
    corpus.clear();
  }

  return corpus_size; 
}

int Profiler::GetGist(std::set<std::string>& tweets,
                      char* lang_class_buffer, const unsigned int lang_class_buffer_len,
                      unsigned int& lang_class_len, unsigned int& lang_class_count,
                      char* text_classes_buffer, const unsigned int text_classes_buffer_len,
                      unsigned int& text_classes_len, unsigned int& text_classes_count,
                      char* sub_classes_buffer, const unsigned int sub_classes_buffer_len,
                      unsigned int& sub_classes_len, unsigned int& sub_classes_count,
                      unsigned char* text_class_contributors_buffer,
                      const unsigned int text_class_contributors_buffer_len,
                      unsigned int& text_class_contributors_len,
                      unsigned int& text_class_contributors_count,
                      char* intent_buffer, const unsigned int intent_buffer_len,
                      char* sentiment_buffer, const unsigned int sentiment_buffer_len,
                      inagist_classifiers::Corpus& corpus, unsigned int& corpus_size) {

  // TODO (balaji) - make these member variables

  lang_class_buffer[0] = '\0';
  lang_class_len = 0;
  lang_class_count = 0;
  text_classes_buffer[0] = '\0';
  text_classes_len = 0;
  text_classes_count = 0;
  sub_classes_buffer[0] = '\0';
  sub_classes_len = 0;
  sub_classes_count = 0;
  text_class_contributors_buffer[0] = '\0';
  text_class_contributors_len = 0;
  text_class_contributors_count = 0;
  intent_buffer[0] = '\0';
  sentiment_buffer[0] = '\0';

  unsigned char text_buffer[MAX_BUFFER_LEN];
  text_buffer[0] = '\0';
  unsigned int text_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_len = 0;

  char safe_status_buffer[10];
  safe_status_buffer[0] = '\0';
  unsigned int safe_status_buffer_len = 10;

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

  unsigned char lang_class_words_buffer[MAX_BUFFER_LEN];
  lang_class_words_buffer[0] = '\0';
  unsigned int lang_class_words_buffer_len = MAX_BUFFER_LEN;
  unsigned int lang_class_words_len = 0;
  unsigned int lang_class_words_count = 0;

  char top_lang_classes_buffer[MAX_BUFFER_LEN];
  unsigned int top_lang_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int top_lang_classes_len = 0;
  unsigned int top_lang_classes_count = 0;

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

  std::set<std::string> lang_classes_set;
  std::map<std::string, double> sub_classes_map;
  std::set<std::string>::iterator set_iter;
  std::string tweet;
  std::string language;
  int count = 0;

  int ret_value = 0;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    tweet.clear();
    tweet.assign(*set_iter);
    if (tweet.length() < 1)
      continue;
    strcpy((char*) text_buffer, tweet.c_str());
    text_len = tweet.length();
    if ((ret_value = m_gist_maker.GetGist((unsigned char*) text_buffer, text_buffer_len, text_len,
                  (char*) safe_status_buffer, safe_status_buffer_len,
                  (char*) scripts_buffer, scripts_buffer_len,
                  (unsigned char*) named_entities_buffer, named_entities_buffer_len,
                  &named_entities_len, &named_entities_count,
                  (unsigned char*) keywords_buffer, keywords_buffer_len,
                  &keywords_len, &keywords_count,
                  (unsigned char*) keyphrases_buffer, keyphrases_buffer_len,
                  &keyphrases_len, &keyphrases_count,
                  (unsigned char*) lang_class_words_buffer, lang_class_words_buffer_len,
                  &lang_class_words_len, &lang_class_words_count,
                  (char*) lang_class_buffer, lang_class_buffer_len,
                  &lang_class_len, &lang_class_count,
                  (char*) top_lang_classes_buffer, top_lang_classes_buffer_len,
                  &top_lang_classes_len, &top_lang_classes_count,
                  (unsigned char*) lang_class_contributors_buffer, lang_class_contributors_buffer_len,
                  &lang_class_contributors_len, &lang_class_contributors_count,
                  // using the text_classes_buffer and sub_classes_buffer here to avoid another allocation.
                  // they'll be cleaned up shortly
                  (unsigned char*) text_class_words_buffer, text_class_words_buffer_len,
                  &text_class_words_len, &text_class_words_count,
                  (char*) text_classes_buffer, text_classes_buffer_len,
                  &text_classes_len, &text_classes_count,
                  (char*) sub_classes_buffer, sub_classes_buffer_len,
                  &sub_classes_len, &sub_classes_count,
                  (unsigned char*) text_class_contributors_buffer, text_class_contributors_buffer_len,
                  &text_class_contributors_len, &text_class_contributors_count,
                  (char*) intent_buffer, intent_buffer_len,
                  (char*) sentiment_buffer, sentiment_buffer_len)) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "ERROR: could not get named_entities\n";
#endif
    } else {

      if ((count = inagist_utils::PipeListToMap((unsigned char*) text_classes_buffer, sub_classes_map)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: PipeListToMap failed\n";
#endif
      }

      if ((count = inagist_utils::PipeListToMap(text_class_words_buffer, corpus)) < 0) {
#ifdef PROFILE_DEBUG
        std::cerr << "ERROR: could not populate corpus with text_class_contributors\n";
#endif
      } else {
        if (m_debug_level > 3) {
          std::cout << "text_class_words_buffer:" \
                    << text_class_words_buffer << std::endl;
        }
        corpus_size += count;
      }

      // this is to purge duplicates
      if (lang_class_buffer && strlen(lang_class_buffer) > 0) {
        language.assign(lang_class_buffer);
        lang_classes_set.insert(language);
      }

      // TODO (balaji) don't send these crap from script_detection code.
      if (strlen(scripts_buffer) > 0) {
        if (strcmp(scripts_buffer, "en") != 0 &&
            strcmp(scripts_buffer, "UU") != 0 &&
            strcmp(scripts_buffer, "RR") != 0 &&
            strcmp(scripts_buffer, "XX") != 0 &&
            strcmp(scripts_buffer, "xx") != 0) {
          language.assign(scripts_buffer);
          lang_classes_set.insert(language);
        }
      }
    }

    safe_status_buffer[0] = '\0';
    scripts_buffer[0] = '\0';
    lang_class_buffer[0] = '\0';
    lang_class_len = 0;
    lang_class_count = 0;
    lang_class_words_buffer[0] = '\0';
    top_lang_classes_buffer[0] = '\0';
    lang_class_contributors_buffer[0] = '\0';
    named_entities_buffer[0] = '\0';
    keywords_buffer[0] = '\0';
    keyphrases_buffer[0] = '\0';
    text_class_words_buffer[0] = '\0';
    text_classes_buffer[0] = '\0';
    sub_classes_buffer[0] = '\0';
    text_class_contributors_buffer[0] = '\0';
  }

  // TODO (balaji) - another call to find text_classes for the whole corpus, relook
#ifdef PROFILE_DEBUG
  std::cout << "corpus_size: " << corpus.size() << std::endl;
#endif

  if (corpus.size() <= 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: empty corpus from individual docs. can't find overall class\n";
#endif
  } else {
    // TODO (balaji) forgot what should be sent here. sending some variable. fix this later
    if (m_gist_maker.FindTextClasses(corpus,
                                     text_classes_buffer, text_classes_buffer_len,
                                     text_classes_len, text_classes_count,
                                     text_class_contributors_buffer, text_class_contributors_buffer_len,
                                     text_class_contributors_len, text_class_contributors_count
                                    ) < 0) {
#ifdef PROFILE_DEBUG
      std::cerr << "ERROR: couldn't find text classes\n";
#endif
    }
  }

  // not clearing corpus, becos its a return parameter
#ifdef PROFILE_DEBUG
  std::cout << "text_classes_count: " << text_classes_count << std::endl;
  std::cout << "text_classe_contributors_count: " << text_class_contributors_count << std::endl;
#endif

  if (inagist_utils::MapToPipeList(sub_classes_map,
                                   (unsigned char*) sub_classes_buffer, sub_classes_buffer_len,
                                   sub_classes_len, sub_classes_count) < 0) {
    std::cerr << "ERROR: could not populate set elements to pipe separated list\n";
  }
  sub_classes_map.clear();
#ifdef PROFILE_DEBUG
  std::cout << "sub_classes_count: " << sub_classes_count << std::endl;
#endif

  if (inagist_utils::SetToPipeList(lang_classes_set,
                                   (unsigned char*) lang_class_buffer, lang_class_buffer_len,
                                   lang_class_len, lang_class_count) < 0) {
#ifdef PROFILE_DEBUG
    std::cerr << "ERROR: could not populate set elements to pipe separated list\n";
#endif
  }
  lang_classes_set.clear();
#ifdef PROFILE_DEBUG
  std::cout << "lang_class_count: " << lang_class_count << std::endl;
#endif

  corpus_size = corpus.size();

  return corpus_size;
}

} // namespace inagist_dashboard
