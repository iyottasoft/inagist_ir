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

//#define PROFILE_DEBUG 3

#define MAX_CLASS_NAME 32
#define MAX_LIST_LEN  255
#define ULTIMATE_BUFFER_LEN 10240

namespace inagist_dashboard {

Profiler::Profiler() {
}

Profiler::~Profiler() {
}

int Profiler::Init(const char* keytuples_extracter_config,
                   const char* language_detection_config,
                   const char* self_text_classification_config,
                   const char* sentiment_analyser_config) {

  if (!keytuples_extracter_config ||
      !language_detection_config ||
      !self_text_classification_config) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif
    return -1;
  }

  if (m_gist_maker.Init(keytuples_extracter_config,
                        language_detection_config,
                        self_text_classification_config,
                        sentiment_analyser_config) < 0) {
    std::cerr << "ERROR: could not initialize gist maker\n";
    return -1;
  }

  return 0;
}

/*
// given a twitter handle this calls other function to produce a corpus, classify and
// then write the corpus to the output file.
// the text classifier must have already been initialized before this is called
int Profiler::Profile(const std::string& twitter_handle,
                      std::set<std::string>& locations,
                      std::set<std::string>& languages,
                      std::set<std::string>& self_text_classes,
                      std::set<std::string>& self_sub_classes,
                      std::map<std::string, std::string>& self_text_class_contributors_map,
                      std::string& sentiment,
                      std::set<std::string>& recommendations,
                      const std::string& profile_name) {

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
                             sentiment_buffer, sentiment_buffer_len,
                             recommendations_buffer, recommendations_buffer_len,
                             recommendations_len, recommendations_count,
                             profile_name.c_str())) < 0) {
    std::cerr << "ERROR: could not generate profile\n";
    return -1;
  } else {
    //safe_status = std::string(safe_status_buffer);
    if ((inagist_utils::PipeListToSet(locations_buffer, locations)) < 0) {
      std::cerr << "ERROR: could not populate locations\n";
    }
    if ((inagist_utils::PipeListToSet((unsigned char*) self_languages_buffer,
                                              languages)) < 0) {
      std::cerr << "ERROR: could not populate languages\n";
    }
    if ((inagist_utils::PipeListToSet((unsigned char*) self_text_classes_buffer,
                                              self_text_classes)) < 0) {
      std::cerr << "ERROR: could not populate self_text_classes\n";
    }
    if ((inagist_utils::PipeListToSet((unsigned char*) self_sub_classes_buffer,
                                              self_sub_classes)) < 0) {
      std::cerr << "ERROR: could not populate self_text_classes\n";
    }
    sentiment = std::string(sentiment_buffer);
  }
  locations_buffer[0] = '\0';
  self_languages_buffer[0] = '\0';
  self_text_classes_buffer[0] = '\0';
  self_sub_classes_buffer[0] = '\0';
  self_text_class_contributors_buffer[0] = '\0';
  recommendations_buffer[0] = '\0';

  return 0;
}
*/

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
      char* sentiment_buffer, const unsigned int sentiment_buffer_len,
      unsigned char* recommendations_buffer, const unsigned int recommendations_buffer_len,
      unsigned int& recommendations_len, unsigned int& recommendations_count,
      const char* profile_name) {

  if (!twitter_handle || twitter_handle_len < 1) {
    std::cerr << "ERROR: invalid input\n";
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
  sentiment_buffer[0] = '\0';
  recommendations_buffer[0] = '\0';
  recommendations_len = 0;
  recommendations_count = 0;

  inagist_classifiers::Corpus corpus;

  unsigned int init_corpus_size = 0;
  bool get_user_info = true;
  if (profile_name && (strlen(profile_name) > 4)) {
    if ((init_corpus_size = inagist_classifiers::CorpusManager::LoadCorpus(profile_name, corpus)) < 0) {
      std::cerr << "ERROR: could not load corpus from file: " << profile_name << std::endl;
      return -1;
    }
    get_user_info = false;
  }

  std::set<std::string> tweets;
  inagist_api::TwitterAPI twitter_api;
  inagist_api::TwitterSearcher twitter_searcher;

  unsigned int count = 0;
  unsigned int user_info_count = 0;
  unsigned int corpus_size = 0;
  unsigned int output_num_docs = 0;

  if (get_user_info) {
    std::set<std::string> user_info_tokens;
    if ((user_info_count = inagist_api::TwitterAPI::GetUserInfo(twitter_handle,
                                                               locations_buffer, locations_buffer_len,
                                                               locations_len, locations_count,
                                                               user_info_tokens)) < 0) {
      std::cerr << "ERROR: could not get user info token for twitter_handle: " << twitter_handle << std::endl;
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
    std::cerr << "WARNING: could not get latest tweets for user: " \
              << twitter_handle << std::endl;
  }

  if (twitter_searcher.Get100TweetsFromUser(twitter_handle, tweets) < 0) {
    std::cerr << "WARNING: could not search for 100 tweets from user: " \
              << twitter_handle << std::endl;
  }

  if (tweets.empty()) {
    // this 'count' is from above user info
    if (0 == user_info_count) {
      return -1;
    } else {
#ifdef PROFILE_DEBUG
    if (PROFILE_DEBUG > 1) {
      std::cout << "INFO: corpus of size " << count \
                << " generated from user info of twitter_handle: " \
                << twitter_handle << std::endl;
    }
#endif
    }
  } else {
    // generate profile
    if ((count += GetGist(tweets,
                    self_languages_buffer, self_languages_buffer_len,
                    self_languages_len, self_languages_count,
                    self_text_classes_buffer, self_text_classes_buffer_len,
                    self_text_classes_len, self_text_classes_count,
                    self_sub_classes_buffer, self_sub_classes_buffer_len,
                    self_sub_classes_len, self_sub_classes_count,
                    self_text_class_contributors_buffer, self_text_class_contributors_buffer_len,
                    self_text_class_contributors_len, self_text_class_contributors_count,
                    sentiment_buffer, sentiment_buffer_len,
                    corpus, corpus_size)) < 0) {
      std::cerr << "ERROR: could not generate profile\n";
      return -1;
    } else {
      corpus_size += count;
    }
  }

  output_num_docs += tweets.size();
  tweets.clear();

  // write corpus to file
  if (profile_name) {
    if (inagist_classifiers::CorpusManager::UpdateCorpusFile(corpus, profile_name) < 0) {
      std::cerr << "ERROR: could not write corpus to file: " << profile_name << std::endl;
      return -1;
    }
  }

  if (!corpus.empty()) {
    corpus.clear();
  }

#ifdef PROFILE_DEBUG
  if (PROFILE_DEBUG > 1) {
    std::cout << "corpus of size " << corpus_size \
              << " generated from " << output_num_docs \
              << " tweets of handle: " << twitter_handle << std::endl;
  }
#endif

  // since this is likely the first time profiling this handle,
  // get archieved data from inagist
  if (get_user_info) {
    // inagist trending tweets
    if (inagist_api::InagistAPI::GetTrendingTweets(twitter_handle, tweets) < 0) {
      std::cerr << "WARNING: could not get inagist trending tweets for user: " \
                << twitter_handle << std::endl;
    }

    // inagist archieves
    if (inagist_api::InagistAPI::GetArchievedTweets(twitter_handle, tweets) < 0) {
      std::cerr << "WARNING: could not get inagist trending tweets for user: " \
                << twitter_handle << std::endl;
    }
  }

  if (tweets.empty()) {
    // this 'count' is from above user info
    if (0 == user_info_count) {
      return -1;
    } else {
#ifdef PROFILE_DEBUG
    if (PROFILE_DEBUG > 1) {
      std::cout << "INFO: corpus of size " << count \
                << " generated from the timeline of twitter_handle: " \
                << twitter_handle << std::endl;
    }
#endif
    }
  } else {
    // generate profile
    if ((count += GetGist(tweets,
                    others_languages_buffer, others_languages_buffer_len,
                    others_languages_len, others_languages_count,
                    others_text_classes_buffer, others_text_classes_buffer_len,
                    others_text_classes_len, others_text_classes_count,
                    others_sub_classes_buffer, others_sub_classes_buffer_len,
                    others_sub_classes_len, others_sub_classes_count,
                    others_text_class_contributors_buffer, others_text_class_contributors_buffer_len,
                    others_text_class_contributors_len, others_text_class_contributors_count,
                    sentiment_buffer, sentiment_buffer_len,
                    corpus, corpus_size)) < 0) {
      std::cerr << "ERROR: could not generate profile\n";
      return -1;
    } else {
      corpus_size += count;
    }
  }

  output_num_docs += tweets.size();
  tweets.clear();

  if (!corpus.empty()) {
    corpus.clear();
  }

#ifdef PROFILE_DEBUG
  if (PROFILE_DEBUG > 1) {
    std::cout << "corpus of size " << corpus_size \
              << "(" << user_info_count << " from user_info) " \
              << " generated from " << output_num_docs \
              << " tweets of handle: " << twitter_handle << std::endl;
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
  unsigned int init_corpus_size = 0;

  if (profile_name && (strlen(profile_name) > 4)) {
    if ((init_corpus_size = inagist_classifiers::CorpusManager::LoadCorpus(profile_name, corpus)) < 0) {
      std::cerr << "ERROR: could not load corpus from file: " << profile_name << std::endl;
      return -1;
    }
  }

  // genrate profile
  unsigned int count = 0;
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
                      char* sentiment_buffer, const unsigned int sentiment_buffer_len,
                      inagist_classifiers::Corpus& corpus, unsigned int& corpus_size) {

  // TODO (balaji) - make these member variables

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
  sentiment_buffer[0] = '\0';

  unsigned char buffer[MAX_BUFFER_LEN];
  //unsigned int buffer_len = MAX_BUFFER_LEN;
  buffer[0] = '\0';

  char safe_status_buffer[10];
  safe_status_buffer[0] = '\0';
  unsigned int safe_status_buffer_len = 10;

  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  keywords_buffer[0] = '\0';
  unsigned int keywords_buffer_len = MAX_BUFFER_LEN;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;

  unsigned char hashtags_buffer[MAX_BUFFER_LEN];
  hashtags_buffer[0] = '\0';
  unsigned int hashtags_buffer_len = MAX_BUFFER_LEN;
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;

  unsigned char keyphrases_buffer[MAX_BUFFER_LEN];
  keyphrases_buffer[0] = '\0';
  unsigned int keyphrases_buffer_len = MAX_BUFFER_LEN;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;

  unsigned char dummy_buffer[MAX_BUFFER_LEN];
  dummy_buffer[0] = '\0';
  unsigned int dummy_buffer_len = MAX_BUFFER_LEN;
  unsigned int dummy_len = 0;
  unsigned int dummy_count = 0;

  char scripts_buffer[MAX_LIST_LEN];
  memset(scripts_buffer, 0, MAX_LIST_LEN);
  unsigned int scripts_buffer_len = MAX_LIST_LEN;

  std::set<std::string> self_languages_set;
  std::map<std::string, int> self_sub_classes_map;
  std::set<std::string>::iterator set_iter;
  std::string tweet;
  std::string language;
  unsigned int count = 0;

  int ret_value = 0;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    tweet.clear();
    tweet.assign(*set_iter);
    if (tweet.length() < 1)
      continue;
    strcpy((char*) buffer, tweet.c_str());
    if ((ret_value = m_gist_maker.GetGist((const unsigned char*) buffer, strlen((char*) buffer),
                  (char*) safe_status_buffer, safe_status_buffer_len,
                  (char*) scripts_buffer, scripts_buffer_len,
                  (char*) self_languages_buffer, self_languages_buffer_len,
                  (unsigned char*) keywords_buffer, keywords_buffer_len,
                  &keywords_len, &keywords_count,
                  (unsigned char*) hashtags_buffer, hashtags_buffer_len,
                  &hashtags_len, &hashtags_count,
                  (unsigned char*) keyphrases_buffer, keyphrases_buffer_len,
                  &keyphrases_len, &keyphrases_count,
                  (char*) self_sub_classes_buffer, self_sub_classes_buffer_len,
                  &self_sub_classes_len, &self_sub_classes_count,
                  (unsigned char*) dummy_buffer, dummy_buffer_len,
                  &dummy_len, &dummy_count,
                  (char*) sentiment_buffer, sentiment_buffer_len)) < 0) {
      std::cerr << "ERROR: could not get keywords\n";
    } else {

      if ((count = inagist_utils::PipeListToMap((unsigned char*) self_sub_classes_buffer, self_sub_classes_map)) < 0) {
        std::cerr << "ERROR: corpus with keywords\n";
      }

      if ((count = inagist_utils::PipeListToMap(keywords_buffer, corpus)) < 0) {
        std::cerr << "ERROR: could not populate corpus with keywords\n";
      } else {
#ifdef PROFILE_DEBUG
        std::cout << "INFO: corpus of size " << count << " generated from keywords\n";
        if (count > 0) {
          std::cout << keywords_buffer << std::endl;
        }
#endif
        corpus_size += count;
      }
      if ((count = inagist_utils::PipeListToMap(hashtags_buffer, corpus)) < 0) {
        std::cerr << "ERROR: could not populate corpus with hashtags\n";
      } else {
#ifdef PROFILE_DEBUG
        std::cout << "INFO: corpus of size " << count << " generated from hashtags\n";
        if (count > 0) {
          std::cout << hashtags_buffer << std::endl;
        }
#endif
        corpus_size += count;
      }
      // this is to purge duplicates
      if (self_languages_buffer && strlen(self_languages_buffer) > 0) {
        language.assign(self_languages_buffer);
        self_languages_set.insert(language);
      }
      if (strlen(scripts_buffer) > 0) {
        language.assign(scripts_buffer);
        self_languages_set.insert(language);
      }
    }
    safe_status_buffer[0] = '\0';
    scripts_buffer[0] = '\0';
    self_languages_buffer[0] = '\0';
    keywords_buffer[0] = '\0';
    hashtags_buffer[0] = '\0';
    keyphrases_buffer[0] = '\0';
    dummy_buffer[0] = '\0';
  }

  // TODO (balaji) - another call to find self_text_classes for the whole corpus, relook
#ifdef PROFILE_DEBUG
  std::cout << "corpus_size: " << corpus.size() << std::endl;
#endif

  if (m_gist_maker.FindTextClasses(corpus,
                                   self_text_classes_buffer, self_text_classes_buffer_len,
                                   self_text_classes_len, self_text_classes_count,
                                   self_text_class_contributors_buffer, self_text_class_contributors_buffer_len,
                                   self_text_class_contributors_len, self_text_class_contributors_count
                                  ) < 0) {
    std::cerr << "ERROR: couldn't find text classes\n";
  }
  // not clearing corpus, becos its a return parameter
#ifdef PROFILE_DEBUG
  std::cout << "self_text_classes_count: " << self_text_classes_count << std::endl;
#endif

  // TODO (balaji) - currently populating self_sub_classes. need to have a relook at this.
  if (inagist_utils::MapToPipeList(self_sub_classes_map,
                                   (unsigned char*) self_sub_classes_buffer, self_sub_classes_buffer_len,
                                   self_sub_classes_len, self_sub_classes_count) < 0) {
    std::cerr << "ERROR: could not populate set elements to pipe separated list\n";
  }
  self_sub_classes_map.clear();
#ifdef PROFILE_DEBUG
  std::cout << "self_sub_classes_count: " << self_sub_classes_count << std::endl;
#endif

  if (inagist_utils::SetToPipeList(self_languages_set,
                                   (unsigned char*) self_languages_buffer, self_languages_buffer_len,
                                   self_languages_len, self_languages_count) < 0) {
    std::cerr << "ERROR: could not populate set elements to pipe separated list\n";
  }
  self_languages_set.clear();
#ifdef PROFILE_DEBUG
  std::cout << "self_languages_count: " << self_languages_count << std::endl;
#endif

  return corpus_size;
}

} // namespace inagist_dashboard
