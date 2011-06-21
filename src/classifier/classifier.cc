#include "classifier.h"
#include <iostream>
#include <cstdlib>
#include <cstring>
#include "twitter_searcher.h"
#include "twitter_api.h"
#include "inagist_api.h"

#ifdef DEBUG
#if DEBUG>0
#define CLASSIFIER_DEBUG DEBUG
#endif
#endif
//#define CLASSIFIER_DEBUG 3

#define MIN_TWEETS_REQUIRED 15

namespace inagist_classifiers {

Classifier::Classifier() {
}

Classifier::~Classifier() {

  if (ClassifierConfig::Clear(m_config) < 0) {
    std::cerr << "ERROR: could not clear config\n";
  }

  try {
    m_corpus_manager.Clear();
  } catch (...) {
    std::cerr << "ERROR: Corpus Manager throws exception" << std::endl;
  }

}

int Classifier::Init(std::string config_file_name, bool ignore_history) {

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified

  if (ClassifierConfig::Read(config_file_name.c_str(), m_config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
    return -1;
  }

  if (m_config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " \
              << config_file_name << std::endl;
    return -1;
  }

  std::map<std::string, std::string> class_name_file_map;
  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {
    class_name_file_map[m_config.iter->name] = m_config.iter->training_data_file;
  }
  if (class_name_file_map.empty()) {
    std::cerr << "ERROR: class_name_file_map cannot be empty\n";
    return -1;
  }

  if (!ignore_history) {
    if (m_corpus_manager.LoadCorpus(m_config.test_data_file,
                                    m_corpus_manager.m_classes_freq_map) < 0) {
      std::cerr << "ERROR: could not load the text classes freq file (test data)\n";
      std::cout << "WARNING: continuing without the text classes freq data\n";
    }
  } else {
#ifdef CLASSIFIER_DEBUG
    if (CLASSIFIER_DEBUG > 1) {
      std::cout << "INFO: ignoring historical data. plain vanilla classification\n";
    }
#endif
  }

  if (m_corpus_manager.LoadCorpusMap(class_name_file_map) < 0) {
    std::cerr << "ERROR: could not load Corpus Map\n";
    return -1;
  }

  if (ClassifierConfig::LoadClassLabelsMap(m_config, m_class_labels_map) < 0) {
    std::cerr << "ERROR: could not load class labels map\n";
    return -1;
  }

  return 0;
}

#ifdef DATA_TRAINING_ENABLED
// this functioin uses round robin to get training data.
// TODO (balaji) - handle rate limiting and get data from all sources
// persist with round robin till an equal number of training text is obtained
int Classifier::GetTrainingData(const char* config_file_name) {

  if (!config_file_name) {
    std::cerr << "ERROR: invalid config file name\n";
    return -1;
  }

  Config config;
  if (ClassifierConfig::Read(config_file_name, config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
    return -1;
  }

  if (config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " << config_file_name << std::endl;
    return -1;
  }

  // the following code, gets a random starting point.
  // though all the sources are pinged, this randomization is to save from biases
  int i = rand() % config.classes.size();
  int j = 0;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    if (j == i)
      break;
    j++;
  }

  int count = 0;
  int count_temp = 0;
  unsigned int num_docs = 0;
  unsigned int corpus_size = 0;
  for (; config.iter != config.classes.end(); config.iter++) {
    num_docs = 0;
    corpus_size = 0;
    if ((count_temp = GetTrainingData(config.iter->name,
                                      config.iter->handles_file,
                                      config.iter->tweets_file,
                                      num_docs,
                                      config.iter->corpus_file,
                                      corpus_size)) < 0) {
      std::cerr << "ERROR: could not get training data for handles in file: " \
                << config.iter->handles_file << std::endl; 
    } else {
      std::cout << "Corpus of size " << count_temp \
                << " generated for " << config.iter->name << " from " \
                << num_docs << " docs" << std::endl;
      count += count_temp;
    }
  }
  j = 0;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end() && j<i; config.iter++) {
    j++;
    num_docs = 0;
    corpus_size = 0;
    if ((count_temp = GetTrainingData(config.iter->name,
                                      config.iter->handles_file,
                                      config.iter->tweets_file,
                                      num_docs,
                                      config.iter->corpus_file,
                                      corpus_size)) < 0) {
      std::cerr << "ERROR: could not get training data for handles in file: " \
                << config.iter->handles_file << std::endl; 
    } else {
      std::cout << "Corpus of size " << count_temp \
                << " generated for " << config.iter->name << " from " \
                << num_docs << " docs" << std::endl;
      count += count_temp;
    }
  }

  ClassifierConfig::Clear(config);

  return count;
}

int Classifier::GetTrainingData(const std::string& class_name,
                                const std::string& twitter_handles_file_name,
                                const std::string& output_tweets_file_name,
                                unsigned int& output_num_docs,
                                const std::string& output_corpus_file_name,
                                unsigned int& output_corpus_size) {

  std::ifstream ifs(twitter_handles_file_name.c_str());
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open twitter handles file: " << twitter_handles_file_name << std::endl;
    return -1;
  }

#ifdef CLASSIFIER_DEBUG
  std::cout << "INFO: training data from handles file: " << twitter_handles_file_name << std::endl;
#endif

  std::string line;
  std::string handle;
  std::string::size_type loc;
  unsigned int flag = 0;
  unsigned int exe_count = 0;
  std::string exe_count_str;
  std::map<std::string, unsigned int> handles;
  std::map<std::string, unsigned int>::iterator handle_iter;

  while(getline(ifs, line)) {
    if (line.length() <= 1) {
      continue;
    }
    loc = line.find("=", 0);
    if (loc == std::string::npos) {
      handle.assign(line);
      exe_count = 0;
    } else {
      handle.assign(line, 0, loc);
      exe_count_str.assign(line, loc+1, line.length()-loc-1);
      exe_count = atoi((char *) exe_count_str.c_str());
    }
    handles[handle] = exe_count;
  }
  ifs.close();

  if (handles.size() < 1) {
    std::cerr << "ERROR: no handles found in file " << twitter_handles_file_name << std::endl;
    return 0;
  }

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  unsigned int count = 0;
  unsigned int count_temp = 0;
  Corpus corpus;

  // the following code is to randomly pick one handle and get tweets from it
  int i = rand() % handles.size();
  int j = 0;
  for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
    if (i==j)
      break;
    j++;
  }

  // this flag indicates a flip in state and hence needs to be committed to file
  flag = 0;
  bool no_fresh_handle = true;
  bool get_user_info = false;
  unsigned int local_num_docs = 0;
  unsigned int local_corpus_size = 0;
  for (; handle_iter != handles.end(); handle_iter++) {
    if (handle_iter->second == 1)
      continue;
    no_fresh_handle = false;
    handle = handle_iter->first;

    // need user info. 0 indicates its the first time this handles is being processed
    if (handle_iter->second == 0) {
      count_temp = GetTrainingData(handle, local_num_docs, corpus, local_corpus_size, get_user_info=true);
    } else {
      count_temp = GetTrainingData(handle, local_num_docs, corpus, local_corpus_size, get_user_info=false);
    }
    usleep(100000);

    if (count_temp < 0) {
      std::cerr << "ERROR: could not get training data for handle: " << handle \
                << " for class: " << class_name << std::endl;
    } else {
      handles[handle] += 1;
      handles[handle] %= 2;
      flag = 1;
    }
    if (0 == count_temp) {
      continue;
    } else {
      count += count_temp;
    }

    if (local_num_docs < MIN_TWEETS_REQUIRED) {
      continue;
    } else {
      output_num_docs += local_num_docs;
      output_corpus_size += local_corpus_size;
      break;
    }
  }

  // if no fresh handle has been found from the random location to end of the hash map,
  // continue from the first till that random location
  if (0 == flag) {
    j = 0;
    local_num_docs = 0;
    local_corpus_size = 0;
    for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {

      if (i==j) // i is the random location
        break;
      j++;

      if (handle_iter->second == 1)
        continue;
      no_fresh_handle = false;
      handle = handle_iter->first;

      // need user info
      if (handle_iter->second == 0) {
        count_temp = GetTrainingData(handle, local_num_docs, corpus, local_corpus_size, get_user_info=true);
      } else {
        count_temp = GetTrainingData(handle, local_num_docs, corpus, local_corpus_size, get_user_info=false);
      }
      usleep(100000);

      if (count_temp < 0) {
        std::cerr << "ERROR: could not get training data for handle: " << handle << std::endl;
      } else {
        handles[handle] += 1;
        handles[handle] %= 2;
        flag = 1;
      }

      if (0 == count_temp) {
        continue;
      } else {
        count += count_temp;
      }

      if (local_num_docs < MIN_TWEETS_REQUIRED) {
        continue;
      } else {
        output_num_docs += local_num_docs;
        output_corpus_size += local_corpus_size;
        break;
      }
    }
  }

  if (no_fresh_handle) {
    // all the entries in the handles file may have been examined and hence have "1".
    // lets flip them all to 2.
#ifdef CLASSIFIER_DEBUG
    if (CLASSIFIER_DEBUG > 1) {
      std::cout << "INFO: flipping all the handle entries to zero" << std::endl;
    }
#endif
    for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
      if (handle_iter->second == 0) {
        std::cerr << "ERROR: a fresh handle found while flipping. logical error\n";
        handles.clear();
        return -1;
      } else if (handle_iter->second == 1) {
        handle = handle_iter->first;
        handles[handle] = 2;
        flag = 1;
      } else {
        std::cerr << "ERROR: ill-maintained traning data state or unexplained error\n";
        handles.clear();
        return -1;
      }
    }
  }

  if (1 == flag) {
    std::ofstream ofs(twitter_handles_file_name.c_str());
    if (!ofs.is_open()) {
      std::cerr << "ERROR: handles file: " << twitter_handles_file_name << " could not be opened for write.\n";
      return -1;
    } else {
#ifdef CLASSIFIER_DEBUG
    if (CLASSIFIER_DEBUG > 3) {
      std::cout << "INFO: updating the handles file after current round\n";
    }
#endif
      for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
         ofs << handle_iter->first << "=" << handle_iter->second << std::endl;
      }
      ofs.close();
    }
  }

  handles.clear();

  if (no_fresh_handle) {
#ifdef CLASSIFIER_DEBUG
    if (CLASSIFIER_DEBUG > 1) {
      std::cout << "INFO: no fresh handle found. hence all handles were flipped." \
                << " now making recursive call\n";
    }
#endif
    // recursive call
    return GetTrainingData(class_name,
                           twitter_handles_file_name,
                           output_tweets_file_name,
                           output_num_docs,
                           output_corpus_file_name,
                           output_corpus_size);
  }

  if (count == 0) {
    std::cout << "WARNING: No tweets found for handles in file " << twitter_handles_file_name << std::endl;
    return 0;
  } else {
    if (CorpusManager::UpdateCorpusFile(corpus, output_corpus_file_name) < 0) {
      std::cerr << "ERROR: could not update features to output file " << output_corpus_file_name << std::endl;
    }
  }

  corpus.clear();

  return count;
}

int Classifier::GetTrainingData(const std::string& handle,
                                unsigned int& output_num_docs,
                                Corpus& corpus,
                                unsigned int& output_corpus_size,
                                bool get_user_info) {

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  unsigned int count = 0;
  unsigned int user_info_count = 0;
  unsigned int count_temp = 0;

  if (get_user_info) {
    /*
    std::string user_info;
    if (inagist_api::TwitterAPI::GetUserInfo(handle, user_info) < 0) {
      std::cerr << "ERROR: could not get user info for handle: " << handle << std::endl;
    } else {
      if (user_info.length() > 0 && (user_info_count = GetCorpus(user_info, corpus)) < 0) {
        std::cerr << "ERROR: could not find ngrams for user info string: " << user_info << std::endl;
      } else {
        count += user_info_count;
      }
    }
    */
    std::set<std::string> user_info_tokens;
    if (inagist_api::TwitterAPI::GetUserInfo(handle, user_info_tokens) < 0) {
      std::cerr << "ERROR: could not get user info token for handle: " << handle << std::endl;
    } else {
      std::set<std::string>::iterator tokens_iter;
      for (tokens_iter = user_info_tokens.begin(); tokens_iter != user_info_tokens.end(); tokens_iter++) {
        if (corpus.find(*tokens_iter) != corpus.end()) {
          corpus[*tokens_iter] += 1;
        } else {
          corpus[*tokens_iter] = 1;
        }
      }
    }
  }
  output_corpus_size += user_info_count;

  if (twitter_searcher.GetTweetsFromUser(handle, tweets) < 0) {
    std::cout << "ERROR: could not get tweets from user: " << handle << std::endl;
    // this 'count' is from above user info
    if (0 == user_info_count) {
      return -1;
    } else {
#ifdef CLASSIFIER_DEBUG
    if (CLASSIFIER_DEBUG > 1) {
      std::cout << "INFO: corpus of size " << count \
                << " generated from user info of handle: " << handle << std::endl;
    }
#endif
      return user_info_count;
    }
  }

  if (get_user_info) {
    if (inagist_api::InagistAPI::GetTrendingTweets(handle, tweets) < 0) {
      std::cerr << "WARNING: could not get inagist trending tweets for user: " \
                << handle << std::endl;
    }

    if (inagist_api::InagistAPI::GetArchievedTweets(handle, tweets) < 0) {
      std::cerr << "WARNING: could not get inagist trending tweets for user: " \
                << handle << std::endl;
    }
  }

  if (!tweets.empty()) {
    std::set<std::string>::iterator set_iter;
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      // this GetCorpus is a pure virtual function
      // ensure your derivation of this classifier provides this function
      if ((count_temp = GetCorpus(*set_iter, corpus)) < 0) {
        std::cerr << "ERROR: could not find ngrams from tweet: " << *set_iter << std::endl;
      } else {
        count += count_temp;
      }
    }
    output_num_docs += tweets.size();
#ifdef CLASSIFIER_DEBUG
    if (CLASSIFIER_DEBUG > 1) {
      std::cout << "corpus of size " << count \
                << " generated from " << tweets.size() \
                << " tweets of handle: " << handle << std::endl;
    }
#endif
    tweets.clear();
  }

  output_corpus_size += count;

  return count + user_info_count;
}
#endif // DATA_TRAINING_ENABLED

} // namespace inagist_classifiers
