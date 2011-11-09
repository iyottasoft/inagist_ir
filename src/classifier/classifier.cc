#include "classifier.h"
#include <iostream>
#include <fstream>
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include "twitter_searcher.h"
#include "twitter_api.h"
#include "inagist_api.h"
#include "test_utils.h"
#include "script_detector_utils.h"
#include "utf8.h"

extern int DetectScript(int code_point, std::string &script);

#ifdef DEBUG
#if DEBUG>0
#define CLASSIFIER_DEBUG DEBUG
#endif
#endif
//#define CLASSIFIER_DEBUG 3

namespace inagist_classifiers {

Classifier::Classifier() {
#ifdef CLASSIFIER_DEBUG
  m_debug_level = CLASSIFIER_DEBUG;
#else
  m_debug_level = 0;
#endif // CLASSIFIER_DEBUG
}

Classifier::~Classifier() {

  if (ClassifierConfig::Clear(m_config) < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not clear config\n";
#endif // CLASSIFIER_DEBUG
  }

  m_class_labels_map.clear();
  m_class_numbers_map.clear();
  CorpusManager::ClearCorpus(m_corpus);
  CorpusManager::ClearCorpus(m_classes_freq_map);
  CorpusManager::ClearCorpusMap(m_corpus_map);

}

int Classifier::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  return 0;
}

int Classifier::Init(std::string config_file_name, bool ignore_history, unsigned int corpus_type) {

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified

  if (ClassifierConfig::Read(config_file_name.c_str(), m_config) < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  if (m_config.classes.empty()) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: class structs could not be read from config file: " \
              << config_file_name << std::endl;
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  CorpusMapMeta corpus_map_meta_data;
#ifdef CLASSIFIER_DEBUG
  if (m_debug_level > 4) {
    std::cout << "classifier training meta data\n";
  }
#endif // CLASSIFIER_DEBUG

  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {
    switch (corpus_type) {
      case 0:
        corpus_map_meta_data[m_config.iter->name] = m_config.iter->class_data_file;
        break;
      case 1:
        corpus_map_meta_data[m_config.iter->name] = m_config.iter->training_data_file;
        break;
      case 2:
        corpus_map_meta_data[m_config.iter->name] = m_config.iter->training_data_file;
        break;
      default:
        break;
    }
#ifdef CLASSIFIER_DEBUG
    if (m_debug_level > 4) {
      std::cout << m_config.iter->name << " = " << corpus_map_meta_data[m_config.iter->name] << std::endl;
    }
#endif // CLASSIFIER_DEBUG
  }

  if (corpus_map_meta_data.empty()) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: corpus_map_meta_data cannot be empty\n";
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  if (!ignore_history) {
    if (CorpusManager::LoadCorpus(m_config.class_freqs_file,
                                  m_classes_freq_map) < 0) {
#ifdef CLASSIFIER_DEBUG
      std::cerr << "ERROR: could not load the text classes freq file (test data)\n";
      std::cout << "WARNING: continuing without the text classes freq data\n";
#endif // CLASSIFIER_DEBUG
    }
  } else {
#ifdef CLASSIFIER_DEBUG
    if (m_debug_level > 1) {
      std::cout << "INFO: ignoring historical data. plain vanilla classification\n";
    }
#endif // CLASSIFIER_DEBUG
  }

#ifdef CLASSIFIER_DEBUG
  if (m_debug_level > 1) {
    std::cout << "loading corpus_map from corpus_map_meta_data (while initializing classifier)\n";
  }
#endif // CLASSIFIER_DEBUG
  if (CorpusManager::LoadCorpusMap(corpus_map_meta_data, m_corpus_map) < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not load Corpus Map\n";
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  if (ClassifierConfig::LoadClassLabelsMap(m_config, m_class_labels_map) < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not load class labels map\n";
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  if (ClassifierConfig::LoadClassNumbersMap(m_config, m_class_numbers_map) < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not load class numbers map\n";
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  return 0;
}

int Classifier::Classify(Corpus& test_corpus,
                         std::string& output_class,
                         std::string& top_classes, unsigned int& top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                         , std::map<std::string, std::string>& class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                        ) {

  if (m_naive_bayes_classifier.GuessClass3(m_corpus_map,
                                           m_classes_freq_map,
                                           test_corpus,
                                           output_class,
                                           top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                           , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                           , m_debug_level) < 0) {
    top_classes_count = 0;
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: naive bayes classifiers could not guess the language\n";
    output_class.assign("UU");
    top_classes.assign("UU");
    top_classes_count = 1;
#endif // CLASSIFIER_DEBUG
    return -1;
  }

#ifdef CLASSIFIER_DEBUG
  if (m_debug_level > 1) {
    std::cout << "output_class: " << output_class << std::endl;
    std::cout << "top_classes: " << top_classes << std::endl;
  }
#endif // CLASSIFIER_DEBUG

  return 1;
}

// this is a dummy function. this has to be overridden by the derived class
int Classifier::GetCorpus(const std::string& text, Corpus& corpus) {
  std::cout << "Fatal Error: this dummy GetCorpus class should not have been called\n";
  return -1;
}

#ifdef CLASSIFIER_DATA_TRAINING_ENABLED

// this functioin uses round robin to get data.
// TODO (balaji) - handle rate limiting and get data from all sources
// persist with round robin till an equal number of text is obtained
int Classifier::GetData(const bool& train_not_test, const char* config_file_name) {

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
  std::string data_file;
  std::string corpus_file;
  std::string handles_file;
  std::string tweets_file;
  for (; config.iter != config.classes.end(); config.iter++) {
    num_docs = 0;
    corpus_size = 0;
    if (train_not_test) {
      corpus_file = config.iter->training_corpus_file;
      data_file = config.iter->training_data_file;
      tweets_file = config.iter->training_tweets_file;
      handles_file = config.iter->training_handles_file;
    } else {
      corpus_file = config.iter->testing_corpus_file;
      data_file = config.iter->testing_data_file;
      tweets_file = config.iter->testing_tweets_file;
      handles_file = config.iter->testing_handles_file;
    }
    if ((count_temp = GetData(train_not_test,
                              config.iter->name,
                              handles_file,
                              tweets_file,
                              num_docs,
                              corpus_file,
                              corpus_size)) < 0) {
#ifdef CLASSIFIER_DEBUG
      std::cerr << "ERROR: could not get training data for handles in file: " \
                << handles_file << std::endl; 
#endif // CLASSIFIER_DEBUG
    } else {
#ifdef CLASSIFIER_DEBUG
      if (m_debug_level > 2) {
        std::cout << "INFO: Corpus of size " << count_temp \
                  << " generated for " << config.iter->name << " from " \
                  << num_docs << " docs" << std::endl;
      }
#endif // CLASSIFIER_DEBUG
      count += count_temp;
    }
  }
  j = 0;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end() && j<i; config.iter++) {
    j++;
    num_docs = 0;
    corpus_size = 0;

    if (train_not_test) {
      corpus_file = config.iter->training_corpus_file;
      data_file = config.iter->training_data_file;
      tweets_file = config.iter->training_tweets_file;
      handles_file = config.iter->training_handles_file;
    } else {
      corpus_file = config.iter->testing_corpus_file;
      data_file = config.iter->testing_data_file;
      tweets_file = config.iter->testing_tweets_file;
      handles_file = config.iter->testing_handles_file;
    }
 
    if ((count_temp = GetData(train_not_test,
                              config.iter->name,
                              handles_file,
                              tweets_file,
                              num_docs,
                              corpus_file,
                              corpus_size)) < 0) {
#ifdef CLASSIFIER_DEBUG
      std::cerr << "ERROR: could not get training data for handles in file: " \
                << handles_file << std::endl; 
#endif // CLASSIFIER_DEBUG
    } else {
#ifdef CLASSIFIER_DEBUG
      if (m_debug_level > 1) {
        std::cout << "INFO: Corpus of size " << count_temp \
                  << " generated for " << config.iter->name << " from " \
                  << num_docs << " docs" << std::endl;
      }
#endif // CLASSIFIER_DEBUG
      count += count_temp;
    }
  }

  ClassifierConfig::Clear(config);

  return count;
}

int Classifier::GetData(const bool& train_not_test,
                        const std::string& class_name,
                        const std::string& twitter_handles_file_name,
                        const std::string& output_tweets_file_name,
                        unsigned int& output_num_docs,
                        const std::string& output_corpus_file_name,
                        unsigned int& output_corpus_size) {

  std::ifstream ifs(twitter_handles_file_name.c_str());
  if (!ifs.is_open()) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not open twitter handles file: " << twitter_handles_file_name << std::endl;
#endif // CLASSIFIER_DEBUG
    return -1;
  }

#ifdef CLASSIFIER_DEBUG
  if (m_debug_level > 3) {
    std::cout << "INFO: training data from handles file: " << twitter_handles_file_name << std::endl;
  }
#endif // CLASSIFIER_DEBUG

  std::ofstream ofs(output_tweets_file_name.c_str());
  std::ostream* output_stream = &ofs;

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
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: no handles found in file " << twitter_handles_file_name << std::endl;
#endif // CLASSIFIER_DEBUG
    ofs.close();
    return 0;
  }

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  unsigned int count = 0;
  int count_temp = 0;
  Corpus corpus;
  Corpus class_freq_map;

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
  unsigned int num_docs_found_for_handle = 0;
  unsigned int corpus_size_from_handle = 0;
  for (; handle_iter != handles.end(); handle_iter++) {
    if (handle_iter->second == 1)
      continue;
    no_fresh_handle = false;
    handle = handle_iter->first;

    // need user info. 0 indicates its the first time this handles is being processed
    if (handle_iter->second == 0) {
      count_temp = GetData(train_not_test,
                           handle,
                           class_name,
                           corpus,
                           corpus_size_from_handle,
                           num_docs_found_for_handle,
                           class_freq_map,
                           *output_stream,
                           get_user_info=true);
    } else {
      count_temp = GetData(train_not_test,
                           handle,
                           class_name,
                           corpus,
                           corpus_size_from_handle,
                           num_docs_found_for_handle,
                           class_freq_map,
                           *output_stream,
                           get_user_info=false);
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

    if (num_docs_found_for_handle < MIN_TWEETS_REQUIRED) {
      continue;
    } else {
      output_num_docs += num_docs_found_for_handle;
      output_corpus_size += corpus_size_from_handle;
      break;
    }
  }

  // if no fresh handle has been found from the random location to end of the hash map,
  // continue from the first till that random location
  if (0 == flag) {
    j = 0;
    num_docs_found_for_handle = 0;
    corpus_size_from_handle = 0;
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
        count_temp = GetData(train_not_test,
                             handle,
                             class_name,
                             corpus,
                             corpus_size_from_handle,
                             num_docs_found_for_handle,
                             class_freq_map,
                             *output_stream,
                             get_user_info=true);
      } else {
        count_temp = GetData(train_not_test,
                             handle,
                             class_name,
                             corpus,
                             corpus_size_from_handle,
                             num_docs_found_for_handle,
                             class_freq_map,
                             *output_stream,
                             get_user_info=false);
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

      if (num_docs_found_for_handle < MIN_TWEETS_REQUIRED) {
        continue;
      } else {
        output_num_docs += num_docs_found_for_handle;
        output_corpus_size += corpus_size_from_handle;
        break;
      }
    }
  }

  ofs.close();

  if (no_fresh_handle) {
    // all the entries in the handles file may have been examined and hence have "1".
    // lets flip them all to 2.
#ifdef CLASSIFIER_DEBUG
    if (m_debug_level > 2) {
      std::cout << "INFO: flipping all the handle entries to zero" << std::endl;
    }
#endif // CLASSIFIER_DEBUG
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
#endif // CLASSIFIER_DEBUG
      for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
         ofs << handle_iter->first << "=" << handle_iter->second << std::endl;
      }
      ofs.close();
    }
  }

  handles.clear();

  if (no_fresh_handle) {
#ifdef CLASSIFIER_DEBUG
    if (m_debug_level > 2) {
      std::cout << "INFO: no fresh handle found. hence all handles were flipped." \
                << " now making recursive call\n";
    }
#endif // CLASSIFIER_DEBUG
    // recursive call
    return GetData(train_not_test,
                   class_name,
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
#ifdef CLASSIFIER_DEBUG
    if (m_debug_level > 1) {
      std::cout << "updating class: " << class_name << " to file: " << output_corpus_file_name << std::endl;
    }
#endif // CLASSIFIER_DEBUG
    if (CorpusManager::UpdateCorpusFile(corpus, output_corpus_file_name) < 0) {
      std::cerr << "ERROR: could not update features to output file " << output_corpus_file_name << std::endl;
    }
  }

  corpus.clear();
  class_freq_map.clear();

  return count;
}

int Classifier::GetData(const bool& train_not_test,
                        const std::string& handle,
                        const std::string& expected_class_name,
                        Corpus& output_corpus, // this corpus is from tweets of the given handle
                        unsigned int& output_corpus_size, // this shd be output_corpus.size(). but its complicated!
                        unsigned int& output_num_docs, // how many docs obtained for this handle
                        Corpus& output_class_freq_map, // how are those docs distributed across classes
                        std::ostream &output_stream,
                        bool get_user_info) {

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  unsigned int count = 0;
  unsigned int user_info_count = 0;
  int count_temp = 0;

  if (get_user_info) {
    /*
    std::string user_info;
    if (inagist_api::TwitterAPI::GetUserInfo(handle, user_info) < 0) {
      std::cerr << "ERROR: could not get user info for handle: " << handle << std::endl;
    } else {
      if (user_info.length() > 0 && (user_info_count = GetCorpus(user_info, output_corpus)) < 0) {
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
        if (output_corpus.find(*tokens_iter) != output_corpus.end()) {
          output_corpus[*tokens_iter] += 1;
        } else {
          output_corpus[*tokens_iter] = 1;
        }
      }
    }
  }
  output_corpus_size += user_info_count;

  if (twitter_searcher.GetTweetsFromUser(handle, tweets) < 0) {
    std::cerr << "ERROR: could not get tweets from user: " << handle << std::endl;
    // this 'count' is from above user info
    if (0 == user_info_count) {
      return -1;
    } else {
#ifdef CLASSIFIER_DEBUG
    if (CLASSIFIER_DEBUG > 1) {
      std::cout << "INFO: corpus of size " << count \
                << " generated from user info of handle: " << handle << std::endl;
    }
#endif // CLASSIFIER_DEBUG
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

  if (tweets.empty()) {
    return user_info_count;
  }

  std::set<std::string>::iterator set_iter;
  if (train_not_test) {
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      // this GetCorpus is a virtual function
      // ensure your derivation of this classifier provides this function
      if ((count_temp = GetCorpus(*set_iter, output_corpus)) < 0) {
        std::cerr << "ERROR: could not get corpus for tweet: " << *set_iter << std::endl;
      } else {
        count += count_temp;
        output_stream << *set_iter << std::endl;
      }
    }
    output_num_docs += tweets.size();
#ifdef CLASSIFIER_DEBUG
    if (CLASSIFIER_DEBUG > 1) {
      std::cout << "corpus of size " << count \
                << " generated from " << tweets.size() \
                << " tweets of handle: " << handle << std::endl;
    }
#endif // CLASSIFIER_DEBUG
  } else {

    int ret_val = 0;
    std::string output_class;
    std::string output_top_classes;
    unsigned int output_top_classes_count = 0;
#ifdef CLASS_CONTRIBUTORS_ENABLED
    std::map<std::string, std::string> class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED

    // like in training, we first call Getcorpus.
    // then we call Classify on that corpus.
    // if expected class is given and our finding doesn't match, we throw away the corpus
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {

      // output_corpus is really test_corpus. excuse the poor nomenclature
      if ((count_temp = GetCorpus(*set_iter, output_corpus)) < 0) {
        std::cerr << "ERROR: could not get corpus for tweet: " << *set_iter << std::endl;
        continue;
      }

      if ((ret_val = Classify(output_corpus,
                              output_class,
                              output_top_classes,
                              output_top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                              , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                             )) < 0) {
        std::cerr << "ERROR: could not find class\n";
      } else if (ret_val == 0) {
        // undefined result. nothing to be done
      } else {
        bool valid_result_flag = true;
        // if expected class_name is not given valid_result_flag will remain true
        if (!expected_class_name.empty()) {
          if (expected_class_name.compare(output_class) == 0) {
            // correct result
            valid_result_flag = true;
          } else if ((output_class.compare(0,2,"UU") == 0) ||
                     (output_class.compare(0,2,"XX") == 0) ||
                     (output_class.compare(0,2,"RR") == 0)) {
            // undefined result
            valid_result_flag = false;
          } else {
            // wrtong result
            valid_result_flag = false;
          }
        }
        if (valid_result_flag) {
          count++;
          if (output_class_freq_map.find(output_class) != output_class_freq_map.end()) {
            output_class_freq_map[output_class] += 1;
          } else {
            output_class_freq_map[output_class] = 1;
          }
          output_corpus.clear();
        }
      }
    }

#ifdef CLASS_CONTRIBUTORS_ENABLED
    class_contributors_map.clear();
#endif // CLASS_CONTRIBUTORS_ENABLED
  }

  tweets.clear();

  output_corpus_size += count;

  return count + user_info_count;
}

int Classifier::CleanCorpusFile(std::string& corpus_file_name,
                                std::string& output_suffix,
                                unsigned int& clean_type) {

  std::ifstream ifs(corpus_file_name.c_str());
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open file " << corpus_file_name << std::endl;
    return -1;
  }

  Corpus corpus;
  Corpus new_corpus;
  Corpus invalid_corpus;
  Corpus temp_corpus;
  CorpusIter corpus_iter;
  CorpusIter temp_corpus_iter;
  CorpusIter trojan_corpus_iter;
  std::string key;
  std::string temp_key;
  std::string new_key;
  double value = 0;
  double temp_value = 0;
  bool flag = false;
  unsigned char str[1024];
  memset((char *) str, '\0', 1024);
  double temp_double = 0;

  if (CorpusManager::LoadCorpus(corpus_file_name, corpus) < 0) {
    std::cerr << "ERROR: could not load corpus file: " << corpus_file_name << std::endl;
    return 0;
  }

#ifdef CLASSIFIER_DEBUG
  bool wait=true;
  bool temp_wait=true;
  std::string input;
#endif // CLASSIFIER_DEBUG

  unsigned int i=0;
  for (corpus_iter = corpus.begin(); corpus_iter != corpus.end(); corpus_iter++) {
    flag = false;
    // corpus_iter->first is a string. we are going to get corpus from that string and investigate
    key.assign(corpus_iter->first);
    value = corpus_iter->second;
#ifdef CLASSIFIER_DEBUG
    temp_wait = false;
    if (m_debug_level > 1) {
      std::cout << "analysing " << key << " = " << value << std::endl;
    }
#endif // CLASSIFIER_DEBUG
    temp_corpus.clear();
    switch (clean_type) {
      case 0:
        // case as in upper lower!
        strcpy((char *) str, key.c_str());
        i = 0;
        while (str[i]) {
          unsigned char ch = str[i];
          if (isupper(ch)) {
            ch += 32;
            str[i] = ch;
          }
          i++;
        }
        temp_double = 1;
        // std::cout << str << std::endl;
        temp_corpus.insert(std::pair<std::string, double> (std::string((char *)str), temp_double));
        break;
      case 1:
        // remove words from another test corpus
        break;
      case 2:
        if (key.find(" ") != std::string::npos)
          continue;
        memset((char*) str, '\0', 1024);
        sprintf((char*) str, "%d", (unsigned int) value);
        new_key = key + " " + std::string((const char*) str);
        if (GetCorpus(new_key, temp_corpus) < 0) {
          std::cerr << "ERROR: could not get corpus for: " << key << std::endl;
        }
        break;
      case 3:
        {
          strcpy((char *) str, key.c_str());
          unsigned char* ptr = str;
          unsigned char* end = ptr + key.length();
          bool flag = true;

          if (*ptr == ' ') {
            if (*(ptr + 1) == '#' || *(ptr + 1) == ' ') {
              flag = false;
            }
          }

          if (isdigit(*ptr)) {
            flag = false;
          }

          if (ispunct(*ptr)) {
            flag = false;
          }

          double temp_double = 1;
          unsigned int code_point=0;
          std::string script_temp;
          if (flag) {
            while (ptr && *ptr != '\0') {
              try {
                code_point = utf8::next(ptr, end);
                if (inagist_classifiers::DetectScript(code_point, script_temp) < 0) {
                  flag = false;
                  break;
                } else {
/*
                  if (script_temp.compare("en") != 0) {
                    flag = false;
                    break;
                  }
*/
                }
              } catch (...) {
                std::cout << "EXCEPTION: utf8 returned exception" << std::endl;
                std::cout << "problem string: '" << str << "'" << std::endl;
                break;
              }
            }
          }
          if (flag) {
            temp_corpus.insert(std::pair<std::string, double> (std::string((char *)str), temp_double));
          }
        }
        break;
      default:
        break;
    }

    for (temp_corpus_iter = temp_corpus.begin();
         temp_corpus_iter != temp_corpus.end();
         temp_corpus_iter++) {
      temp_key.assign(temp_corpus_iter->first);
      temp_value = temp_corpus_iter->second;
      if (key.compare(temp_key) == 0) {
#ifdef CLASSIFIER_DEBUG
        // std::cout << temp_key << " is same as orig " << key << ". ignored." << std::endl;
#endif // CLASSIFIER_DEBUG
        flag = true;
      } else {
        if ((trojan_corpus_iter = corpus.find(temp_key)) != corpus.end()) {
#ifdef CLASSIFIER_DEBUG
          std::cout << "incrementing " << trojan_corpus_iter->first \
                    << "=" << trojan_corpus_iter->second \
                    << " by " << value << std::endl;
          temp_wait = true;
#endif // CLASSIFIER_DEBUG
          trojan_corpus_iter->second += value;
        } else {
          new_corpus.insert(std::pair<std::string, double> (temp_corpus_iter->first, value));
          corpus.insert(std::pair<std::string, double> (temp_corpus_iter->first, value));
#ifdef CLASSIFIER_DEBUG
          std::cout << "inserting " << temp_corpus_iter->first << std::endl;
          temp_wait = true;
#endif // CLASSIFIER_DEBUG
        }
      }
    }
    temp_corpus.clear();
    if (!flag) {
      //std::cout << key << ", " << value_str << " is invalid. ignoring." << std::endl;
      invalid_corpus.insert(std::pair<std::string, double> (key, value));
#ifdef CLASSIFIER_DEBUG
      std::cout << "removing " << corpus_iter->first << std::endl;
#endif // CLASSIFIER_DEBUG
      corpus.erase(corpus_iter);
    }
#ifdef CLASSIFIER_DEBUG
    if (m_debug_level > 3) {
      if (wait) {
        if (temp_wait) {
        std::cout << "press any key to continue\n";
        std::cin >> input;
        if (input.compare("enuf") == 0) {
          wait = false;
        }
        }
      }
    }
#endif // CLASSIFIER_DEBUG
  }

  std::string new_file_name;
  if (output_suffix.compare("none") == 0) {
    // replace old file
    new_file_name = corpus_file_name;
  } else {
    new_file_name = corpus_file_name + "." + output_suffix;
  }
  std::cout << "writing corpus to file: " << new_file_name << std::endl;
  CorpusManager::WriteCorpusToFile(corpus, new_file_name);
  corpus.clear();

#ifdef CLASSIFIER_DEBUG
  new_file_name = corpus_file_name + ".new_";
  CorpusManager::WriteCorpusToFile(new_corpus, new_file_name);
#endif // CLASSIFIER_DEBUG
  new_corpus.clear();

#ifdef CLASSIFIER_DEBUG
  new_file_name = corpus_file_name + ".invalid_";
  CorpusManager::WriteCorpusToFile(invalid_corpus, new_file_name);
#endif // CLASSIFIER_DEBUG
  invalid_corpus.clear();

  return 0;
}

int Classifier::CleanCorpus(const bool& train_not_test,
                            unsigned int& input_type,
                            std::string& file_name,
                            std::string& output_suffix,
                            unsigned int& clean_type) {

  if (0 == input_type) {
    return CleanCorpusFile(file_name, output_suffix, clean_type);
  }

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified

  inagist_classifiers::Config config;
  if (inagist_classifiers::ClassifierConfig::Read(file_name.c_str(), config) < 0) {
    std::cerr << "ERROR: could not read config file: " << file_name << std::endl;
    return -1;
  }

  if (config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " << file_name << std::endl;
    return -1;
  }

  std::string corpus_file_name;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    if (train_not_test) {
      corpus_file_name = config.iter->training_corpus_file;
    } else {
      corpus_file_name = config.iter->testing_corpus_file;
    }
    std::cout << "cleaning: " << corpus_file_name << std::endl;
    if (CleanCorpusFile(corpus_file_name, output_suffix, clean_type) < 0) {
      std::cerr << "ERROR: could not clean corpus in file: " \
                << corpus_file_name << std::endl; 
      break;
    }
  }
  inagist_classifiers::ClassifierConfig::Clear(config);

  return 0;

}
#endif // CLASSIFIER_DATA_TRAINING_ENABLED

#if defined CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED

int Classifier::MakeDictionary(const char* classifier_dictionary_file) {

  if (m_corpus_map.empty() || !classifier_dictionary_file) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  CorpusMap* corpus_map = &(m_corpus_map);

  // just checking if the file can be opened. if not what is the point of calculating anything?
  std::ofstream ofs(classifier_dictionary_file);
  if (!ofs.is_open()) {
    std::cerr << "ERROR: could not open classifier dictionary file: " \
              << classifier_dictionary_file << std::endl;
    return -1;
  }
  ofs.close();

  CorpusMapIter corpus_map_iter;
  CorpusIter corpus_iter;
  Corpus* corpus_ptr;
  std::string class_name;
  std::string class_label;
  std::string class_number;
  std::string word;
  double freq;

  // a dictionary entry will look like this
  // word - class_name1:prob_value1, class_name2:prob_value2 etc
  std::map<std::string, std::map<std::string, double> > classifier_dictionary_map;
  std::map<std::string, std::map<std::string, double> >::iterator dict_iter;
  std::map<std::string, std::string>::iterator string_iter;
  for (corpus_map_iter = corpus_map->begin(); corpus_map_iter != corpus_map->end(); corpus_map_iter++) {
    // for this iteration, what is the class name and corpus?
    class_name = corpus_map_iter->first;
    if (class_name.empty()) {
      std::cerr << "ERROR: invalid class name. fatal error\n";
      break;
    }
    if ((string_iter = m_class_labels_map.find(class_name)) != m_class_labels_map.end()) {
      class_label = string_iter->second;
    }
    if ((string_iter = m_class_numbers_map.find(class_name)) != m_class_numbers_map.end()) {
      class_number = string_iter->second;
    }
    { // creating a scope
      std::map<std::string, double> class_map;
      freq = 0;
      class_map.insert(std::pair<std::string, double> (class_label, freq)); 
      classifier_dictionary_map.insert(std::pair<std::string, std::map<std::string, double> > \
                                       ("_" + class_number, class_map));
    }
    corpus_ptr = &(corpus_map_iter->second);
    if (corpus_ptr->empty()) {
#ifdef NBC_DEBUG
      std::cout << "WARNING: no entries found in corpus for class: " << class_name << std::endl;
#endif // NBC_DEBUG
      continue;
    }

    for (corpus_iter = corpus_ptr->begin();
         corpus_iter != corpus_ptr->end();
         corpus_iter++) {
#ifdef CLASSIFIER_DEBUG
      if (CLASSIFIER_DEBUG > 3) {
          std::cout << (*corpus_iter).first << " : " << (*corpus_iter).second \
                    << " in " << class_name << std::endl;
      }
#endif // CLASSIFIER_DEBUG
      word = (*corpus_iter).first;
      freq = (*corpus_iter).second;
      if ((dict_iter = classifier_dictionary_map.find(word)) != classifier_dictionary_map.end()) {
        dict_iter->second.insert(std::pair<std::string, double>("_" + class_number, freq)); 
      } else {
        std::map<std::string, double> class_map;
        class_map.insert(std::pair<std::string, double> ("_" + class_number, freq)); 
        classifier_dictionary_map.insert(std::pair<std::string, std::map<std::string, double> >(word, class_map));
      }
    }
  }

  // the next three steps can be merged into a single loop. not doing it for clarity.
  std::map<std::string, double>::iterator class_map_iter;

  // now lets calculate the idf component

  // each class is a document, so how does tf-idf work?
  // tf - number of times a word occurs in a class - here instead of td, probability Nc/N will be used
  // idf - is number of classes / number of classes in which this word occurs

  double idf = 0;
  double total_documents = corpus_map->size();

  for (dict_iter = classifier_dictionary_map.begin();
       dict_iter != classifier_dictionary_map.end();
       dict_iter++) {

    idf = log(total_documents/(double)dict_iter->second.size());
    idf *= 0.001; // this was found by trial and error - to make the value compatible with tf values

    for (class_map_iter = dict_iter->second.begin();
         class_map_iter != dict_iter->second.end();
         class_map_iter++) {
      class_map_iter->second += idf; // tf + idf for this word
    }
  }

  // write the dictionary map to file
  ofs.open(classifier_dictionary_file);
  if (!ofs.is_open()) {
    std::cerr << "ERROR: could not open classifier dictionary file: " \
              << classifier_dictionary_file << std::endl;
  } else {
    for (dict_iter = classifier_dictionary_map.begin();
         dict_iter != classifier_dictionary_map.end();
         dict_iter++) {
      ofs << dict_iter->first << "=";
      for (class_map_iter = dict_iter->second.begin();
           class_map_iter != dict_iter->second.end();
           class_map_iter++) {
        ofs << class_map_iter->first << ":" << class_map_iter->second << "|";
      }
      ofs << std::endl;
    }
    ofs.close();
  }

  // clear the dictionary map
  for (dict_iter = classifier_dictionary_map.begin();
       dict_iter != classifier_dictionary_map.end();
       dict_iter++) {
    dict_iter->second.clear();
  }
  classifier_dictionary_map.clear();

  return 0;
}

#endif // CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED

} // namespace inagist_classifiers
