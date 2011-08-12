#include "classifier.h"
#include <iostream>
#include <fstream>
#include <iostream>
#include <cstdlib>
#include <cstring>
#include "twitter_searcher.h"
#include "twitter_api.h"
#include "inagist_api.h"
#include "test_utils.h"

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
#endif
}

Classifier::~Classifier() {

  if (ClassifierConfig::Clear(m_config) < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not clear config\n";
#endif // CLASSIFIER_DEBUG
  }

  try {
    m_corpus_manager.Clear();
  } catch (...) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: Corpus Manager throws exception" << std::endl;
#endif // CLASSIFIER_DEBUG
  }

}

int Classifier::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  return 0;
}

int Classifier::Init(std::string config_file_name, bool ignore_history) {

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
    corpus_map_meta_data[m_config.iter->name] = m_config.iter->class_data_file;
#ifdef CLASSIFIER_DEBUG
    if (m_debug_level > 4) {
      std::cout << m_config.iter->name << " = " << m_config.iter->class_data_file << std::endl;
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
    if (m_corpus_manager.LoadCorpus(m_config.class_freqs_file,
                                    m_corpus_manager.m_classes_freq_map) < 0) {
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
#endif
  }

#ifdef CLASSIFIER_DEBUG
  if (m_debug_level > 1) {
    std::cout << "loading corpus_map from corpus_map_meta_data (while initializing classifier)\n";
  }
#endif // CLASSIFIER_DEBUG
  if (m_corpus_manager.LoadCorpusMap(corpus_map_meta_data) < 0) {
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

  return 0;
}

#ifdef CLASSIFIER_DATA_TRAINING_ENABLED

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
      if (NormalizeFrequencies(config.iter->corpus_file.c_str(),
                               config.iter->training_data_file.c_str()) < 0) {
        std::cerr << "ERROR - : could not normalize raw data file: " \
                  << config.iter->corpus_file << " to " \
                  << config.iter->training_data_file;
        break;
      }
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
      if (NormalizeFrequencies(config.iter->corpus_file.c_str(),
                               config.iter->training_data_file.c_str()) < 0) {
        std::cerr << "ERROR - : could not normalize raw data file: " \
                  << config.iter->corpus_file << " to " \
                  << config.iter->training_data_file;
        break;
      }
#ifdef CLASSIFIER_DEBUG
      if (m_debug_level > 1) {
        std::cout << "Corpus of size " << count_temp \
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

int Classifier::GetTrainingData(const std::string& class_name,
                                const std::string& twitter_handles_file_name,
                                const std::string& output_tweets_file_name,
                                const std::string& manual_seed_file_name,
                                unsigned int& output_num_docs,
                                const std::string& output_corpus_file_name,
                                unsigned int& output_corpus_size) {

  std::ifstream ifs(twitter_handles_file_name.c_str());
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open twitter handles file: " << twitter_handles_file_name << std::endl;
    return -1;
  }

#ifdef CLASSIFIER_DEBUG
  if (m_debug_level > 1) {
    std::cout << "INFO: training data from handles file: " << twitter_handles_file_name << std::endl;
  }
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
  int count_temp = 0;
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
  int count_temp = 0;

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
  char str[255];
  memset(str, '\0', 255);
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
        strcpy(str, key.c_str());
        i = 0;
        while (str[i]) {
          char ch = str[i];
          if (isupper(ch)) {
            ch += 32;
            str[i] = ch;
          }
          i++;
        }
        temp_double = 1;
        std::cout << str << std::endl;
        temp_corpus.insert(std::pair<std::string, double> (std::string(str), temp_double));
        break;
      case 1:
        // remove words from another test corpus
        break;
      case 2:
        if (key.find(" ") != std::string::npos)
          continue;
        memset(str, '\0', 255);
        sprintf(str, "%d", (unsigned int) value);
        new_key = key + " " + std::string(str);
        if (GetCorpus(new_key, temp_corpus) < 0) {
          std::cerr << "ERROR: could not get corpus for: " << key << std::endl;
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
    if (m_debug_level > 1) {
      if (wait) {
        if (temp_wait) {
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

int Classifier::CleanCorpus(unsigned int& input_type,
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
    corpus_file_name = config.iter->corpus_file;
    if (CleanCorpusFile(corpus_file_name, output_suffix, clean_type) < 0) {
      std::cerr << "ERROR: could not clean corpus in file: " \
                << config.iter->corpus_file << std::endl; 
      break;
    }
  }
  inagist_classifiers::ClassifierConfig::Clear(config);

  return 0;

}
#endif // CLASSIFIER_DATA_TRAINING_ENABLED

#ifdef CLASSIFIER_DATA_TESTING_ENABLED

int Classifier::WriteTestData(Corpus& corpus, const char* classes_freq_file) {

  if (corpus.empty() || !classes_freq_file) {
    std::cerr << "ERROR: invalid input. could not write test data\n";
    return -1;
  }

  inagist_classifiers::CorpusIter corpus_iter;
  double sum = 0;
  for (corpus_iter = corpus.begin(); corpus_iter != corpus.end(); corpus_iter++) {
    std::cout << corpus_iter->first << ":" << corpus_iter->second << std::endl;
    if ((corpus_iter->first.compare("UU") == 0) ||
        (corpus_iter->first.compare("XX") == 0) ||
        (corpus_iter->first.compare("RR") == 0)) {
      //std::cout << "deleting" << std::endl;
      corpus.erase(corpus_iter);
      continue;
    } else if (corpus_iter->first.compare("all_classes") != 0) {
      sum += corpus_iter->second;
    } else {
#ifdef CLASSIFIER_DEBUG
      std::cout << "all_classes. hence ignoring" << std::endl;
#endif // CLASSIFIER_DEBUG
    }
  }

  // update the all_classes value
  if ((corpus_iter = corpus.find("all_classes")) != corpus.end()) {
    corpus_iter->second = sum;
  } else {
    corpus.insert(std::pair<std::string, double> ("all_classes", sum));
  }

  // write to classes_freq file
  if (m_corpus_manager.UpdateCorpusFile(corpus, classes_freq_file) < 0) {
    std::cerr << "ERROR: could not update corpus file\n";
    return -1;
  }

  return corpus.size();
}

// this function assumes that the classifier is already initialized.
// and also assumes that the GetCorpus implementation by the derived class
// will work fine with its dependencies already initalized
//
// input_type:
//          0 - not implemented
//          1 - input file 
//          2 - single tweet
//          3 - multiple tweets
//          10 - random selection of training sources
//          11 - all training sources (all handles)
//          12 - only given training source will be tested 
//          13 - test input is taken from the given file
// output_type:
//          0 - stdout
//          1 - class frequency file
//          2 - html version
//          3 - test corpus files
//
// Note: the output file is to write texts that contributed to the class frequenceies.
//
int Classifier::GetTestData(const unsigned int& input_type,
                            const char* input_value,
                            const unsigned int& output_type,
                            const char* output_value) {

  if (output_type > 0 && output_type < 3) {
    if (NULL == output_value) {
      std::cerr << "ERROR: invalid output file. cannot get test data.\n";
      return -1;
    }
  }

  std::ofstream ofs;
  std::ostream* output_stream; // note ostream not ofstream
  Corpus test_freq_map;
  CorpusMap test_corpus_map;
  CorpusMapMeta test_corpus_map_meta_data;

  switch (output_type) {
    case 0:
      output_stream = &std::cout;
      break;
    case 1:
      ofs.open(output_value);
      if (!ofs.is_open()) {
        std::cerr << "ERROR: could not open output file: " << output_value << std::endl;
        return -1;
      }
      output_stream = &ofs;
      break;
    case 2:
      std::cout << "ERROR: not implemented yet\n";
      break;
    case 3:
      {
        output_stream = &std::cout;
        for (m_config.iter = m_config.classes.begin();
           m_config.iter != m_config.classes.end();
           m_config.iter++) {
          test_corpus_map_meta_data[m_config.iter->name] = m_config.iter->testing_data_file;
        }
        if (test_corpus_map_meta_data.empty()) {
          std::cerr << "ERROR: test_corpus_map_meta_data cannot be empty\n";
          return -1;
        }
#ifdef CLASSIFIER_DEBUG
        std::cout << "loading test_corpus_map to get testing data\n";
#endif // CLASSIFIER_DEBUG
/*
        if (CorpusManager::LoadCorpusMap(test_corpus_map_meta_data, test_corpus_map) < 0) {
          std::cerr << "ERROR: could not load Corpus Map\n";
          return -1;
        }
*/
      }
      break;
    default:
      break;
  }

  bool random_selection = false;
  const char* training_class = NULL;
  std::string handle;
  std::string expected_class_name;
  TestResult test_result;
  test_result.clear();
  const char* training_texts_file = NULL;

  switch (input_type) {
    case 0 ... 9:
      // send empty handle and expected class_name to test public timeline
      if (TestTimeline(input_type,
                       input_value,
                       expected_class_name,
                       test_freq_map,
                       test_corpus_map,
                       test_result,
                       *output_stream) < 0) {
        std::cerr << "ERROR: could not test twitter timeline\n";
        ofs.close();
        return -1;
      }
      break;
    case 10:
      if (TestTrainingSources(training_class=NULL,
                              test_freq_map,
                              test_corpus_map,
                              test_result,
                              *output_stream,
                              random_selection = true) < 0) {
        std::cerr << "ERROR: could not test training sources at random\n";
        ofs.close();
        return -1;
      }
      break;
    case 11:
      if (TestTrainingSources(training_class=NULL,
                              test_freq_map,
                              test_corpus_map,
                              test_result,
                              *output_stream,
                              random_selection = false) < 0) {
        std::cerr << "ERROR: could not test training sources at random\n";
        ofs.close();
        return -1;
      }
      break;
    case 12:
      if (TestTrainingSources(training_class=expected_class_name.c_str(),
                              test_freq_map,
                              test_corpus_map,
                              test_result,
                              *output_stream,
                              random_selection = true) < 0) {
        std::cerr << "ERROR: could not test training sources at random\n";
        ofs.close();
        return -1;
      }
      break;
    case 13:
        if (TestTrainingTexts(training_texts_file=input_value,
                              expected_class_name,
                              test_freq_map,
                              test_corpus_map,
                              test_result,
                              *output_stream) < 0) {
          std::cerr << "ERROR: could not test training sources from file: " \
                    << training_texts_file << std::endl;
          return -1;
        }
      break;
    default:
      break;
  }

  if (test_freq_map.empty()) {
    std::cout << "WARNING: no test data found\n";
    ofs.close();
    return 0;
  }

  int ret_val = 0;
  switch (output_type) {
    case 0:
      if (CorpusManager::PrintCorpus(test_freq_map) < 0) {
        std::cerr << "ERROR: could not print corpus\n";
        ret_val = -1;
      }
      break;
    case 1:
      if (WriteTestData(test_freq_map, m_config.test_freqs_file.c_str()) < 0) {
        std::cerr << "ERROR: could not write test data to file: " \
                  << m_config.test_freqs_file << std::endl;
        ret_val = -1;
      }
      break;
    case 2:
      break;
    case 3:
      if (!test_freq_map.empty()) {
        if (WriteTestData(test_freq_map, m_config.test_freqs_file.c_str()) < 0) {
          std::cerr << "ERROR: could not write test data to file: " \
                    << m_config.test_freqs_file << std::endl;
          ret_val = -1;
          break;
        }
      }
      if (!test_corpus_map.empty()) {
        if (CorpusManager::WriteCorpusMap(test_corpus_map, test_corpus_map_meta_data) < 0) {
          std::cerr << "ERROR: could not update test corpus files\n";
          ret_val = -1;
        }
      }
      break;
    default:
      break;
  }

  ofs.close();
  test_freq_map.clear();
  test_corpus_map.clear();
  test_corpus_map_meta_data.clear();

  std::cout << "Total text: " << test_result.total << std::endl;
  if (!expected_class_name.empty()) {
    std::cout << "Expected Class: " << expected_class_name << std::endl;
  }
  std::cout << "Correct: " << test_result.correct << std::endl;
  std::cout << "Wrong: " << test_result.wrong << std::endl;
  std::cout << "Undefined: " << test_result.undefined << std::endl;

  return ret_val;

}

int Classifier::TestTrainingTexts(const char* training_texts_file,
                                  const std::string& expected_class_name,
                                  Corpus& test_freq_map,
                                  CorpusMap& test_corpus_map,
                                  TestResult& test_result,
                                  std::ostream &output_stream) {

  if (!training_texts_file) {
    std::cerr << "ERROR: invalid input training texts file\n";
    return -1;
  }

  std::ifstream ifs(training_texts_file);
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open file with traning texts: " \
              << training_texts_file << std::endl;
    return -1;
  }

  std::string tweet;
  std::set<std::string> tweets;
  while (getline(ifs, tweet)) {
    tweets.insert(tweet);
  }
  ifs.close();

  if (tweets.empty()) {
    return 0;
  }

  std::set<std::string>::iterator set_iter;
  std::string output_class;
  std::string output_top_classes;
  unsigned int output_top_classes_count = 0;
  int ret_val = 0;
  Corpus test_corpus;

#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED

#ifdef CLASSIFIER_DEBUG
  if (CLASSIFIER_DEBUG > 2) {
    std::cout << "check corpus map" << std::endl;
    CorpusManager::PrintCorpusMap(m_corpus_manager.m_corpus_map);
  }
#endif

  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    test_result.total++;
    tweet = *set_iter;
    if ((ret_val = Classify(tweet, tweet.length(), output_class,
                            output_top_classes, output_top_classes_count
#ifdef CLASSIFIER_DATA_TESTING_ENABLED
                            , test_corpus
#endif // CLASSIFIER_DATA_TESTING_ENABLED
#ifdef CLASS_CONTRIBUTORS_ENABLED
                            , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                           )) < 0) {
      std::cerr << "ERROR: could not find class\n";
      test_result.undefined++;
    } else if (ret_val == 0) {
      if (output_stream) {
        output_stream << expected_class_name << "|" << output_class << "|" << tweet << std::endl;
      }
      test_result.undefined++;
    } else {
      if (output_stream) {
        output_stream << expected_class_name << "|" << output_class << "|" << tweet << std::endl;
      }
      if (!expected_class_name.empty()) {
        if (expected_class_name.compare(output_class) == 0) {
          test_result.correct++;
        } else if ((output_class.compare(0,2,"UU") == 0) ||
                   (output_class.compare(0,2,"XX") == 0) ||
                   (output_class.compare(0,2,"RR") == 0)) {
           test_result.undefined++;
        } else {
          test_result.wrong++;
        }
      }
      if (test_freq_map.find(output_class) != test_freq_map.end()) {
        test_freq_map[output_class] += 1;
      } else {
        test_freq_map[output_class] = 1;
      }
      if (!test_corpus.empty()) {
        if (m_corpus_manager.UpdateCorpusMap(test_corpus_map, output_class, test_corpus) < 0) {
          std::cerr << "ERROR: could not update test_corpus_map for class: " \
                    << output_class << std::endl;
        }
      }
    }
    test_corpus.clear();
  }

  tweets.clear();

  return test_freq_map.size();
}

int Classifier::TestTimeline(const unsigned int& input_type,
                             const char* input_value,
                             const std::string& expected_class_name,
                             Corpus& test_freq_map,
                             CorpusMap& test_corpus_map,
                             TestResult& test_result,
                             std::ostream &output_stream) {

  std::set<std::string> texts;

  if (inagist_utils::GetInputText(input_type, input_value, texts) < 0) {
    std::cerr << "ERROR: could not input texts\n";
    return -1;
  }

  if (texts.empty()) {
    return 0;
  }

  int ret_value = TestTimeline(texts,
                      expected_class_name,
                      test_freq_map,
                      test_corpus_map,
                      test_result,
                      output_stream
                     );

  texts.clear();

  return ret_value;
}

int Classifier::TestTimeline(const std::set<std::string>& texts,
                             const std::string& expected_class_name,
                             Corpus& test_freq_map,
                             CorpusMap& test_corpus_map,
                             TestResult& test_result,
                             std::ostream &output_stream) {

  std::set<std::string>::iterator set_iter;
  std::string text;
  std::string output_class;
  std::string output_top_classes;
  unsigned int output_top_classes_count = 0;
  int ret_val = 0;
#ifdef CLASSIFIER_DATA_TESTING_ENABLED
  Corpus test_corpus;
#endif // CLASSIFIER_DATA_TESTING_ENABLED
#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED

#ifdef CLASSIFIER_DEBUG
  if (CLASSIFIER_DEBUG > 4) {
    std::cout << "check corpus map" << std::endl;
    CorpusManager::PrintCorpusMap(m_corpus_manager.m_corpus_map);
  }
#endif

  for (set_iter = texts.begin(); set_iter != texts.end(); set_iter++) {
    test_result.total++;
    text = *set_iter;

#ifdef CLASSIFIER_DEBUG
    if (CLASSIFIER_DEBUG > 1) {
      std::cout << "Classifying text: " << text << std::endl;
    }
#endif

    if ((ret_val = Classify(text, text.length(), output_class,
                            output_top_classes, output_top_classes_count
#ifdef CLASSIFIER_DATA_TESTING_ENABLED
                            , test_corpus
#endif // CLASSIFIER_DATA_TESTING_ENABLED
#ifdef CLASS_CONTRIBUTORS_ENABLED
                            , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                           )) < 0) {
      std::cerr << "ERROR: could not find class\n";
      test_result.undefined++;
    } else if (ret_val == 0) {
      if (output_stream) {
        output_stream << expected_class_name << "|" \
                      << output_class << "|(" \
                      << output_top_classes << ")|" \
                      << text << std::endl;
      }
      test_result.undefined++;
    } else {
      if (output_stream) {
        output_stream << expected_class_name << "|" \
                      << output_class << "|(" \
                      << output_top_classes << ")|" \
                      << text << std::endl;
      }
      if (!expected_class_name.empty()) {
        if (expected_class_name.compare(output_class) == 0) {
          test_result.correct++;
        } else if ((output_class.compare(0,2,"UU") == 0) ||
                   (output_class.compare(0,2,"XX") == 0) ||
                   (output_class.compare(0,2,"RR") == 0)) {
           test_result.undefined++;
        } else {
          test_result.wrong++;
        }
      }
      if (test_freq_map.find(output_class) != test_freq_map.end()) {
        test_freq_map[output_class] += 1;
      } else {
        test_freq_map[output_class] = 1;
      }
      if (!test_corpus.empty()) {
        if (m_corpus_manager.UpdateCorpusMap(test_corpus_map, output_class, test_corpus) < 0) {
          std::cerr << "ERROR: could not update test_corpus_map for class: " \
                    << output_class << std::endl;
        }
      }
    }
#ifdef CLASSIFIER_DATA_TESTING_ENABLED
    test_corpus.clear();
#endif // CLASSIFIER_DATA_TESTING_ENABLED
  }

  return test_freq_map.size();
}

// training_class - this can be null. when not null, only the handles in the given class will be tested
// class_freq_map - output parameter for returning classes and their frequencies
// test_result - test result is a typedef to return the results of this testing exercise
// output_stream - if an output file is given, its fstream will be pointed by output_stream or else stdout
// random_selection - while choosing input from training sources, whether to randomly select classes and handles
int Classifier::TestTrainingSources(const char* training_class,
                                    Corpus& class_freq_map,
                                    CorpusMap& test_corpus_map,
                                    TestResult& test_result,
                                    std::ostream &output_stream,
                                    bool random_selection) {

  std::set<std::string> handles_set;
  std::set<std::string>::iterator set_iter;
  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {

    std::string class_name = m_config.iter->name;

    if (NULL != training_class) {
      if (class_name.compare(0, strlen(training_class), training_class) != 0) {
        continue;
      }
    }

    std::ifstream hfs(m_config.iter->handles_file.c_str());

    if (!hfs.is_open()) {
      std::cout << "ERROR: could not open handles file: " << m_config.iter->handles_file \
                << " for class: " << class_name << std::endl;
      continue;
    }

    std::string line;
    std::string::size_type loc;
    std::string handle;
    while (getline(hfs, line)) {
      loc = line.find("=", 0);
      if (loc != std::string::npos) {
        handle.assign(line, 0, loc);
        handles_set.insert(handle);
      } else {
        std::cerr << "ERROR: ill-written handles file\n";
      }
    }
    hfs.close();

    if (handles_set.size() <= 1) {
      continue;
    }

    unsigned int input_type = 0;
    const char* input_value = NULL;
    if (random_selection) {
      unsigned int index = rand();
      index = index % handles_set.size();
      if (index > 0 && index >= handles_set.size()) {
        continue;
      }

      unsigned int temp_index = 0;
      for (set_iter = handles_set.begin(); set_iter != handles_set.end(); set_iter++) {
        if (temp_index == index) {
          handle = *set_iter;
          break;
        }
        temp_index++;
      }
      handles_set.clear();

      if (TestTimeline(input_type = 3,
                       input_value = handle.c_str(),
                       class_name,
                       class_freq_map,
                       test_corpus_map,
                       test_result,
                       output_stream) < 0) {
        std::cout << "ERROR: TestTimeline failed for class: " \
                  << class_name << " on handle: " << handle << std::endl;
      }
    } else {
      for (set_iter = handles_set.begin(); set_iter != handles_set.end(); set_iter++) {
        handle = *set_iter;
        if (TestTimeline(input_type = 3,
                       input_value = handle.c_str(),
                       class_name,
                       class_freq_map,
                       test_corpus_map,
                       test_result,
                       output_stream) < 0) {
          std::cout << "ERROR: TestTimeline failed for class: " \
                    << class_name << " on handle: " << handle << std::endl;
        }
      }
    }

/*
    if (CorpusManager::PrintCorpus(class_freq_map) < 0) {
      std::cerr << "ERROR: could not print corpus\n";
      break;
    }
*/
  } // end of for loop

  return 0;
}

#endif // CLASSIFIER_DATA_TESTING_ENABLED

#if defined CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED

int Classifier::NormalizeFrequencies(const char* raw_data_file,
                                     const char* relative_freq_file) {

  if (!raw_data_file || !relative_freq_file) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: invalid input\n";
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  Corpus raw_data_corpus;
  if (CorpusManager::LoadCorpus(raw_data_file, raw_data_corpus) < 0) {
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR: could not load raw corpus from file: " \
              << raw_data_file << std::endl;
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  if (raw_data_corpus.size() < 2) {
    raw_data_corpus.clear();
    return 0;
  }

  CorpusIter corpus_iter;
  double vocabulary_size = (double) raw_data_corpus.size();
  double vocabulary_sum = 0;
  for (corpus_iter = raw_data_corpus.begin(); corpus_iter != raw_data_corpus.end(); corpus_iter++) {
    vocabulary_sum += corpus_iter->second;
  }
  
  double normalizing_denominator = vocabulary_sum + vocabulary_size;
  if (normalizing_denominator <= 1) {
    raw_data_corpus.clear();
    return 0;
  }

  for (corpus_iter = raw_data_corpus.begin(); corpus_iter != raw_data_corpus.end(); corpus_iter++) {
    raw_data_corpus[corpus_iter->first] = (corpus_iter->second / normalizing_denominator);
  }

  if (CorpusManager::WriteCorpusToFile(raw_data_corpus, relative_freq_file) < 0) {
    raw_data_corpus.clear();
#ifdef CLASSIFIER_DEBUG
    std::cerr << "ERROR - could not write normalized values to " \
              << relative_freq_file << std::endl;
#endif // CLASSIFIER_DEBUG
    return -1;
  }

  raw_data_corpus.clear();

  return 0;
}

int Classifier::NormalizeFrequencies() {

  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {
    if (NormalizeFrequencies(m_config.iter->corpus_file.c_str(),
                             m_config.iter->training_data_file.c_str()) < 0) {
#ifdef CLASSIFIER_DEBUG
      std::cerr << "ERROR - could not normalize frequencies for: " << m_config.iter->corpus_file << std::endl;
#endif // CLASSIFIER_DEBUG
      break;
    }
  }

  return 0;
}

#endif // CLASSIFIER_DATA_TRAINING_ENABLED || CLASSIFIER_DATA_TESTING_ENABLED

} // namespace inagist_classifiers
