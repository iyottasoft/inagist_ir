#include "classifier.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include "corpus_manager.h"
#include "config_reader.h"
#include "twitter_searcher.h"
#include "twitter_api.h"

#ifdef DEBUG
#if DEBUG>0
#define CLASSIFIER_DEBUG DEBUG
#endif
#endif
//#define CLASSIFIER_DEBUG 3

namespace inagist_classifiers {

Classifier::Classifier() {
}

Classifier::~Classifier() {
}

int Classifier::GetTrainingData(const char* config_file_name) {

  if (!config_file_name) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  Config config;
  if (ConfigReader::Read(config_file_name, config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
    return -1;
  }

  if (config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " << config_file_name << std::endl;
    return -1;
  }

  int i = rand() % config.classes.size();
  int j = 0;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    if (j == i)
      break;
    j++;
  }

  int count = 0;
  int count_temp = 0;
  for (; config.iter != config.classes.end(); config.iter++) {
    if ((count_temp = GetTrainingData(config.iter->handles_file,
                                 config.iter->tweets_file,
                                 config.iter->corpus_file)) < 0) {
      std::cerr << "ERROR: could not get training data for handles in file: " \
                << config.iter->handles_file << std::endl; 
    } else {
      std::cout << "Corpus of size " << count_temp << " generated for " << config.iter->name << std::endl;
      count += count_temp;
    }
  }
  j = 0;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end() && j<i; config.iter++) {
    j++;
    if ((count_temp = GetTrainingData(config.iter->handles_file,
                                 config.iter->tweets_file,
                                 config.iter->corpus_file)) < 0) {
      std::cerr << "ERROR: could not get training data for handles in file: " \
                << config.iter->handles_file << std::endl; 
    } else {
      std::cout << "Corpus of size " << count_temp << " generated for " << config.iter->name << std::endl;
      count += count_temp;
    }
  }

  ConfigReader::Clear(config);

  return count;
}

int Classifier::GetTrainingData(const std::string& handle, Corpus& corpus, bool get_user_info) {

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  unsigned int count = 0;
  unsigned int count_temp = 0;

  if (get_user_info) {
    std::string user_info;
    if (inagist_api::TwitterAPI::GetUserInfo(handle, user_info) < 0) {
      std::cerr << "ERROR: could not get user info for handle: " << handle << std::endl;
    } else {
      if (user_info.length() > 0 && (count_temp = GetCorpus(user_info, corpus)) < 0) {
        std::cerr << "ERROR: could not find ngrams for user info string: " << user_info << std::endl;
      } else {
        count += count_temp;
      }
    }
  }

  if (twitter_searcher.GetTweetsFromUser(handle, tweets) > 0) {
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
  }
  tweets.clear();

  return count;
}

int Classifier::GetTrainingData(const std::string& twitter_handles_file_name,
                                const std::string& output_tweets_file_name,
                                const std::string& output_corpus_file_name) {

  std::ifstream ifs(twitter_handles_file_name.c_str());
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open file " << twitter_handles_file_name << std::endl;
    return -1;
  }

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
  bool no_new_handle = true;
  for (; handle_iter != handles.end(); handle_iter++) {
    if (handle_iter->second == 1)
      continue;
    no_new_handle = false;
    handle = handle_iter->first;

    // need user info. 0 indicates its the first time this handles is being processed
    if (handle_iter->second == 0) {
      count_temp = GetTrainingData(handle, corpus, true);
    } else {
      count_temp = GetTrainingData(handle, corpus, false);
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
      break;
    }
  }

  if (0 == flag) {
    j = 0;
    for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
      if (i==j) break; j++;
      if (handle_iter->second == 1)
        continue;
      no_new_handle = false;
      handle = handle_iter->first;

      // need user info
      if (handle_iter->second == 0) {
        count_temp = GetTrainingData(handle, corpus, true);
      } else {
        count_temp = GetTrainingData(handle, corpus, false);
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
        break;
      }
    }
  }

  if (no_new_handle) {
    // all the entries in the handles file may have been examined and hence have "1".
    // lets flip them all to 0.
    for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
      if (handle_iter->second == 1) {
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
      for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
         ofs << handle_iter->first << "=" << handle_iter->second << std::endl;
      }
      ofs.close();
    }
  }

  handles.clear();

  if (no_new_handle) {
    // recursive call
    return GetTrainingData(twitter_handles_file_name,
                           output_corpus_file_name,
                           output_tweets_file_name);
  }

  if (count == 0) {
    std::cout << "No tweets found for handles in file " << twitter_handles_file_name << std::endl;
    return 0;
  } else {
    if (CorpusManager::UpdateCorpusFile(corpus, output_corpus_file_name) < 0) {
      std::cerr << "ERROR: could not update features to output file " << output_corpus_file_name << std::endl;
    }
  }

  corpus.clear();

  return count;
}

int Classifier::GetTestData() {
  return 0;
}

} // namespace inagist_classifiers
