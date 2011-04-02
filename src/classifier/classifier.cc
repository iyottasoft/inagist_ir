#include "classifier.h"
#include <iostream>
#include <fstream>
#include "corpus_manager.h"
#include "config_reader.h"
#include "twitter_searcher.h"

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

  int count = 0;
  int count_temp = 0;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    if ((count_temp = GetTrainingData(config.iter->handles_file,
                                 config.iter->tweets_file,
                                 config.iter->corpus_file)) < 0) {
      std::cout << "ERROR: could not get training data for handles in file: " \
                << config.iter->handles_file << std::endl; 
    } else {
      std::cout << "Corpus of size " << count_temp << " generated for " << config.iter->name << std::endl;
      count += count_temp;
    }
  }
  ConfigReader::Clear(config);

  return count;
}

int Classifier::GetTrainingData(const std::string& handle, Corpus& corpus) {

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  unsigned int count = 0;
  unsigned int count_temp = 0;
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
  if (!ifs) {
    std::cout << "ERROR: could not open file " << twitter_handles_file_name << std::endl;
    return -1;
  }

  std::string handle;
  std::set<std::string> handles;
  while(getline(ifs, handle)) {
    handles.insert(handle);
  }
  ifs.close();

  if (handles.size() < 1) {
    std::cout << "ERROR: no handles found in file " << twitter_handles_file_name << std::endl;
    return 0;
  }

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  std::set<std::string>::iterator handle_iter;
  unsigned int num_tweets = 0;
  unsigned int count = 0;
  unsigned int count_temp = 0;
  Corpus corpus;
  for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
    if (twitter_searcher.GetTweetsFromUser(*handle_iter, tweets) > 0) {
      num_tweets += tweets.size();
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
      tweets.clear();
    }
  }
  handles.clear();

  if (num_tweets == 0) {
    std::cout << "No tweets found for handles in file " << twitter_handles_file_name << std::endl;
    return 0;
  } else {
    if (CorpusManager::WriteCorpusToFile(corpus, output_corpus_file_name) < 0) {
      std::cout << "ERROR: could not write to features to output file " << output_corpus_file_name << std::endl;
    }
  }

  corpus.clear();

  return count;
}

int Classifier::GetTestData() {
  return 0;
}

} // namespace inagist_classifiers
