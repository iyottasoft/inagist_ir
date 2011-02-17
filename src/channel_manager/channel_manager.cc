#include "channel_manager.h"
#include <iostream>
#include <fstream>
#include "twitter_searcher.h"
#include "string_utils.h"

#ifdef DEBUG
#if DEBUG>0
//#define CM_DEBUG DEBUG
#endif
#endif
#define CM_DEBUG 2

namespace inagist_classifiers {

ChannelManager::ChannelManager() {
#ifdef CM_DEBUG
  m_debug_level = CM_DEBUG;
#else
  m_debug_level = 0;
#endif
}

ChannelManager::~ChannelManager() {
  Clear();
}

int ChannelManager::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  return 0;
}

int ChannelManager::Init(std::string config_file_name) {

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified

  std::ifstream ifs(config_file_name.c_str());
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open config file " << config_file_name << std::endl;
    return -1;
  } else {
    std::string line;
    std::string key;
    std::string value;
    std::string::size_type loc;
    int line_count = 0;
    std::string handles_file_name;
    std::string tweets_file_name;
    std::string corpus_file_name;
    std::string corpus_class_name;
    std::map<std::string, std::string> corpus_class_file_map;
    while (getline(ifs, line)) {
      line_count++;
      // std::cout << line << std::endl;
      loc = line.find("=", 0);
      if (loc == std::string::npos) {
        std::cout << "ERROR: invalid config file entry\n";
        break;
      }
      key.assign(line.c_str(), loc);
      value.assign(line.c_str(), loc+1, (line.length()-loc-1));
      if (key.compare(0, 7, "handles") == 0) {
        handles_file_name = value;
#ifdef CM_DEBUG
        if (CM_DEBUG > 1)
          std::cout << "handles: " << handles_file_name << std::endl;
#endif
      } else if (key.compare(0, 6, "corpus") == 0) {
        corpus_file_name = value;
#ifdef CM_DEBUG
        if (CM_DEBUG > 1)
          std::cout << "corpus: " << corpus_file_name << std::endl;
#endif
      } else if (key.compare(0, 6, "tweets") == 0) {
        tweets_file_name = value;
#ifdef CM_DEBUG
        if (CM_DEBUG > 1)
          std::cout << "tweets: " << tweets_file_name << std::endl;
#endif
      }
    }
    ifs.close();
  }

  return 0;
}

int ChannelManager::FindChannels(const unsigned char* text,
                                 const unsigned int &text_len,
                                 char *channels_buffer,
                                 const unsigned int &channels_buffer_len,
                                 unsigned int &channels_count,
                                 unsigned int &channels_len,
                                 bool ignore_case) {
  // this should probably call FindChannels with a word set
  strcpy(channels_buffer, "001");
  channels_count = 1;
  channels_len = 3;

  return channels_count;
}

int ChannelManager::FindChannels(std::set<std::string>& words_set,
                                 char *channels_buffer,
                                 const unsigned int &channels_buffer_len,
                                 unsigned int &channels_count,
                                 unsigned int &channels_len,
                                 bool ignore_case) {

  // this should look up some hash table or trie to give the channel
  strcpy(channels_buffer, "001");
  channels_count = 1;
  channels_len = 3;

  return channels_count;
}

int ChannelManager::Clear() {
  /*
  try {
  } catch (...) {
    std::cerr << "ERROR: Corpus Manager throws exception" << std::endl;
  }
  */
  return 0;
}

} // namespace inagist_classifiers

