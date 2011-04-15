#include "channel_manager.h"
#include <iostream>
#include <fstream>
#include <cstring>
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

int ChannelManager::Init(const char* channels_dictionary_file) {

  std::ifstream ifs(channels_dictionary_file);
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open channels dictionary file " << channels_dictionary_file << std::endl;
    return -1;
  } else {
    std::string line;
    std::string key;
    std::string value;
    std::string::size_type loc;
    while (getline(ifs, line)) {
      // std::cout << line << std::endl;
      loc = line.find("=", 0);
      if (loc == std::string::npos) {
        std::cout << "ERROR: invalid dictionary file entry\n";
        break;
      }
      key.assign(line.c_str(), loc);
      value.assign(line.c_str(), loc+1, (line.length()-loc-1));
      m_channels_dictionary.insert(std::pair<std::string, std::string>(key, value));
    }
    ifs.close();
  }

  return 0;
}

int ChannelManager::FindChannels(std::string& text,
                                 std::set<std::string>& channels_set) {

  unsigned int text_len = text.length();
  if (text_len < 1 || text_len > MAX_BUFFER_LEN) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  unsigned char buffer[MAX_BUFFER_LEN];
  strcpy((char *) buffer, text.c_str());
  char channels[MAX_BUFFER_LEN];
  channels[0] = '\0';
  unsigned int channels_count = 0;
  unsigned int channels_len = 0;
  char temp_char;
  if (FindChannels(buffer, text.length(),
                   channels, MAX_BUFFER_LEN,
                   channels_count, channels_len,
                   true) < 0) {
    std::cerr << "ERROR: could not find channels\n";
  } else {
    if (channels_count > 0 && channels_len > 0) {
      char* start = channels;
      char* end = strstr(channels, "|");
      while (start && end && end != '\0' && start < end) {
        temp_char = *end;
        *end = '\0';
        channels_set.insert(std::string(start));
        *end = temp_char;
      }
    }
  }
  return channels_count;
}

int ChannelManager::FindChannels(const unsigned char* text,
                                 const unsigned int &text_len,
                                 char *channels_buffer,
                                 const unsigned int &channels_buffer_len,
                                 unsigned int &channels_count,
                                 unsigned int &channels_len,
                                 bool ignore_case) {

  std::set<std::string> tokens;
  if (inagist_utils::Tokenize((const char*) text, tokens) < 0) {
    std::cout << "ERROR: could not tokenize\n";
    return -1;
  }

  int ret_value = 0;
  if (FindChannels(tokens, channels_buffer, channels_buffer_len,
                   channels_count, channels_len, ignore_case) < 0) {
    std::cerr << "ERROR: could not find channels\n";
    ret_value = -1;
  } else {
    ret_value = channels_count;
  }

  tokens.clear();

  return ret_value;
}

int ChannelManager::FindChannels(std::set<std::string>& words_set,
                                 char *channels_buffer,
                                 const unsigned int &channels_buffer_len,
                                 unsigned int &channels_count,
                                 unsigned int &channels_len,
                                 bool ignore_case) {

  // this should look up some hash table or trie to give the channel
  channels_count = 0;
  ChannelsMapIter channels_map_iter;
  std::set<std::string>::iterator set_iter;
  for (set_iter = words_set.begin(); set_iter != words_set.end(); set_iter++) {
    if ((channels_map_iter = m_channels_dictionary.find(*set_iter)) != m_channels_dictionary.end()) {
      strcpy(channels_buffer, channels_map_iter->second.c_str());
      channels_count++;
    }
  }
  channels_len = strlen(channels_buffer);

  return channels_count;
}

int ChannelManager::Clear() {
  try {
    if (!m_channels_dictionary.empty()) {
      m_channels_dictionary.clear();
    }
  } catch (...) {
    std::cerr << "ERROR: Channel Manager exception" << std::endl;
  }
  return 0;
}

} // namespace inagist_classifiers

