#ifndef _INAGIST_CLASSIFIERS_CHANNEL_MANAGER_H_
#define _INAGIST_CLASSIFIERS_CHANNEL_MANAGER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <map>
#include <set>

namespace inagist_classifiers {

typedef std::map<std::string, std::string> ChannelsMap;
typedef std::map<std::string, std::string>::iterator ChannelsMapIter;

class ChannelManager {
 public:
  ChannelManager();
  ~ChannelManager();
  int Init(const char* channels_dictionary_file);
  int FindChannels(std::string& text,
                   std::set<std::string>& channels);
  int FindChannels(const unsigned char *text,
                   const unsigned int &text_len,
                   char *channels_buffer,
                   const unsigned int &channels_buffer_len,
                   unsigned int &channels_count,
                   unsigned int &channels_len,
                   bool ignore_case=false);
  int FindChannels(std::set<std::string>& words_set,
                   char *channels_buffer,
                   const unsigned int &channels_buffer_len,
                   unsigned int &channels_count,
                   unsigned int &channels_len,
                   bool ignore_case=false);
  int Clear();
  int SetDebugLevel(unsigned int debug_level);
  
 private:
  unsigned int m_debug_level;
  ChannelsMap m_channels_dictionary;

  DISALLOW_COPY_AND_ASSIGN(ChannelManager); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_CHANNEL_MANAGER_H_
