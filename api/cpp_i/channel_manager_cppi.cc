/* channel_manager_cppi.cc */

#include "channel_manager_cppi.h"

#ifdef _CPLUSPLUS
#include <string>
#include <cstring>
#include <iostream>
#endif

#include "channel_manager.h"

inagist_classifiers::ChannelManager g_channel_manager;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitChannelManager(const char* channels_dictionary_file) {

  if (!channels_dictionary_file)
    return -1;

  if (g_channel_manager.Init(channels_dictionary_file) < 0)
    return -1;

  return 0;
}

// named_entities and keyphrases are output parameters
#ifdef _CPLUSPLUS
extern "C"
#endif
int FindChannels(const unsigned char *tweet, const unsigned int tweet_len,
                 char *channels_buffer, const unsigned int channels_buffer_len,
                 unsigned int *channels_count_ptr, unsigned int *channels_len_ptr) {

#ifdef CM_DEBUG
  std::cout << tweet << std::endl;
#endif

  int ret_value = 0;
  unsigned int channels_count = 0;
  unsigned int channels_len = 0;
  if ((ret_value = g_channel_manager.FindChannels(tweet, tweet_len,
                   channels_buffer, channels_buffer_len, channels_count, channels_len, false)) < 0) {
    strcpy(channels_buffer, "ERR");
    channels_count = 1;
    channels_len = 3;
  }
  *channels_count_ptr = channels_count;
  *channels_len_ptr = channels_len;

  return ret_value;
}

