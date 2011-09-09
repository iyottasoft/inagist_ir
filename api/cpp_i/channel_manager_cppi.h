/* channel_manager_cppi.h */

#ifndef _INAGIST_ERLANG_API_CHANNELS_H_
#define _INAGIST_ERLANG_API_CHANNELS_H_

#ifdef _CPLUSPLUS
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitChannelManager(const char* config_file);
int FindChannels(const unsigned char *tweet, const unsigned int tweet_len,
                char *channels_buffer, const unsigned int channels_buffer_len,
                unsigned int *channels_count_ptr, unsigned int *channels_len_ptr);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_ERLANG_API_CHANNELS_H_
