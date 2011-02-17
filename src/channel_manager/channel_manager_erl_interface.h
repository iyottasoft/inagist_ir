/* channel_manager_erl_interface.h */

#ifndef _INAGIST_SEARCH_CHANNEL_MANAGER_ERL_INTERFACE_H_
#define _INAGIST_SEARCH_CHANNEL_MANAGER_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int Init(const char* config_file);
int SubmitTweet(const unsigned char *tweet, const unsigned int tweet_len,
                char *channels_buffer, const unsigned int channels_buffer_len,
                unsigned int *channels_count_ptr, unsigned int *channels_len_ptr);
int GetTestTweets(const char* user_name,
                  const unsigned int in_length,
                  char* tweets_buffer,
                  unsigned int *out_length);
int GetTestTweetsFromFile(const char* file_name,
                          const unsigned int in_length,
                          char *tweets_buffer,
                          unsigned int *out_length);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_SEARCH_CHANNEL_MANAGER_ERL_INTERFACE_H_
