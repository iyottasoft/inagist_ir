#ifdef _CPLUSPLUS
extern "C"
#endif
int GetTestTweetsFromFile(const char* file_name,
                          const unsigned int in_length,
                          char *tweets_buffer,
                          unsigned int *out_length);

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetTestTweets(const char* user_name,
                  const unsigned int in_length,
                  char* tweets_buffer,
                  unsigned int *out_length);

