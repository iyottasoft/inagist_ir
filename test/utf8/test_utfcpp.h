#ifdef _CPLUSPLUS
#include <string>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int test_detect_script(char* text, int text_len, char* script_buffer, int script_buffer_len);
int GetTestTweets(const char* user_name,
                  const unsigned int in_length,
                  char* tweets_buffer,
                  unsigned int *out_length);
#ifdef _CPLUSPLUS
}
#endif
