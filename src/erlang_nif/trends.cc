#ifdef _CPLUSPLUS
#include <cstring>
#else
#include <string.h>
#endif

#include "trends.h"
#include "trends_manager.h"

inagist_trends::TrendsManager g_tm;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitTrendsManager(const char* stopwords_file_path,
         const char* dictionary_file_path,
         const char* unsafe_dictionary_file_path,
         const char* lang_detect_config_file_path,
         const char* channels_dictionary_file_path) {

  if (g_tm.Init(stopwords_file_path,
           dictionary_file_path,
           unsafe_dictionary_file_path,
           lang_detect_config_file_path,
           channels_dictionary_file_path) < 0) {
    std::cerr << "ERROR: could not initialize trends manager\n";
    return -1;
  }

  return 0;
}

#ifdef _CPLUSPLUS
extern "C"
#endif
int GetKeywords(const unsigned char* text, const unsigned int text_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr,
                char* buffer1, const unsigned int buffer1_len,
                char* buffer2, const unsigned int buffer2_len,
                char* buffer3, const unsigned int buffer3_len,
                char* buffer4, const unsigned int buffer4_len) {

  int ret_value = 0;
  if ((ret_value = g_tm.GetKeyTuples(text, text_len,
                safe_status_buffer, safe_status_buffer_len,
                script_buffer, script_buffer_len,
                (unsigned char*) keywords_buffer, keywords_buffer_len,
                keywords_len_ptr, keywords_count_ptr,
                (unsigned char*) hashtags_buffer, hashtags_buffer_len,
                hashtags_len_ptr, hashtags_count_ptr,
                (unsigned char*) keyphrases_buffer, keyphrases_buffer_len,
                keyphrases_len_ptr, keyphrases_count_ptr,
                buffer1, buffer1_len,
                buffer2, buffer2_len,
                buffer3, buffer3_len,
                buffer4, buffer4_len)) < 0) {
    std::cerr << "ERROR: could not get keywords\n";
    return -1;
  }
  
  return ret_value;
}

// takes a pipe separated list of the structs
// <keywords, count, handle, class> and returns a purged list of the same
int GetTrends(char* trends_buffer, unsigned int* trends_len_ptr, unsigned int* trends_count_ptr) {

  if (!trends_buffer || *trends_len_ptr < 7) {
    std::cout << "ERROR: invalid input for GetTrends\n";
    return -1;
  }

  unsigned char* ptr = (unsigned char*) trends_buffer;
  while ((ptr = (unsigned char *) strstr((char *) ptr, "|")) != NULL) {
    (*trends_count_ptr)++;
    ptr++;
  }
  ptr = NULL;

  return 0;
}

int GetTestTrends(const char* trends_file_path,
                  unsigned char* trends_buffer, const unsigned int trends_buffer_len,
                  unsigned int *trends_len_ptr, unsigned int *trends_count_ptr) {

  *trends_count_ptr = 0;

  // get trends input
  std::ifstream ifs(trends_file_path);
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open trends file: " << trends_file_path << std::endl;
    return -1;
  }

  std::string line;
  unsigned char *ptr = trends_buffer;
  unsigned int len = 0;
  unsigned int total_len = 0;
  while (getline(ifs, line)) {
#ifdef TRENDS_DEBUG
    if (line.length() <= 0) {
      std::cerr << "Empty file\n";
      continue;
    }
    if (TRENDS_DEBUG > 2)
      std::cout << line << std::endl;
#endif

    len = line.length();
    total_len += (len + 1); // 1 for the pipe
    if (total_len < trends_buffer_len) {
      strcpy((char *) ptr, line.c_str());
      ptr += len;
      strcpy((char *) ptr, "|");
      ptr++;
      (*trends_count_ptr)++;
    } else {
#ifdef TRENDS_DEBUG
      std::cout << "Not enuf space in the tweets buffer\n";
#endif
      break;
    }
  
    (*trends_count_ptr)++;
  }
  *ptr = '\0';
  ifs.close();
  *trends_len_ptr = ptr - trends_buffer;
  ptr = NULL;

#ifdef TRENDS_DEBUG
  if (TRENDS_DEBUG) {
    std::cout << "trends len: " << *trends_len_ptr << std::endl;
    std::cout << "trends count: " << *trends_count_ptr << std::endl;
  }
#endif

  return *trends_count_ptr;
}
