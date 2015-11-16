#ifdef _CPLUSPLUS
#include <cstring>
#include <iostream>
#include <fstream>
#else
#include <string.h>
#endif

#include "trends_manager_cppi.h"
#include "trends_manager.h"

inagist_trends::TrendsManager g_tm;

#ifdef _CPLUSPLUS
extern "C"
#endif
int InitTrendsManager(const char* keytuples_config_file) {

  if (g_tm.Init(keytuples_config_file) < 0) {
    std::cerr << "ERROR: could not initialize trends manager\n";
    return -1;
  }

  return 0;
}

// takes a pipe separated list of tweets and returns a list of trends
int GetTrends(char* docs_buffer, unsigned int docs_len, unsigned int docs_count,
              char* trends_buffer, unsigned int* trends_len_ptr, unsigned int* trends_count_ptr) {

  if (!docs_buffer || docs_len <= 0 || !trends_buffer) {
    std::cout << "ERROR: invalid input for GetTrends\n";
    return -1;
  }

  strcpy(trends_buffer, "Not Implemented Yet");
  *trends_len_ptr = strlen(trends_buffer);
  *trends_count_ptr = 1;

  return *trends_count_ptr;
}

// takes a pipe separated list of the structs
// <named_entities, count, handle, class> and returns a purged list of the same
int ProcessTrends(char* trends_buffer, unsigned int* trends_len_ptr, unsigned int* trends_count_ptr) {

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

// this returns a pipe separated list of trends from a file.
// these trends can then be sent to GetTrends to purge
int GetTestTrendsFromFile(const char* trends_file_path,
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
