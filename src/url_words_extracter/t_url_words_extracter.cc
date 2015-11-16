#include <iostream>
#include <cstring>
#include <cstdlib>
#include <cassert>
#include <string>
#include "url_words_extracter.h"
#include "test_utils.h"

#define MAX_BUFFER_LEN 1024

inagist_trends::URLwordsExtracter g_ue;

int GetURLwords(std::string url) {

  if (url.empty())
    return 0;

  unsigned char url_buffer[MAX_BUFFER_LEN];
  unsigned int url_buffer_len = MAX_BUFFER_LEN;
  strcpy((char *) url_buffer, url.c_str()); 
  unsigned int url_len = url.length();

  std::cout << std::endl << url_buffer << std::endl;

  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  unsigned int keywords_buffer_len = MAX_BUFFER_LEN;
  keywords_buffer[0] = '\0';
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  if (g_ue.GetURLwords(url_buffer, url_buffer_len, url_len,
                       keywords_buffer, keywords_buffer_len,
                       keywords_len, keywords_count
                      ) < 0) {
    std::cout << "ERROR: could not get url_words\n";
  } else {
    std::cout << "url_words: ";
    if (keywords_len > 0) {
      std::cout << std::string((char *) keywords_buffer);
    }
    std::cout << std::endl;
  }

  memset(url_buffer, 0, 1024);

  return 0;
}

int main(int argc, char *argv[]) {

  if (argc < 2 || argc > 4) {
    std::cout << "Usage: " << argv[0] \
                           << "\n\t<0/1/8, 0-interactive, 1-file, 8-twitter urls>" \
                           << "\n\t[debug_level]" \
                           << "\n\t[file_name/handle]\n";
    return -1;
  }

  std::string bin_location = std::string(argv[0]);
  std::string::size_type loc = bin_location.find("bin", 0);
  std::string root_dir;
  if (loc == std::string::npos) {
    std::cout << "ERROR: could not find bin location\n" << std::endl;
    return -1;
  } else {
    root_dir = std::string(bin_location, 0, loc);
  }

  std::string stopwords_file = root_dir + "/data/static_data/stopwords.txt";
  std::string dictionary_file = root_dir + "/data/static_data/dictionary.txt";
  std::string unsafe_dictionary_file = root_dir + "/data/static_data/unsafe_dictionary.txt";

  int input_type = atoi(argv[1]);
  assert((input_type >= 0 && input_type <= 3) || input_type == 8);
  const char* input_value = NULL;

  // initialize keywords extracter
  if (g_ue.Init(stopwords_file.c_str(),
                dictionary_file.c_str(),
                unsafe_dictionary_file.c_str()
               ) < 0) {
    std::cerr << "ERROR: couldn't initialize keywordsExtracter\n";
    return -1; 
  }

  unsigned int debug_level = 0;
  if (argc >= 3) {
    debug_level = atoi(argv[2]);
    if (debug_level > 0) {
      g_ue.SetDebugLevel(debug_level);
    }
  }

  if (4 == argc) {
    input_value = argv[3];
  }

  std::string url;
  std::set<std::string> urls;
  std::set<std::string>::iterator set_iter;

  if (0 == input_type) {
    while (getline(std::cin, url)) {
      if (url.compare("exit") == 0 || url.compare("quit") == 0)
        break;
      GetURLwords(url);
    }
  } else {
    if (inagist_utils::GetInputText(input_type, input_value, urls) < 0) {
      std::cerr << "ERROR: could not input urls\n";
      return -1;
    }
    for (set_iter = urls.begin(); set_iter != urls.end(); set_iter++) {
      GetURLwords(*set_iter);
      if (2 == input_type)
        break;
    }
  }

  urls.clear();

  return 0;
}
