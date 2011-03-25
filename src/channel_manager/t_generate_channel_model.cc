#include "channel_manager.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include "twitter_api.h"

#define MAX_TEST_BUFFER_LEN 1024
int main(int argc, char* argv[]) {

  // TODO (balaji) use getopt for heaven's sake!
  if (argc < 3 || argc > 5) {
    std::cout << "Usage: " << argv[0] << " \n\t<0/1/2, 0-interactive|1-file/2-tweet/3-manytweets> \n\t<0/1/2, 0-create|1-test|2-clean> \n\t[<input_file_path>]\n\t[<output_file_path>]\n";
    return -1;
  }

  unsigned int input_type = atoi(argv[1]);
  assert((input_type >= 0 && input_type <= 3));

  unsigned int test_type = atoi(argv[2]);
  assert(test_type >= 0 && test_type <= 2);

  std::string arguments(argv[0]);
  std::string::size_type loc = arguments.find("bin", 0);
  std::string root_dir;
  if (loc != std::string::npos) {
    loc-=1;
    root_dir.assign(argv[0], loc);
  }

  std::string stopwords_file = root_dir + "/data/static_data/stopwords.txt";
  std::string dictionary_file = root_dir + "/data/static_data/dictionary.txt";
  std::string unsafe_dictionary_file = root_dir + "/data/static_data/unsafe_dictionary.txt";
  std::string lang_detect_config_file = root_dir + "/configs/language_detection.config";

  std::string output_file_path;
  std::string input_file_path;
  if (4 == argc) {
    if (1 == input_type) {
      input_file_path = std::string(argv[3]);
    } else {
      output_file_path = std::string(argv[3]);
    }
  } else if (5 == argc) {
      input_file_path = std::string(argv[3]);
      output_file_path = std::string(argv[4]);
  }

  inagist_classifiers::ChannelManager cm;

  std::string text;
  if (0 == input_type) {
    while (getline(std::cin, text)) {
      if (text.compare("exit") == 0 || text.compare("quit") == 0) {
        break;
      }
    }
    return 0;
  }

  std::set<std::string> tweets;
  if (1 == input_type) {
    if (argc < 4) {
      std::cout << "ERROR: input file needed\n";
      return -1;
    }
    std::ifstream ifs(input_file_path.c_str());
    if (!ifs.is_open()) {
      std::cout << "ERROR: could not open input file ";
      std::cout << input_file_path << std::endl;
    } else {
      while (getline(ifs, text)) {
        tweets.insert(text);
      }
    }
    ifs.close();
  }

  if (2 == input_type || 3 == input_type) {
    inagist_api::TwitterAPI tapi;
    if (tapi.GetPublicTimeLine(tweets) < 0) {
      std::cout << "Error: could not get trending tweets from inagist\n";
      return -1;
    }
  }

  if (tweets.empty()) {
    std::cout << "No tweets found on input\n";
    return 0;
  }

  tweets.clear();
  cm.Clear();

  return 0;
}

