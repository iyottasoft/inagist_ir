#include "language_detector.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cassert>
#include <set>
#include "twitter_api.h"
#include "twitter_searcher.h"

int main(int argc, char* argv[]) {

  if (argc < 4 || argc > 5) {
    std::cout << "Usage: " << argv[0] << " \n\t<config_file_name> \n\t<0/1/2, 0-interactive/1-file/2-tweets> [<debug_level>] \n\t[<input_file_name>/[handle]]\n";
    return -1;
  }

  std::string corpus_config_file_name = std::string(argv[1]);

  inagist_classifiers::LanguageDetector ld;
  if (ld.Init(corpus_config_file_name) < 0) {
    std::cout << "ERROR: could not initialize language detector\n";
    return -1;
  }

  int test_type = atoi(argv[2]);
  assert((test_type >= 0 && test_type <= 2));

  unsigned int debug_level = 0;
  if (argc >= 4) {
    debug_level = atoi(argv[3]);
  }
  ld.SetDebugLevel(debug_level);

  std::string line;
  std::string lang;
  if (0 == test_type) {
    while (getline(std::cin, line)) {
      if (line.compare("exit") == 0 || line.compare("quit") == 0) {
        break;
      }
      if (ld.DetectLanguage(line, line.length(), lang) < 0) {
        std::cout << "ERROR: could not find language\n";
      } else {
        std::cout << lang << std::endl;
      }
    }
    return 0;
  }

  std::set<std::string> lines;
  int num_docs = 0;

  if (1 == test_type) {
    if (argc != 5) {
      std::cout << "ERROR: input file needed\n";
      return -1;
    } else {
      std::string input_file_name = std::string(argv[4]);
      std::ifstream ifs(input_file_name.c_str());
      if (!ifs.is_open()) {
        std::cout << "ERROR: could not open input file " << input_file_name << std::endl;
        return -1;
      } else {
        while (getline(ifs, line)) {
         lines.insert(line);
         num_docs++;
        }
        ifs.close();
      }

      if (num_docs < 1) {
        std::cout << "ERROR: empty input file " << input_file_name << std::endl;
        return -1;
      }
    }
  }

  if (2 == test_type) {
    if (3 == argc || 4 == argc) { 
        inagist_api::TwitterAPI twitter_api;
        num_docs = twitter_api.GetPublicTimeLine(lines);
    } else if (5 == argc) {
        inagist_api::TwitterSearcher twitter_searcher;
        num_docs = twitter_searcher.GetTweetsFromUser(std::string(argv[4]), lines);
    }

    if (num_docs < 1) {
      std::cout << "ERROR: no input tweets\n";
      return -1;
    }
  }

  std::set<std::string>::iterator set_iter;
  for (set_iter = lines.begin(); set_iter != lines.end(); set_iter++) {
    line = *set_iter;
    std::cout << line << std::endl;
    if (ld.DetectLanguage(line, line.length(), lang) < 0) {
      std::cout << "ERROR: could not find language\n";
    } else {
#ifdef LD_DEBUG
      if (LD_DEBUG > 0) {
        std::cout << line << std::endl;
      }
#endif
      std::cout << lang << std::endl;
      if (ld.DetectLanguage(line, line.length(), lang, true) < 0) {
        std::cout << "ERROR: could not find language after ToLower\n";
      }
      std::cout << "ignoring case: " << lang << std::endl;
    }
    break;
  }

  lines.clear();

  return 0;
}

