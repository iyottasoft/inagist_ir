#include "language_detector.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cassert>
#include <set>
#include "twitter_api.h"
#include "twitter_searcher.h"

using namespace inagist_classifiers;

int main(int argc, char* argv[]) {

  if (argc < 3 || argc > 5) {
    std::cout << "Usage: " << argv[0] << " \n\t<config_file_name> \n\t<0/1/2, 0-interactive/1-file/2-tweet/3-many tweets> \n\t[<debug_level>] \n\t[<input_file_name>/[handle]]\n";
    return -1;
  }

  std::string corpus_config_file_name = std::string(argv[1]);

  int input_type = atoi(argv[2]);
  assert((input_type >= 0 && input_type <= 3));

  unsigned int debug_level = 0;
  if (argc >= 4) {
    debug_level = atoi(argv[3]);
  }

  std::string line;
  std::string lang;
  std::string top_classes;
  unsigned int top_classes_count = 0;

  std::set<std::string> lines;
  int num_docs = 0;

  if (1 == input_type) {
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

  if (2 == input_type || 3 == input_type) {
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

  LanguageDetector* ld = (LanguageDetector*) new LanguageDetector();
  if (ld->Init(corpus_config_file_name) < 0) {
    std::cout << "ERROR: could not initialize language detector\n";
    return -1;
  }
  ld->SetDebugLevel(debug_level);

#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED
  if (0 == input_type) {
    while (getline(std::cin, line)) {
      if (line.compare("exit") == 0 || line.compare("quit") == 0) {
        break;
      }
      if (ld->Classify(line, line.length(), lang,
                       top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                       , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                      ) < 0) {
        std::cout << "ERROR: could not find language\n";
      } else {
        std::cout << lang << std::endl;
      }
    }
    delete ld;
    return 0;
  }

  std::set<std::string>::iterator set_iter;
  for (set_iter = lines.begin(); set_iter != lines.end(); set_iter++) {
    line = *set_iter;
    std::cout << line << std::endl;
    if (ld->Classify(line, line.length(), lang,
                     top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                     , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                    ) < 0) {
      std::cout << "ERROR: could not find language\n";
    } else {
#ifdef LD_DEBUG
      if (LD_DEBUG > 0) {
        std::cout << line << std::endl;
      }
#endif
      std::cout << lang << std::endl;
      if (ld->Classify(line, line.length(), lang,
                       top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                       , class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                       , true) < 0) {
        std::cout << "ERROR: could not find language after ToLower\n";
      }
      std::cout << "ignoring case: " << lang << std::endl;
    }
    if (2 == input_type)
      break;
  }

  lines.clear();

  delete ld;

  return 0;
}

