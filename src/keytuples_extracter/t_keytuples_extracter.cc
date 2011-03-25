#include "keytuples_extracter.h"
#include <cstring>
#include <string>
#include <set>

using std::string;

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <-i | text within double quotes>" << std::endl;
    return -1;
  }

  inagist_trends::KeyTuplesExtracter ke;

  string arguments(argv[0]);
  string::size_type loc = arguments.find("bin", 0);
  string root_dir;
  if (loc != string::npos) {
    loc-=1;
    root_dir.assign(argv[0], loc);
  }

  string stopwords_file = root_dir + "/data/static_data/stopwords.txt";
  string dictionary_file = root_dir + "/data/static_data/dictionary.txt";
  string unsafe_dictionary_file = root_dir + "/data/static_data/unsafe_dictionary.txt";
  string lang_detect_config_file = root_dir + "/configs/language_detection.config";
  if (ke.Init(stopwords_file.c_str(), dictionary_file.c_str(), unsafe_dictionary_file.c_str(), lang_detect_config_file.c_str()) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
    return -1;
  }

  std::set<std::string> keywords_set;
  std::set<std::string> hashtags_set;
  std::set<std::string> keyphrases_set;
  std::string safe_status;
  std::string script;
  char str[141];
  int ret_value = 0;
  if (strcmp(argv[1], "-i") == 0) {
    std::string s;
    while (getline(std::cin, s)) {
      if (s.compare("exit") == 0)
        break;
      strcpy(str, s.c_str()); 
      ret_value = ke.GetKeyTuples(str, safe_status, script, keywords_set, hashtags_set, keyphrases_set);
      if (ret_value < 0) {
        std::cout << "ERROR: could not get keywords\n";
      } else if (ret_value == 0) {
        std::cout << "No keywords found\n";
      } else {
        std::cout << "Safe Status: " << safe_status << std::endl;
        std::cout << "Script: " << script << std::endl;
        std::cout << "Keywords:" << std::endl;
        ke.PrintKeywords(keywords_set);
        keywords_set.clear();
        if (keyphrases_set.size() > 0) {
          std::cout << "Keyphrases:" << std::endl;
          ke.PrintKeywords(keyphrases_set);
          keyphrases_set.clear();
        }
        if (hashtags_set.size() > 0) {
          std::cout << "hashtags:" << std::endl;
          ke.PrintKeywords(hashtags_set);
          hashtags_set.clear();
        }
      }
    }
  } else {
    ke.GetKeyTuples(argv[1], safe_status, script, keywords_set, hashtags_set, keyphrases_set);
    std::cout << script << std::endl;
    ke.PrintKeywords(keywords_set);
    keywords_set.clear();
    if (keyphrases_set.size() > 0) {
      std::cout << "Keyphrases:" << std::endl;
      ke.PrintKeywords(keyphrases_set);
      keyphrases_set.clear();
    }
  }

  ke.DeInit();

  return 0;
}
