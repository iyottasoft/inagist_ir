#include "keywords_extract.h"
#include <cstring>
#include <string>
#include <set>

using std::string;

int main(int argc, char* argv[]) {

  if (argc > 2) {
    std::cout << "Usage: " << argv[0] << " <text within double quotes>" << std::endl;
    return -1;
  }

  inagist_trends::KeywordsExtract ke;

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
  if (ke.Init(stopwords_file.c_str(), dictionary_file.c_str(), unsafe_dictionary_file.c_str()) < 0) {
    std::cerr << "ERROR: couldn't initialize KeywordsExtract\n";
    return -1;
  }

  std::set<std::string> keywords_set;
  std::set<std::string> keyphrases_set;
  std::string safe_status;
  std::string script;
  char str[141];
  if (argc == 1) {
    std::string s;
    while (getline(std::cin, s)) {
      if (s.compare("exit") == 0)
        break;
      strcpy(str, s.c_str()); 
      ke.GetKeywords(str, safe_status, script, keywords_set, keyphrases_set);
      std::cout << "Safe Status: " << script << std::endl;
      std::cout << "Script: " << script << std::endl;
      std::cout << "Keywords:" << std::endl;
      ke.PrintKeywords(keywords_set);
      keywords_set.clear();
      if (keyphrases_set.size() > 0) {
        std::cout << "Keyphrases:" << std::endl;
        ke.PrintKeywords(keyphrases_set);
        keyphrases_set.clear();
      }
    }
  } else {
    ke.GetKeywords(argv[1], safe_status, script, keywords_set, keyphrases_set);
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
