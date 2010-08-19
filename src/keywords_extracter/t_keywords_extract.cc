#include "keywords_extract.h"
#include <cstring>
#include <string>
#include <set>

int main(int argc, char* argv[]) {

  if (argc < 2 && argc > 6) {
    //std::cerr << "Usage: " << argv[0] << " <input_file> [<stopwords_file>] [<dictionary_file>] [<stemmer_dict_file>] [<output_file>]\n";
    return -1;
  }

  inagist_trends::KeywordsExtract ke;

  //if (ke.Init(NULL) < 0)
  //  std::cout << "passed empty input test\n";

  if (ke.Init("./data/tweets.txt") < 0)
    std::cerr << "ERROR: didn't use default parameters\n";

  ke.DeInit();

  if (ke.Init("./data/tweets.txt", "./data/static_data/stopwords.txt", "./data/static_data/dictionary.txt", NULL, "./data/output.txt") < 0) {
    std::cerr << "ERROR: couldn't initialize\n";
    return -1;
  }

  std::set<std::string> keywords_set;
  char str[141];
  if (argc == 2) {
    std::string s;
    while (getline(std::cin, s)) {
      strcpy(str, s.c_str()); 
      ke.GetKeywords(str, keywords_set);
      ke.PrintKeywords(keywords_set);
      keywords_set.clear();
    }
  } else {
    strcpy(str, "At Pragati, we have started using the new rupee symbol in our digital and print editions. We might be the first Indian magazine to do so.");
    ke.GetKeywords(str, keywords_set);
    ke.PrintKeywords(keywords_set);
    keywords_set.clear();
  }
  /*
  std::vector<std::string> keywords;

  ke.GetKeywords(keywords);
  ke.PrintKeywords(keywords);
  */

  ke.DeInit();

  return 0;
}
