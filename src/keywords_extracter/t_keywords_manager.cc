#include <iostream>
#include <cstring>
#include "keywords_extract.h"
#include "keywords_manager.h"

int main(int argc, char *argv[]) {

  inagist_trends::KeywordsExtract ke;
  if (ke.Init("./data/static_data/stopwords.txt", "./data/static_data/dictionary.txt", NULL, "./data/tweets.txt", "./data/output.txt") < 0) {
    std::cerr << "ERROR: couldn't initialize\n";
    return -1;
  }
  inagist_trends::KeywordsManager km;

  std::string line;
  std::set<std::string> keywords_set;
  char input[255];
  
  unsigned int num_docs = 0;
  unsigned int count = 1;
  if (argc > 1)
    count = atoi(argv[1]);

  while (num_docs < count && getline(std::cin, line)) {
    memset(input, 0, 255);
    strcpy(input, (char *) line.c_str());
    ke.GetKeywords(input, keywords_set);
    ke.PrintKeywords(keywords_set);
    km.PopulateFreqMap(keywords_set);
    ++num_docs;
    km.PrintFreqMap();
    keywords_set.clear();
  }
  km.CalculateIDF(num_docs);
  km.PrintEntityIDFs();

  return 0;
}
