#include <iostream>
#include <cstdlib>
#include <cstring>
#include "keytuples_extracter.h"
#include "named_entities_manager.h"

int main(int argc, char *argv[]) {

  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <count>\n";
    return -1;
  }

#ifndef NAMED_ENTITIES_ENABLED
    std::cerr << "named_entities not enabled. nothing to be done.\n";
    return -1;
#endif // NAMED_ENTITIES_ENABLED

  std::string bin_location = std::string(argv[0]);
  std::string::size_type loc = bin_location.find("bin", 0);
  std::string root_dir;
  if (loc == std::string::npos) {
    std::cerr << "ERROR: could not find bin location\n" << std::endl;
    return -1;
  } else {
    root_dir = std::string(bin_location, 0, loc);
  }

  std::string data_dir = root_dir + "data/";
  std::string stopwords_file = data_dir + "static_data/stopwords.txt";
  std::string dictionary_file = data_dir + "static_data/dictionary.txt";
  std::string unsafe_dictionary_file = data_dir + "static_data/unsafe_dictionary.txt";
  std::string input_file = data_dir + "tweets.txt";
  std::string output_file = data_dir + "static_data/output.txt";

  inagist_trends::KeyTuplesExtracter ke;
  if (ke.Init(stopwords_file.c_str(),
              dictionary_file.c_str(),
              unsafe_dictionary_file.c_str(),
              NULL,
              input_file.c_str(),
              output_file.c_str()) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
    return -1; 
  }

  inagist_trends::KeywordsManager km;

  std::string line;
  std::set<std::string> named_entities_set;
  std::set<std::string>::iterator set_iter;
  char input[255];

  unsigned int num_docs = 0;
  unsigned int count = 1;
  if (argc > 1)
    count = atoi(argv[1]);

  while (num_docs < count && getline(std::cin, line)) {
    memset(input, 0, 255);
    strcpy(input, (char *) line.c_str());
#ifdef NAMED_ENTITIES_ENABLED
    ke.GetKeyTuples(input, named_entities_set);
#endif // NAMED_ENTITIES_ENABLED
    for (set_iter = named_entities_set.begin(); set_iter != named_entities_set.end(); set_iter++) {
      std::cout << *set_iter << std::endl;
    }
    km.PopulateFreqMap(named_entities_set);
    ++num_docs;
    km.PrintFreqMap();
    named_entities_set.clear();
  }
  km.CalculateIDF(num_docs);
  km.PrintEntityIDFs();

  return 0;
}
