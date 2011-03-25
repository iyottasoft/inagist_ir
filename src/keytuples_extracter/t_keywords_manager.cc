#include <iostream>
#include <cstdlib>
#include <cstring>
#include "keytuples_extracter.h"
#include "keywords_manager.h"

int main(int argc, char *argv[]) {

  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <count>\n";
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

  std::string data_dir = root_dir + "data/";
  std::string stopwords_file = data_dir + "static_data/stopwords.txt";
  std::string dictionary_file = data_dir + "static_data/dictionary.txt";
  std::string unsafe_dictionary_file = data_dir + "static_data/unsafe_dictionary.txt";
  std::string lang_detect_config_file = root_dir + "configs/language_detection.config";
  std::string channels_dictionary_file = root_dir + "static_data/channels_dictionary.txt";
  std::string input_file = data_dir + "tweets.txt";
  std::string output_file = data_dir + "static_data/output.txt";

  inagist_trends::KeyTuplesExtracter ke;
  if (ke.Init(stopwords_file.c_str(),
              dictionary_file.c_str(),
              unsafe_dictionary_file.c_str(),
              lang_detect_config_file.c_str(),
              channels_dictionary_file.c_str(),
              NULL,
              input_file.c_str(),
              output_file.c_str()) < 0) {
    std::cerr << "ERROR: couldn't initialize KeyTuplesExtracter\n";
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
