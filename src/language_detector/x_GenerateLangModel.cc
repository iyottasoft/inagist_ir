#include "language_detector.h"
#include <iostream>
#include <cstdlib>

int main(int argc, char* argv[]) {

  if (argc != 4 && argc !=5) {
    std::cout << "Usage: " << argv[0] << " <input_file_name> " \
              << "<0/1> 0-from tweet handles/1-text> "
              << "<output_corpus_file_name> "
              << "<output_tweets_file_name>\n";
    return -1;
  }

  std::string input_file_name = std::string(argv[1]);

  int input_type = atoi(argv[2]);
  if (input_type != 0 && input_type != 1) {
    std::cout << "ERROR: invalid input type <0/1, 0-from tweet handles/1-text>" << input_type << std::endl;
    return -1;
  }

  std::string output_file_name;
  output_file_name = std::string(argv[3]);

  // see if the output file can be opened
  inagist_classifiers::LanguageDetector ld;

  int count = 0;
  if (0 == input_type) { 
    std::string output_tweets_file_name = argv[4];
    if ((count = ld.GenerateLangModelFromTweets(input_file_name, output_tweets_file_name, output_file_name)) < 0) {
      std::cout << "ERROR: could not generate lang model for handles in file " << input_file_name << std::endl; 
    } else {
      std::cout << "Corpus of size " << count << " generated\n";
    }
  } else if (1 == input_type) {
    if ((count = ld.GenerateLangModel(input_file_name, output_file_name)) < 0) {
      std::cout << "ERROR: could not get language model for text in file " << input_file_name << std::endl;
    } else {
      std::cout << "Corpus of size " << count << " generated\n";
    }
  }
  ld.Clear();

  return 0;
}

