#include "corpus_manager.h"
#include <iostream>
#include <cstdlib>

int main(int argc, char* argv[]) {

/*
  if (argc != 3 && argc != 5) {
    std::cout << "Usage: " << argv[0] << " <input_file_name> <0/1, 0-create/1-read> [<0/1> 0-create from tweet handles/1-text] [output_file_name]\n";
    return -1;
  }
*/

  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input_file_name> <0/1, 0-read corpus/1-read corpus map>\n";
    return -1;
  }

  std::string input_file_name = std::string(argv[1]);
  // create corpus or read from supplied corpus
  int test_type = atoi(argv[2]);
  if (test_type != 0 && test_type != 1) {
    std::cout << "ERROR: invalid test type " << test_type << std::endl;
    return -1;
  }

  inagist_classifiers::CorpusManager cm;
  if (0 == test_type) {
    if (cm.InitRead(input_file_name) < 0) {
      std::cout << "ERROR: could not initialize corpus manager to read " << input_file_name << std::endl;
      return -1;
    }
    std::string line;
    int freq = 0;
    while (getline(std::cin, line)) {
      if (line.compare("exit") == 0)
        break;
      if ((freq = cm.LookUp(line)) < 0)
        std::cout << "ERROR: could not get frequency for " << line << std::endl;
      if (freq == 0)
        std::cout << line << " not found\n";
      else
        std::cout << line << " = " << freq << std::endl;
    }
    cm.Clear();
    return 0;
  }

/*
  if (0 == test_type) {
    int file_type = 0;
    std::string output_file_name;
    if (argc == 5) {
      file_type = atoi(argv[3]);
      if (file_type != 0 && file_type != 1) {
        std::cout << "ERROR: invalid file type. 0 - tweet handles file, 1 - text file\n";
        return -1;
      }
      output_file_name = std::string(argv[4]);
    }
    if (0 == file_type) { 
      if (cm.GenerateLangModelFromTweets(input_file_name, "/tmp/tweets_file.txt", output_file_name) < 0) {
        std::cout << "ERROR: could not generate lang model for handles in file " << input_file_name << std::endl; 
        return -1;
      }
    } else {
      if (cm.GenerateLangModel(input_file_name, output_file_name) < 0) {
        std::cout << "ERROR: could not get language model for text in file " << input_file_name << std::endl;
        return -1;
      }
    }
  }
*/
  return 0;
}

