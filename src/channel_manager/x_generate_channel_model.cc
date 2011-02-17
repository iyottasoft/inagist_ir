#include "channel_manager.h"
#include <iostream>
#include <fstream>
#include <cstdlib>

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <config_file_name>\n";
    return -1;
  }

  std::string config_file_name = argv[1];
  inagist_classifiers::ChannelManager cm;

  std::ifstream ifs(config_file_name.c_str());
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open config file " << config_file_name << std::endl;
    return -1;
  } else {
    std::string line;
    std::string key;
    std::string value;
    std::string::size_type loc;
    int line_count = 0;
    std::string channel;
    std::string handles_file_name;
    std::string output_tweets_file_name;
    std::string output_corpus_file_name;
    while (getline(ifs, line)) {
      line_count++;
      //std::cout << line << std::endl;
      loc = line.find("=", 0);
      if (loc == std::string::npos) {
        std::cout << "ERROR: invalid config file entry\n";
        break;
      }
      key.assign(line.c_str(), loc);
      value.assign(line.c_str(), loc+1, (line.length()-loc-1));
      //std::cout << key << std::endl;
      //std::cout << value << std::endl;
      if (key.compare(0, 4, "channel") == 0) {
        channel = value;
      } else if (key.compare(0, 7, "handles") == 0) {
        handles_file_name = value;
      } else if (key.compare(0, 6, "corpus") == 0) {
        output_corpus_file_name = value;
      } else if (key.compare(0, 6, "tweets") == 0) {
        output_tweets_file_name = value;
      }
      if (line_count == 4) {
        //int count = 0;
        line_count = 0;
      }
    }
    ifs.close();
  }

  cm.Clear();

  return 0;
}

