#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cstdlib>
#include <cstring>
#include "config_reader.h"

//#define TEST_DEBUG 1

int clean_lang_model(std::string& lang_model_file_name) {

  std::ifstream ifs(lang_model_file_name.c_str());
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open file " << lang_model_file_name << std::endl;
    return -1;
  }

  std::map<std::string, std::string> lang_corpus;
  std::string line;
  std::string key;
  std::string value;
  std::string::size_type loc;
  char* ptr = NULL;
  bool all_caps = true;
  char text[1024];
  memset(text, '\0', 1024);
  int count = 0;
  while (getline(ifs, line)) {
    //std::cout << line << std::endl;
    if ((loc = line.find("=")) != std::string::npos) {
      key.assign(line.c_str(), loc);
      value.assign(line, loc+1, line.length()-loc-1);

      all_caps = true;
      strcpy(text, key.c_str());
      ptr = text;
      while (ptr != NULL && *ptr != '\0') {
        if (*ptr > 96 && *ptr < 123) {
          all_caps = false;
        }
        ptr++;
      }
      if (!all_caps) {
#ifdef TEST_DEBUG
        if (count < 10) {
          std::cout << key << ", " << value << std::endl;
        }
#endif
        lang_corpus.insert(std::pair<std::string, std::string> (key, value));
        count++;
      } else {
        std::cout << key << ", " << value << " is invalid. ignoring." << std::endl;
      }
    }
  }
  ifs.close();

  if (count <= 0) {
    std::cout << "ERROR: empty corpus\n";
    return -1;
  }

  std::string new_file_name = lang_model_file_name + ".new";
  std::ofstream ofs(new_file_name.c_str());
  if (ofs) {
    std::map<std::string, std::string>::iterator map_iter;
    for (map_iter = lang_corpus.begin(); map_iter != lang_corpus.end(); map_iter++) {
      ofs << map_iter->first << "=" << map_iter->second << std::endl;
    }
  }
  ofs.close();

  return 0;
}

int main(int argc, char* argv[]) {

  if (argc != 3) {
    std::cout << argv[0] << " <0/1, 0-lang, 1-config> <lang_model_file_name/config_file_name>\n";
    return -1;
  }

  int input_type = atoi(argv[1]);
  if (input_type != 0 && input_type != 1) {
    std::cout << "ERROR: invalid input type\n";
    return -1;
  }

  if (0 == input_type) {
    std::string lang_model_file_name = std::string(argv[2]);
    if (lang_model_file_name.find(".config") != std::string::npos) {
      std::cout << "WARNING: config file given as input for corpus file? " << lang_model_file_name << ". will be wiped out. check and rename!" << std::endl;
      return -1;
    }
    return clean_lang_model(lang_model_file_name);
  }

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified

  std::string config_file_name = std::string(argv[2]);

  inagist_classifiers::Config config;
  if (inagist_classifiers::ConfigReader::Read(config_file_name.c_str(), config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
    return -1;
  }

  if (config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " << config_file_name << std::endl;
    return -1;
  }

  std::string lang_model;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    lang_model = config.iter->corpus_file;
    if (clean_lang_model(lang_model) < 0) {
      std::cout << "ERROR: could not clean lang model in file: " \
                << config.iter->corpus_file << std::endl; 
      break;
    }
  }
  inagist_classifiers::ConfigReader::Clear(config);

  return 0;

}
