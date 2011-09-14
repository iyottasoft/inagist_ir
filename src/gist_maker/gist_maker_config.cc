#include "gist_maker_config.h"
#include <iostream>
#include <fstream>

namespace inagist_trends {

GistMakerConfig::GistMakerConfig() {
}

GistMakerConfig::~GistMakerConfig() {
}

int GistMakerConfig::Clear(Config& config) {
  config.stopwords_file.clear();
  config.dictionary_file.clear();
  config.unsafe_dictionary_file.clear();
#ifdef LANG_ENABLED
  config.language_dictionary_file.clear();
  config.language_prior_freqs_file.clear();
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  config.classifier_dictionary_file.clear();
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
  config.intent_words_file.clear();
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  config.sentiment_words_file.clear();
#endif // SENTIMENT_ENABLED
  config.stemmer_dictionary_file.clear();
  return 0;
}

int GistMakerConfig::Read(const char* config_file_name, Config& config) {

  if (!config_file_name) {
    std::cerr << "ERROR: invalid input. can't read config\n";
    return -1;
  }

  std::ifstream ifs(config_file_name);
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open config file " << config_file_name << std::endl;
    return -1;
  } else {
    std::string line;
    std::string key;
    std::string value;
    std::string::size_type loc;
    while (getline(ifs, line)) {
      //std::cout << line << std::endl;
      if (line.length() < 1)
        continue;
      loc = line.find("=", 0);
      if (loc == std::string::npos) {
        std::cout << "ERROR: invalid config file entry in line: " << line << std::endl;
        break;
      }
      key.assign(line.c_str(), loc);
      value.assign(line.c_str(), loc+1, (line.length()-loc-1));
      //std::cout << key << std::endl;
      //std::cout << value << std::endl;
      if (key.compare(0, 9, "stopwords") == 0) {
        config.stopwords_file = value;
      } else if (key.compare(0, 10, "dictionary") == 0) {
        config.dictionary_file = value;
      } else if (key.compare(0, 17, "unsafe_dictionary") == 0) {
        config.unsafe_dictionary_file = value;
#ifdef LANG_ENABLED
      } else if (key.compare(0, 19, "language_dictionary") == 0) {
        config.language_dictionary_file = value;
      } else if (key.compare(0, 20, "language_prior_freqs") == 0) {
        config.language_prior_freqs_file = value;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      } else if (key.compare(0, 21, "classifier_dictionary") == 0) {
        config.classifier_dictionary_file = value;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
      } else if (key.compare(0, 12, "intent_words") == 0) {
        config.intent_words_file = value;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
      } else if (key.compare(0, 15, "sentiment_words") == 0) {
        config.sentiment_words_file = value;
#endif // SENTIMENT_ENABLED
      } else if (key.compare(0, 18, "stemmer_dictionary") == 0) {
        config.stemmer_dictionary_file = value;
      }
    }
    ifs.close();
  }

  return 0;
}

} // namespace inagist_classifiers
