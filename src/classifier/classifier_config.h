#ifndef _INAGIST_CLASSIFIERS_CLASSIFIER_CONFIG_H_
#define _INAGIST_CLASSIFIERS_CLASSIFIER_CONFIG_H_

#include <string>
#include <set>
#include <map>
#include <iostream>

namespace inagist_classifiers {

typedef struct _class_struct { 
  std::string name;
  std::string number;
  std::string label;
  std::string class_data_file;
  // testing
  std::string testing_data_file;
  std::string testing_handles_file;
  std::string testing_corpus_file;
  std::string testing_tweets_file; 
  // training
  std::string training_data_file;
  std::string training_handles_file;
  std::string training_corpus_file;
  std::string training_tweets_file; 
  std::string seed_file;
  friend bool operator<(_class_struct const& a, _class_struct const& b) {
    return a.name.compare(b.name) < 0;
  }
  void Clear() {
    name.clear();
    number.clear();
    label.clear();
    class_data_file.clear();
    testing_data_file.clear();
    testing_handles_file.clear();
    testing_corpus_file.clear();
    testing_tweets_file.clear();
    training_data_file.clear();
    training_handles_file.clear();
    training_corpus_file.clear();
    training_tweets_file.clear();
    seed_file.clear();
  }
  void Print() const {
    std::cout << "name: " << name << std::endl;
    std::cout << "number: " << number << std::endl;
    std::cout << "label: " << label << std::endl;
    std::cout << "class_data: " << class_data_file << std::endl;
    std::cout << "testing_data: " << testing_data_file << std::endl;
    std::cout << "testing_handles: " << testing_handles_file << std::endl;
    std::cout << "testing_corpus: " << testing_corpus_file << std::endl;
    std::cout << "testing_tweets: " << testing_tweets_file << std::endl; 
    std::cout << "training_data: " << training_data_file << std::endl;
    std::cout << "training_handles: " << training_handles_file << std::endl;
    std::cout << "training_corpus: " << training_corpus_file << std::endl;
    std::cout << "training_tweets: " << training_tweets_file << std::endl; 
    std::cout << "seed: " << seed_file << std::endl;
  }
} ClassStruct;

typedef struct _config_struct {
  std::string class_freqs_file; 
  std::string test_freqs_file; // this is same data as class_freqs_file except this file is updated repeatedly
  unsigned int num_classes;
  std::set<ClassStruct> classes;
  std::set<ClassStruct>::iterator iter;
} Config;

class ClassifierConfig {
 public:
  // functions
  ClassifierConfig();
  ~ClassifierConfig();
  static int Read(const char* config_file_name, Config& config);
  static int LoadClassLabelsMap(Config& config, std::map<std::string, std::string>& class_labels_map);
  static int LoadClassNumbersMap(Config& config, std::map<std::string, std::string>& class_numbers_map);
  static int Clear(Config& config);
  static int Write(Config& config, const char* config_file_name);
};

} // namespace inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_CLASSIFIER_CONFIG_H_
