#ifndef _INAGIST_CLASSIFIERS_CLASSIFIER_CONFIG_H_
#define _INAGIST_CLASSIFIERS_CLASSIFIER_CONFIG_H_

#include <string>
#include <set>
#include <map>
#include <iostream>

namespace inagist_classifiers {

typedef struct _class_struct { 
  std::string name;
  std::string label;
  std::string class_data_file;
  // testing
  std::string testing_data_file;
  // training
  std::string training_data_file;
  std::string handles_file;
  std::string corpus_file;
  std::string tweets_file; 
  std::string seed_file;
  friend bool operator<(_class_struct const& a, _class_struct const& b) {
    return a.name.compare(b.name) < 0;
  }
  void clear() {
    name.clear();
    label.clear();
    class_data_file.clear();
    testing_data_file.clear();
    training_data_file.clear();
    handles_file.clear();
    corpus_file.clear();
    tweets_file.clear();
    seed_file.clear();
  }
  void print() const {
    std::cout << "name: " << name << std::endl;
    std::cout << "label: " << std::endl;
    std::cout << "class_data: " << class_data_file << std::endl;
    std::cout << "testing_data: " << testing_data_file << std::endl;
    std::cout << "training_data: " << training_data_file << std::endl;
    std::cout << "handles: " << handles_file << std::endl;
    std::cout << "corpus: " << corpus_file << std::endl;
    std::cout << "tweets: " << tweets_file << std::endl; 
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
  static int Clear(Config& config);
  static int Write(Config& config, const char* config_file_name);
};

} // namespace inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_CLASSIFIER_CONFIG_H_
