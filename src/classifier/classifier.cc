#include "classifier.h"
#include <iostream>
#include <cstdlib>
#include "twitter_searcher.h"
#include "twitter_api.h"

#ifdef DEBUG
#if DEBUG>0
#define CLASSIFIER_DEBUG DEBUG
#endif
#endif
//#define CLASSIFIER_DEBUG 3

namespace inagist_classifiers {

Classifier::Classifier() {
}

Classifier::~Classifier() {

  if (ConfigReader::Clear(m_config) < 0) {
    std::cerr << "ERROR: could not clear config\n";
  }

}

int Classifier::Init(std::string config_file_name) {

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified

  if (ConfigReader::Read(config_file_name.c_str(), m_config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
    return -1;
  }

  if (m_config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " \
              << config_file_name << std::endl;
    return -1;
  }

  std::map<std::string, std::string> class_name_file_map;
  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {
    class_name_file_map[m_config.iter->name] = m_config.iter->training_data_file;
  }
  if (class_name_file_map.empty()) {
    std::cerr << "ERROR: class_name_file_map cannot be empty\n";
    return -1;
  }

  if (m_corpus_manager.LoadCorpus(m_config.test_data_file,
                                  m_corpus_manager.m_classes_freq_map) < 0) {
    std::cerr << "ERROR: could not load the text classes freq file (test data)\n";
    std::cout << "WARNING: continuing without the text classes freq data\n";
  }

  if (m_corpus_manager.LoadCorpusMap(class_name_file_map) < 0) {
    std::cerr << "ERROR: could not load Corpus Map\n";
    return -1;
  }

  return 0;
}

// this functioin uses round robin to get training data.
// TODO (balaji) - handle rate limiting and get data from all sources
int Classifier::GetTrainingData(const char* config_file_name) {

  if (!config_file_name) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  Config config;
  if (ConfigReader::Read(config_file_name, config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
    return -1;
  }

  if (config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " << config_file_name << std::endl;
    return -1;
  }

  int i = rand() % config.classes.size();
  int j = 0;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    if (j == i)
      break;
    j++;
  }

  int count = 0;
  int count_temp = 0;
  for (; config.iter != config.classes.end(); config.iter++) {
    if ((count_temp = GetTrainingData(config.iter->handles_file,
                                 config.iter->tweets_file,
                                 config.iter->corpus_file)) < 0) {
      std::cerr << "ERROR: could not get training data for handles in file: " \
                << config.iter->handles_file << std::endl; 
    } else {
      std::cout << "Corpus of size " << count_temp << " generated for " << config.iter->name << std::endl;
      count += count_temp;
    }
  }
  j = 0;
  for (config.iter = config.classes.begin(); config.iter != config.classes.end() && j<i; config.iter++) {
    j++;
    if ((count_temp = GetTrainingData(config.iter->handles_file,
                                 config.iter->tweets_file,
                                 config.iter->corpus_file)) < 0) {
      std::cerr << "ERROR: could not get training data for handles in file: " \
                << config.iter->handles_file << std::endl; 
    } else {
      std::cout << "Corpus of size " << count_temp << " generated for " << config.iter->name << std::endl;
      count += count_temp;
    }
  }

  ConfigReader::Clear(config);

  return count;
}

int Classifier::GetTrainingData(const std::string& handle, Corpus& corpus, bool get_user_info) {

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  unsigned int count = 0;
  unsigned int count_temp = 0;

  if (get_user_info) {
    std::string user_info;
    if (inagist_api::TwitterAPI::GetUserInfo(handle, user_info) < 0) {
      std::cerr << "ERROR: could not get user info for handle: " << handle << std::endl;
    } else {
      if (user_info.length() > 0 && (count_temp = GetCorpus(user_info, corpus)) < 0) {
        std::cerr << "ERROR: could not find ngrams for user info string: " << user_info << std::endl;
      } else {
        count += count_temp;
      }
    }
  }

  if (twitter_searcher.GetTweetsFromUser(handle, tweets) > 0) {
    std::set<std::string>::iterator set_iter;
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      // this GetCorpus is a pure virtual function
      // ensure your derivation of this classifier provides this function
      if ((count_temp = GetCorpus(*set_iter, corpus)) < 0) {
        std::cerr << "ERROR: could not find ngrams from tweet: " << *set_iter << std::endl;
      } else {
        count += count_temp;
      }
    }
  }
  tweets.clear();

  return count;
}

int Classifier::GetTrainingData(const std::string& twitter_handles_file_name,
                                const std::string& output_tweets_file_name,
                                const std::string& output_corpus_file_name) {

  std::ifstream ifs(twitter_handles_file_name.c_str());
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open file " << twitter_handles_file_name << std::endl;
    return -1;
  }

  std::string line;
  std::string handle;
  std::string::size_type loc;
  unsigned int flag = 0;
  unsigned int exe_count = 0;
  std::string exe_count_str;
  std::map<std::string, unsigned int> handles;
  std::map<std::string, unsigned int>::iterator handle_iter;

  while(getline(ifs, line)) {
    if (line.length() <= 1) {
      continue;
    }
    loc = line.find("=", 0);
    if (loc == std::string::npos) {
      handle.assign(line);
      exe_count = 0;
    } else {
      handle.assign(line, 0, loc);
      exe_count_str.assign(line, loc+1, line.length()-loc-1);
      exe_count = atoi((char *) exe_count_str.c_str());
    }
    handles[handle] = exe_count;
  }
  ifs.close();

  if (handles.size() < 1) {
    std::cerr << "ERROR: no handles found in file " << twitter_handles_file_name << std::endl;
    return 0;
  }

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  unsigned int count = 0;
  unsigned int count_temp = 0;
  Corpus corpus;

  // the following code is to randomly pick one handle and get tweets from it
  int i = rand() % handles.size();
  int j = 0;
  for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
    if (i==j)
      break;
    j++;
  }

  // this flag indicates a flip in state and hence needs to be committed to file
  flag = 0;
  bool no_new_handle = true;
  for (; handle_iter != handles.end(); handle_iter++) {
    if (handle_iter->second == 1)
      continue;
    no_new_handle = false;
    handle = handle_iter->first;

    // need user info. 0 indicates its the first time this handles is being processed
    if (handle_iter->second == 0) {
      count_temp = GetTrainingData(handle, corpus, true);
    } else {
      count_temp = GetTrainingData(handle, corpus, false);
    }
    usleep(100000);

    if (count_temp < 0) {
      std::cerr << "ERROR: could not get training data for handle: " << handle << std::endl;
    } else {
      handles[handle] += 1;
      handles[handle] %= 2;
      flag = 1;
    }
    if (0 == count_temp) {
      continue;
    } else {
      count += count_temp;
      break;
    }
  }

  if (0 == flag) {
    j = 0;
    for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
      if (i==j) break; j++;
      if (handle_iter->second == 1)
        continue;
      no_new_handle = false;
      handle = handle_iter->first;

      // need user info
      if (handle_iter->second == 0) {
        count_temp = GetTrainingData(handle, corpus, true);
      } else {
        count_temp = GetTrainingData(handle, corpus, false);
      }
      usleep(100000);

      if (count_temp < 0) {
        std::cerr << "ERROR: could not get training data for handle: " << handle << std::endl;
      } else {
        handles[handle] += 1;
        handles[handle] %= 2;
        flag = 1;
      }
      if (0 == count_temp) {
        continue;
      } else {
        count += count_temp;
        break;
      }
    }
  }

  if (no_new_handle) {
    // all the entries in the handles file may have been examined and hence have "1".
    // lets flip them all to 0.
    for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
      if (handle_iter->second == 1) {
        handle = handle_iter->first;
        handles[handle] = 2;
        flag = 1;
      } else {
        std::cerr << "ERROR: ill-maintained traning data state or unexplained error\n";
        handles.clear();
        return -1;
      }
    }
  }

  if (1 == flag) {
    std::ofstream ofs(twitter_handles_file_name.c_str());
    if (!ofs.is_open()) {
      std::cerr << "ERROR: handles file: " << twitter_handles_file_name << " could not be opened for write.\n";
      return -1;
    } else {
      for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
         ofs << handle_iter->first << "=" << handle_iter->second << std::endl;
      }
      ofs.close();
    }
  }

  handles.clear();

  if (no_new_handle) {
    // recursive call
    return GetTrainingData(twitter_handles_file_name,
                           output_corpus_file_name,
                           output_tweets_file_name);
  }

  if (count == 0) {
    std::cout << "No tweets found for handles in file " << twitter_handles_file_name << std::endl;
    return 0;
  } else {
    if (CorpusManager::UpdateCorpusFile(corpus, output_corpus_file_name) < 0) {
      std::cerr << "ERROR: could not update features to output file " << output_corpus_file_name << std::endl;
    }
  }

  corpus.clear();

  return count;
}

int Classifier::WriteTestData(Corpus& corpus, const char* classes_freq_file) {

  if (corpus.empty() || !classes_freq_file) {
    std::cerr << "ERROR: invalid input. could not write test data\n";
    return -1;
  }

  inagist_classifiers::CorpusIter corpus_iter;
  unsigned int sum = 0;
  for (corpus_iter = corpus.begin(); corpus_iter != corpus.end(); corpus_iter++) {
    if (corpus_iter->first.compare("all_classes") != 0) {
      sum += corpus_iter->second;
    }
  }
  if ((corpus_iter = corpus.find("all_classes")) != corpus.end()) {
    corpus_iter->second = sum;
  } else {
    corpus.insert(std::pair<std::string, unsigned int> ("all_classes", sum));
  }

  if (m_corpus_manager.UpdateCorpusFile(corpus, classes_freq_file) < 0) {
    std::cerr << "ERROR: could not update corpus file\n";
    return -1;
  }

  return corpus.size();
}

// this function assumes that the classifier is already initialized.
// and also assumes that the GetCorpus implementation by the derived class
// will work fine with its dependencies already initalized
//
// input_type:
//          0 - twitter timeline
//          1 - random selection of training sources
//          2 - all training sources (all handles)
// output_type:
//          0 - stdout
//          1 - class frequency file
//          2 - html version
//
// Note: the output file is to write texts that contributed to the class frequenceies.
//
int Classifier::GetTestData(const unsigned int& input_type,
                            const unsigned int& output_type,
                            const char* output_file) {

  if (output_type > 0 && !output_file) {
    std::cerr << "ERROR: invalid output file\n";
    return -1;
  }

  std::ofstream ofs;
  std::ostream* output_stream; // note ostream not ofstream

  if (0 == output_type) {
    output_stream = &std::cout;
  } else {
    ofs.open(output_file);
    if (!ofs.is_open()) {
      std::cerr << "ERROR: could not open output file: " << output_file << std::endl;
      return -1;
    }
    output_stream = &ofs;
  }

  Corpus class_freq_map;
  bool random_selection = false;
  std::string handle;
  std::string expected_lang;
  TestResult test_result;
  test_result.clear();
  switch (input_type) {
    case 0:
      // send empty handle and expected class_name to test public timeline
      if (TestTwitterTimeline(handle, expected_lang, class_freq_map, test_result, *output_stream) < 0) {
        std::cerr << "ERROR: could not test twitter timeline\n";
        ofs.close();
        return -1;
      }
      break;
    case 1:
      if (TestTrainingSources(class_freq_map, test_result, *output_stream, random_selection = true) < 0) {
        std::cerr << "ERROR: could not test training sources at random\n";
        ofs.close();
        return -1;
      }
      break;
    case 2:
      if (TestTrainingSources(class_freq_map, test_result, *output_stream, random_selection = false) < 0) {
        std::cerr << "ERROR: could not test training sources at random\n";
        ofs.close();
        return -1;
      }
      break;
    default:
      break;
  }

  if (class_freq_map.empty()) {
    std::cout << "WARNING: no test data found\n";
    ofs.close();
    return 0;
  }

  int ret_val = 0;
  switch (output_type) {
    case 0:
      if (CorpusManager::PrintCorpus(class_freq_map) < 0) {
        std::cerr << "ERROR: could not print corpus\n";
        ret_val = -1;
      }
      break;
    case 1:
      if (WriteTestData(class_freq_map, m_config.test_data_file.c_str()) < 0) {
        std::cerr << "ERROR: could not write test data to file: " \
                  << m_config.test_data_file << std::endl;
        ret_val = -1;
      }
      break;
    case 2:
      break;
    default:
      break;
  }

  ofs.close();
  class_freq_map.clear();

  std::cout << "Total: " << test_result.total << std::endl;
  std::cout << "Undefined: " << test_result.undefined << std::endl;
  std::cout << "Correct: " << test_result.correct << std::endl;
  std::cout << "Wrong: " << test_result.wrong << std::endl;

  return ret_val;

}

int Classifier::TestTwitterTimeline(const std::string& handle,
                                    const std::string& expected_class_name,
                                    Corpus& class_freq_map,
                                    TestResult& test_result,
                                    std::ostream &output_stream) {

  std::set<std::string> tweets;

  if (handle.empty()) {
    if (inagist_api::TwitterAPI::GetPublicTimeLine(tweets) < 0) {
      std::cout << "ERROR: could not get twitter's public timeline\n";
      return -1;
    }
  } else {
    if (inagist_api::TwitterSearcher::GetTweetsFromUser(handle, tweets) < 0) {
      std::cout << "ERROR: could not search twitter for user handle: " << handle << std::endl;
      return -1;
    }
  }

  if (tweets.empty()) {
    return 0;
  }

  std::set<std::string>::iterator set_iter;
  std::string tweet;
  std::string output_class;
  int ret_val = 0;
#ifdef CLASSIFIER_DEBUG
  if (CLASSIFIER_DEBUG > 2) {
    std::cout << "check corpus map" << std::endl;
    CorpusManager::PrintCorpusMap(m_corpus_manager.m_corpus_map);
  }
#endif
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
    test_result.total++;
    tweet = *set_iter;
    if ((ret_val = Classify(tweet, tweet.length(), output_class)) < 0) {
      std::cerr << "ERROR: could not find class\n";
      test_result.undefined++;
    } else if (ret_val == 0) {
      if (output_stream) {
        output_stream << expected_class_name << "|" << output_class << "|" << tweet << std::endl;
      }
      test_result.undefined++;
    } else {
      if (output_stream) {
        output_stream << expected_class_name << "|" << output_class << "|" << tweet << std::endl;
      }
      if (expected_class_name.compare(output_class) == 0) {
        test_result.correct++;
      } else {
        test_result.wrong++;
      }
      if (class_freq_map.find(output_class) != class_freq_map.end()) {
        class_freq_map[output_class] += 1;
      } else {
        class_freq_map[output_class] = 1;
      }
    }
  }

  tweets.clear();

  return class_freq_map.size();
}

int Classifier::TestTrainingSources(Corpus& class_freq_map,
                                    TestResult& test_result,
                                    std::ostream &output_stream,
                                    bool random_selection) {

  std::set<std::string> handles_set;
  std::set<std::string>::iterator set_iter;
  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {

    std::string class_name = m_config.iter->name;
    std::ifstream hfs(m_config.iter->handles_file.c_str());

    if (!hfs.is_open()) {
      std::cout << "ERROR: could not open handles file: " << m_config.iter->handles_file \
                << " for class: " << class_name << std::endl;
      continue;
    }

    std::string line;
    std::string::size_type loc;
    std::string handle;
    while (getline(hfs, line)) {
      loc = line.find("=", 0);
      if (loc != std::string::npos) {
        handle.assign(line, 0, loc);
        handles_set.insert(handle);
      } else {
        std::cerr << "ERROR: ill-written handles file\n";
      }
    }
    hfs.close();

    if (handles_set.size() <= 1) {
      continue;
    }

    unsigned int index = rand();
    index = index % handles_set.size();
    if (index > 0 && index >= handles_set.size()) {
      continue;
    }

    unsigned int temp_index = 0;
    for (set_iter = handles_set.begin(); set_iter != handles_set.end(); set_iter++) {
      if (temp_index == index) {
        handle = *set_iter;
        break;
      }
      temp_index++;
    }
    handles_set.clear();

    if (TestTwitterTimeline(handle, class_name, class_freq_map, test_result, output_stream) < 0) {
      std::cout << "ERROR: TestTwitterTimeline failed for class: " \
                << class_name << " on handle: " << handle << std::endl;
    }

/*
    if (CorpusManager::PrintCorpus(class_freq_map) < 0) {
      std::cerr << "ERROR: could not print corpus\n";
      break;
    }
*/
  } // end of for loop

  return 0;
}

} // namespace inagist_classifiers
