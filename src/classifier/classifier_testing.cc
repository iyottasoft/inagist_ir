#include "classifier_testing.h"
#include <iostream>

namespace inagist_classifiers {

ClassifierTesting::ClassifierTesting() {
}

ClassifierTesting::~ClassifierTesting() {
}

int ClassifierTesting::WriteTestData(Corpus& corpus, const char* classes_freq_file) {

  if (corpus.empty() || !classes_freq_file) {
    std::cerr << "ERROR: invalid input. could not write test data\n";
    return -1;
  }

  inagist_classifiers::CorpusIter corpus_iter;
  unsigned int sum = 0;
  for (corpus_iter = corpus.begin(); corpus_iter != corpus.end(); corpus_iter++) {
    std::cout << corpus_iter->first << ":" << corpus_iter->second << std::endl;
    if ((corpus_iter->first.compare("UU") == 0) ||
        (corpus_iter->first.compare("XX") == 0) ||
        (corpus_iter->first.compare("RR") == 0)) {
      std::cout << "deleting" << std::endl;
      corpus.erase(corpus_iter);
      continue;
    } else if (corpus_iter->first.compare("all_classes") != 0) {
      sum += corpus_iter->second;
    } else {
      std::cout << "all_classes. hence ignoring" << std::endl;
    }
  }

  // update the all_classes value
  if ((corpus_iter = corpus.find("all_classes")) != corpus.end()) {
    corpus_iter->second = sum;
  } else {
    corpus.insert(std::pair<std::string, unsigned int> ("all_classes", sum));
  }

  // write to classes_freq file
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
//          3 - only given training source will be tested 
//          4 - test input is taken from the given file
// output_type:
//          0 - stdout
//          1 - class frequency file
//          2 - html version
//
// Note: the output file is to write texts that contributed to the class frequenceies.
//
int ClassifierTesting::GetTestData(const unsigned int& input_type,
                            const char* input_file,
                            const char* input_handle,
                            const std::string& expected_class_name,
                            const unsigned int& output_type,
                            const char* output_file) {

  if (NULL == input_file && 4 == input_type) {
    std::cerr << "ERROR: invalid input file. cannot get test data.\n";
    return -1;
  }

  if (NULL == input_handle && 5 == input_type) {
    std::cerr << "ERROR: invalid input handle. cannot get test data.\n";
    return -1;
  }

  if (expected_class_name.empty() && 3 == input_type) {
    std::cerr << "ERROR: invalid expected class name. cannot get test data.\n";
    return -1;
  }

  if (output_type > 0 && NULL == output_file) {
    std::cerr << "ERROR: invalid output file. cannot get test data.\n";
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
  const char* training_class = NULL;
  std::string handle;
  TestResult test_result;
  test_result.clear();
  const char* training_texts_file = NULL;

  switch (input_type) {
    case 0:
      // send empty handle and expected class_name to test public timeline
      if (TestTwitterTimeline(handle,
                              expected_class_name,
                              class_freq_map,
                              test_result,
                              *output_stream) < 0) {
        std::cerr << "ERROR: could not test twitter timeline\n";
        ofs.close();
        return -1;
      }
      break;
    case 1:
      if (TestTrainingSources(training_class=NULL,
                              class_freq_map,
                              test_result,
                              *output_stream,
                              random_selection = true) < 0) {
        std::cerr << "ERROR: could not test training sources at random\n";
        ofs.close();
        return -1;
      }
      break;
    case 2:
      if (TestTrainingSources(training_class=NULL,
                              class_freq_map,
                              test_result,
                              *output_stream,
                              random_selection = false) < 0) {
        std::cerr << "ERROR: could not test training sources at random\n";
        ofs.close();
        return -1;
      }
      break;
    case 3:
      if (TestTrainingSources(training_class=expected_class_name.c_str(),
                              class_freq_map,
                              test_result,
                              *output_stream,
                              random_selection = true) < 0) {
        std::cerr << "ERROR: could not test training sources at random\n";
        ofs.close();
        return -1;
      }
      break;
    case 4:
        if (TestTrainingTexts(training_texts_file=input_file,
                              expected_class_name,
                              class_freq_map,
                              test_result,
                              *output_stream) < 0) {
          std::cerr << "ERROR: could not test training sources from file: " \
                    << training_texts_file << std::endl;
          return -1;
        }
      break;
    case 5:
      if (input_handle)
        handle.assign(input_handle);
      if (TestTwitterTimeline(handle,
                              expected_class_name,
                              class_freq_map,
                              test_result,
                              *output_stream) < 0) {
        std::cerr << "ERROR: could not test tweets from hanlde: " << handle << std::endl;
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
      if (WriteTestData(class_freq_map, m_config.freqs_file.c_str()) < 0) {
        std::cerr << "ERROR: could not write test data to file: " \
                  << m_config.freqs_file << std::endl;
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

  std::cout << "Total text: " << test_result.total << std::endl;
  if (!expected_class_name.empty()) {
    std::cout << "Expected Class: " << expected_class_name << std::endl;
  }
  std::cout << "Correct: " << test_result.correct << std::endl;
  std::cout << "Wrong: " << test_result.wrong << std::endl;
  std::cout << "Undefined: " << test_result.undefined << std::endl;

  return ret_val;

}

int ClassifierTesting::TestTrainingTexts(const char* training_texts_file,
                                  const std::string& expected_class_name,
                                  Corpus& class_freq_map,
                                  TestResult& test_result,
                                  std::ostream &output_stream) {

  if (!training_texts_file) {
    std::cerr << "ERROR: invalid input training texts file\n";
    return -1;
  }

  std::ifstream ifs(training_texts_file);
  if (!ifs.is_open()) {
    std::cerr << "ERROR: could not open file with traning texts: " \
              << training_texts_file << std::endl;
    return -1;
  }

  std::string tweet;
  std::set<std::string> tweets;
  while (getline(ifs, tweet)) {
    tweets.insert(tweet);
  }
  ifs.close();

  if (tweets.empty()) {
    return 0;
  }

  std::set<std::string>::iterator set_iter;
  std::string output_class;
  std::string output_top_classes;
  unsigned int output_top_classes_count = 0;
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
    if ((ret_val = Classify(tweet, tweet.length(), output_class,
                            output_top_classes, output_top_classes_count)) < 0) {
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
      if (!expected_class_name.empty()) {
        if (expected_class_name.compare(output_class) == 0) {
          test_result.correct++;
        } else if ((output_class.compare(0,2,"UU") == 0) ||
                   (output_class.compare(0,2,"XX") == 0) ||
                   (output_class.compare(0,2,"RR") == 0)) {
           test_result.undefined++;
        } else {
          test_result.wrong++;
        }
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

int ClassifierTesting::TestTwitterTimeline(const std::string& handle,
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
  std::string output_top_classes;
  unsigned int output_top_classes_count = 0;
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
    if ((ret_val = Classify(tweet, tweet.length(), output_class,
                            output_top_classes, output_top_classes_count)) < 0) {
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
      if (!expected_class_name.empty()) {
        if (expected_class_name.compare(output_class) == 0) {
          test_result.correct++;
        } else if ((output_class.compare(0,2,"UU") == 0) ||
                   (output_class.compare(0,2,"XX") == 0) ||
                   (output_class.compare(0,2,"RR") == 0)) {
           test_result.undefined++;
        } else {
          test_result.wrong++;
        }
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

// training_class - this can be null. when not null, only the handles in the given class will be tested
// class_freq_map - output parameter for returning classes and their frequencies
// test_result - test result is a typedef to return the results of this testing exercise
// output_stream - if an output file is given, its fstream will be pointed by output_stream or else stdout
// random_selection - while choosing input from training sources, whether to randomly select classes and handles
int ClassifierTesting::TestTrainingSources(const char* training_class,
                                    Corpus& class_freq_map,
                                    TestResult& test_result,
                                    std::ostream &output_stream,
                                    bool random_selection) {

  std::set<std::string> handles_set;
  std::set<std::string>::iterator set_iter;
  for (m_config.iter = m_config.classes.begin();
       m_config.iter != m_config.classes.end();
       m_config.iter++) {

    std::string class_name = m_config.iter->name;

    if (NULL != training_class) {
      if (class_name.compare(0, strlen(training_class), training_class) != 0) {
        continue;
      }
    }

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

    if (random_selection) {
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
    } else {
      for (set_iter = handles_set.begin(); set_iter != handles_set.end(); set_iter++) {
        handle = *set_iter;
        if (TestTwitterTimeline(handle, class_name, class_freq_map, test_result, output_stream) < 0) {
          std::cout << "ERROR: TestTwitterTimeline failed for class: " \
                    << class_name << " on handle: " << handle << std::endl;
        }
      }
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

int ClassifierTesting::ValidateTestDataInput(int argc, char* argv[],
                                      const char* &config_file,
                                      const char* &keytuples_config_file,
                                      unsigned int &input_type,
                                      unsigned int &output_type,
                                      const char* &input_file,
                                      const char* &output_file,
                                      const char* &input_handle,
                                      std::string &class_name) {

  config_file = argv[1];
  keytuples_config_file = argv[2];
  input_type = atoi(argv[3]);
  output_type = atoi(argv[4]);

  if (8 == argc) {
    if (4 == input_type)
      input_file = argv[7];
    else if (5 == input_type)
      input_handle = argv[7];
    output_file = argv[6];

    if ((3 == input_type || 4 == input_type) &&
        (argv[5] && strlen(argv[5]) > 0))
      class_name = std::string(argv[5]);
  }

  if (7 == argc) {
    if (input_type == 4)
      input_file = argv[6];
    else if (input_type == 5)
      input_handle = argv[6];
    else if (output_type > 0)
      output_file = argv[6];

    if ((3 == input_type || 4 == input_type) &&
        (argv[5] && strlen(argv[5]) > 0))
      class_name = std::string(argv[5]);
  }

  if (6 == argc) {
    if (argv[5] && strlen(argv[5]) > 0) {
      if (3 == input_type || 4 == input_type) {
        class_name = argv[5];
      } else {
        output_file = argv[5];
      }
    }
  }

// validate inputs and assign inputs
  if (3 == input_type) {
    if (class_name.empty()) {
      std::cout << "class_name is required for input_type: " << input_type << std::endl;
      return -1;
    }
  } else if (4 == input_type) {
    if (!input_file) {
      std::cout << "input file required for input_type: " << input_type << std::endl;
      return -1;
    }
  } else if (5 == input_type) {
    if (!input_handle) {
      std::cout << "input handle required for input_type: " << input_type << std::endl;
      return -1;
    }
  }

  if (output_type < 0 || output_type > 2) {
    std::cout << "invalid output type: " << output_type << std::endl;
    return -1;
  }

  if (1 == output_type) {
    if (!output_file) {
      std::cout << "output file required for output_type: " << output_type << std::endl;
      return -1;
    }
    if (strlen(output_file) < 4) {
      std::cout << "invalide output file name: " << output_file << std::endl;
      return -1;
    }
  }

  std::cout << "config_file: ";
  if (config_file)
    std::cout << config_file;
  std::cout << std::endl;

  std::cout << "keytuples_config: ";
  if (keytuples_config_file)
    std::cout << keytuples_config_file;
  std::cout << std::endl;

  std::cout << "input_type: " << input_type << std::endl;
  std::cout << "output_type: " << output_type << std::endl;
  std::cout << "class_name: " << class_name << std::endl;

  std::cout << "input_file: ";
  if (input_file)
    std::cout << input_file;
  std::cout << std::endl;

  std::cout << "output_file: ";
  if (output_file)
    std::cout << output_file;
  std::cout << std::endl;

  std::cout << "input_handle: ";
  if (input_handle)
    std::cout << input_handle;
  std::cout << std::endl;

  return 0;
}

} // namespace inagist_classifiers
