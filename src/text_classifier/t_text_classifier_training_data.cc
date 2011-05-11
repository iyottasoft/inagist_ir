#include "text_classifier.h"
#include <iostream>
#include <cstdlib>
#include <cassert>
#include <cstring>
#include "twitter_api.h"
#include "inagist_api.h"

int main(int argc, char* argv[]) {

  if (argc < 4 || argc > 5) {
    std::cout << "Usage: " << argv[0] << "\n\t<keytuples_config>\n\t<dictionary_override>\n\t<0/1/2/3, 0-interactive, 1-file, 2-tweet, 3-many tweets, 4-inagist>\n\t<file_name/handle>\n";
    return -1;
  }

  std::string keytuples_config = argv[1];
  std::string dictionary_file = argv[2];

  unsigned int input_type = atoi(argv[3]);
  assert(input_type >=0 && input_type <=4);

  std::string input_file_name;
  std::string handle;

  if (5 == argc) {
    if (1==input_type) {
      input_file_name = std::string(argv[4]);
    } else if (input_type >= 2) {
      handle = std::string(argv[4]);
    }
  }

  inagist_classifiers::TextClassifier tc;

  int my_argc = 1;
  char my_argv[1][255];
  strcpy((char *) my_argv[0], keytuples_config.c_str());
  std::cout << my_argv[0] << std::endl;
  if (tc.InitDependencies(my_argc, (char**) my_argv) < 0) {
    std::cerr << "ERROR: could not init keytuples extracter" \
              << " for training. config_file: " << keytuples_config << std::endl;
    return -1;
  }

  if (tc.LoadKeyTuplesDictionary(dictionary_file.c_str()) < 0) {
    tc.Clear();
    return -1;
  }

  inagist_classifiers::Corpus corpus;
  int count = 0;

  std::string text;
  std::set<std::string> tweets;
  std::set<std::string>::iterator set_iter;
  std::ifstream ifs;
  switch (input_type) {
    case 0:
      while (getline(std::cin, text)) {
        if (text.compare("exit") == 0 || text.compare("quit") == 0)
          break;
        tc.GetCorpus(text, corpus);
        inagist_classifiers::CorpusManager::PrintCorpus(corpus);
      }
      break;
    case 1:
      ifs.open(input_file_name.c_str());
      if (!ifs.is_open()) {
        std::cout << "ERROR: could not open input file: " << input_file_name << std::endl;
      }
      while (getline(ifs, text)) {
        tweets.insert(text);
      }
      ifs.close();
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        std::cout << *set_iter << std::endl;
        tc.GetCorpus(*set_iter, corpus);
        inagist_classifiers::CorpusManager::PrintCorpus(corpus);
      }
      break;
    case 2:
      // fall through
    case 3:
      // get top tweets from twitter api
      if (argc == 4) {
        inagist_api::TwitterAPI tapi;
        if ((tapi.GetPublicTimeLine(tweets)) < 0) {
          std::cout << "Error: could not get public timeline from twitter\n";
          return -1;
        }
        for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
          tc.GetCorpus(*set_iter, corpus);
          inagist_classifiers::CorpusManager::PrintCorpus(corpus);
          if (2 == input_type)
            break;
        }
      } else {
        unsigned int num_docs = 0;
        unsigned int corpus_size = 0;
        if ((count = tc.GetTrainingData(handle, num_docs, corpus, corpus_size)) < 0) {
          std::cerr << "ERROR: could not get training data for handle: " << handle << std::endl;
        } else {
          std::cout << "Corpus of size " << count << " generated for " << handle << std::endl;
        }
        inagist_classifiers::CorpusManager::PrintCorpus(corpus);
      }
      break;
    case 4:
      // get tweets from inagist api
      if (argc == 5) {
        inagist_api::InagistAPI ia;
        if ((ia.GetTrendingTweets(std::string(argv[4]), tweets)) < 0) {
          std::cout << "ERROR: could not get trending tweets from inagist\n";
          return -1;
        }
      } else {
        std::cout << "this feature has not been implemented yet\n";
        return -1;
      }
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        tc.GetCorpus(*set_iter, corpus);
        inagist_classifiers::CorpusManager::PrintCorpus(corpus);
      }
      break;
    default:
      break;
  }

  tweets.clear();

  tc.Clear();

  return 0;
}

