#include "language_detector.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cassert>
#include "twitter_api.h"
#include "corpus_manager.h"

int main(int argc, char* argv[]) {

  if (argc < 3 || argc > 4) {
    std::cout << "Usage: " << argv[0] << " \n\t<0/1/2, 0-interactive|1-file/2-tweet/3-manytweets> \n\t<0/1/2, 0-create|1-test|2-clean> [<output_file_name>]\n";
    return -1;
  }

  unsigned int input_type = atoi(argv[1]);
  assert((input_type >= 0 && input_type <= 3));
 
  unsigned int test_type = atoi(argv[2]);
  assert(test_type >= 0 && test_type <= 2);

  std::string output_file_name;
  if (4 == argc) 
    output_file_name = std::string(argv[3]);

  inagist_classifiers::LanguageDetector ld;
  inagist_classifiers::NgramsGenerator ng;

  std::string text;
  std::string line;
  inagist_classifiers::Corpus lang_corpus;
  inagist_classifiers::CorpusIter iter;
  if (0 == input_type) {
    while (getline(std::cin, text)) {
      if (ng.GetNgramsFromTweet(text, lang_corpus) < 0) {
        std::cout << "ERROR: could not get ngrams from tweet\n";
      } else {
        for (iter = lang_corpus.begin(); iter != lang_corpus.end(); iter++) {
          std::cout << iter->first << " " << iter->second << std::endl;
        }
      }
      lang_corpus.clear();
    }
    return 0;
  }

  if (1 == input_type) {
    std::cout << "This feature is not implemented yet\n";
  }

  if (2 == input_type || 3 == input_type) {
    std::set<std::string> tweets;
    inagist_api::TwitterAPI tapi;
    if (tapi.GetPublicTimeLine(tweets) < 0) {
      std::cout << "Error: could not get trending tweets from inagist\n";
      return -1;
    }
    std::set<std::string>::iterator set_iter;
    for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
      text = *set_iter;
      if (ng.GetNgramsFromTweet(text, lang_corpus) < 0) {
        std::cout << "ERROR: could not get ngrams from tweet\n";
      } else {
        for (iter = lang_corpus.begin(); iter != lang_corpus.end(); iter++) {
          std::cout << iter->first << " " << iter->second << std::endl;
        }
      }
      lang_corpus.clear();
      if (2 == input_type)
        getchar();
    }
    tweets.clear();
  }

  ld.Clear();

  return 0;
}

