#include "language_detector.h"
#include <iostream>
#include <fstream>
#include "twitter_searcher.h"
#include "string_utils.h"

#ifdef DEBUG
#define LD_DEBUG=DEBUG
#endif
//#define LD_DEBUG 0

namespace inagist_classifiers {

LanguageDetector::LanguageDetector() {
}

LanguageDetector::~LanguageDetector() {
  Clear();
}

int LanguageDetector::Init(std::string config_file_name) {

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified

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
    //std::string handles_file_name;
    //std::string tweets_file_name;
    std::string corpus_file_name;
    std::string corpus_class_name;
    std::map<std::string, std::string> corpus_class_file_map;
    while (getline(ifs, line)) {
      line_count++;
      // std::cout << line << std::endl;
      loc = line.find("=", 0);
      if (loc == std::string::npos) {
        std::cout << "ERROR: invalid config file entry\n";
        break;
      }
      key.assign(line.c_str(), loc);
      value.assign(line.c_str(), loc+1, (line.length()-loc-1));
      //std::cout << key << std::endl;
      //std::cout << value << std::endl;
      if (key.compare(0, 4, "lang") == 0) {
        corpus_class_name = value;
      }
      //else if (key.compare(0, 7, "handles") == 0) {
        //handles_file_name = value;
      //}
      else if (key.compare(0, 6, "corpus") == 0) {
        corpus_file_name = value;
      }
      //else if (key.compare(0, 6, "tweets") == 0) {
      //  tweets_file_name = value;
      //}
      if (line_count == 4) {
        //std::cout << "loading " << class_name << " with " << corpus_file_name << std::endl;
        corpus_class_file_map[corpus_class_name] = corpus_file_name;
        line_count = 0;
      }
    }
    ifs.close();
    if (!corpus_class_file_map.empty()) {
      if (m_corpus_manager.LoadCorpusMap(corpus_class_file_map) < 0) {
        std::cerr << "ERROR: could not load Corpus Map\n";
        return -1;
      }
    }
  }

  return 0;
}
 
int LanguageDetector::DetectLanguage(const std::string& text, const unsigned int& text_len,
                                     std::string& guess_lang_output) {
  int num_ngrams = 0;
  Corpus test_corpus;

  if ((num_ngrams = m_ngrams_generator.GetNgramsFromTweet(text, test_corpus)) < 0) {
    std::cerr << "ERROR: m_ngrams_generator returned -1" << std::endl;
    return -1;
  }

  if (num_ngrams == 0) {
#ifdef LD_DEBUG
    if (LD_DEBUG > 0)
      std::cout << "no ngrams found for ... \n" << text << std::endl;
#endif
    guess_lang_output.assign("RR");
    return 0;
  }

#ifdef LD_DEBUG
  if (LD_DEBUG > 1)
    std::cout << "now guessing class for ... \n" << text << std::endl;
#endif

  if (m_naive_bayes_classifier.GuessClass(m_corpus_manager.m_corpus_map,
                                          test_corpus,
                                          guess_lang_output) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the language\n";
    test_corpus.clear();
    return -1;
  }

#ifdef LD_DEBUG
  if (LD_DEBUG > 1)
    std::cout << "guess_lang: " << guess_lang_output << std::endl;
#endif

  test_corpus.clear();

  return 1;
}

int LanguageDetector::GetNgramFrequencies(const std::string& input_file_name,
                                          Corpus& corpus) {

  std::ifstream ifs(input_file_name.c_str());
  if (!ifs) {
    std::cout << "ERROR: could not open file " << input_file_name << std::endl;
    return -1;
  }

  std::string line;
  int num_docs = 0;
  while (getline(ifs, line)) {
    if (m_ngrams_generator.GetNgrams((unsigned char*) line.c_str(), line.length(), corpus) <= 0) {
      num_docs++;
    }
  }
  ifs.close();

  return corpus.size();
}

int LanguageDetector::GenerateLangModel(const std::string& input_file_name,
                                     const std::string& output_file_name) {

  Corpus lang_corpus;
  int count = 0;
  if ((count = GetNgramFrequencies(input_file_name, lang_corpus)) < 0) {
    std::cout << "ERROR: could not get features for lang in file " << input_file_name;
    return -1;
  } else if (count > 0) {
    if (m_corpus_manager.WriteCorpusToFile(lang_corpus, output_file_name) < 0) {
      std::cout << "ERROR: could not write to features to output file " << output_file_name << std::endl;
    }
  }
  lang_corpus.clear();

  return count;
}

int LanguageDetector::GenerateLangModelFromTweets(const std::string& twitter_handles_file_name,
                                               const std::string& output_tweets_file_name,
                                               const std::string& output_file_name) {

  std::ifstream ifs(twitter_handles_file_name.c_str());
  if (!ifs) {
    std::cout << "ERROR: could not open file " << twitter_handles_file_name << std::endl;
    return -1;
  }

  std::string handle;
  std::set<std::string> handles;
  while(getline(ifs, handle)) {
    handles.insert(handle);
  }
  ifs.close();

  if (handles.size() < 1) {
    std::cout << "ERROR: no handles found in file " << twitter_handles_file_name << std::endl;
    return 0;
  }

  std::set<std::string> tweets;
  inagist_api::TwitterSearcher twitter_searcher;

  std::set<std::string>::iterator handle_iter;
  unsigned int num_tweets = 0;
  unsigned int num_ngrams = 0;
  unsigned int ngrams_temp = 0;
  Corpus lang_corpus;
  for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
    if (twitter_searcher.GetTweetsFromUser(*handle_iter, tweets) > 0) {
      num_tweets += tweets.size();
      std::set<std::string>::iterator set_iter;
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        if ((ngrams_temp = m_ngrams_generator.GetNgramsFromTweet(*set_iter, lang_corpus)) < 0) {
          std::cerr << "ERROR: could not find ngrams from tweet: " << *set_iter << std::endl;
        } else {
          num_ngrams += ngrams_temp;
        }
      }
      tweets.clear();
    }
  }
  handles.clear();

  if (num_tweets == 0) {
    std::cout << "No tweets found for handles in file " << twitter_handles_file_name << std::endl;
    return 0;
  } else {
    if (m_corpus_manager.WriteCorpusToFile(lang_corpus, output_file_name) < 0) {
      std::cout << "ERROR: could not write to features to output file " << output_file_name << std::endl;
    }
  }

  lang_corpus.clear();

  return num_ngrams;
}

int LanguageDetector::Clear() {
  try {
    m_corpus_manager.Clear();
  } catch (...) {
    std::cerr << "ERROR: Corpus Manager throws exception" << std::endl;
  }
  return 0;
}

} // namespace inagist_classifiers

