#include "language_detector.h"
#include <iostream>
#include <fstream>
#include "twitter_searcher.h"

namespace inagist_classifiers {

LanguageDetector::LanguageDetector() {
}

LanguageDetector::~LanguageDetector() {
  Clear();
}

int LanguageDetector::Init(std::string config_file_name) {

  // this config file name should have corpus files
  // and the strings with which the corpus contents can be uniquely identified
  if (m_corpus_manager.LoadCorpusMap(config_file_name) < 0) {
    std::cerr << "ERROR: could not load Corpus Map\n";
    return -1;
  }

  return 0;
}
 
int LanguageDetector::DetectLanguage(const std::string& text, const unsigned int& text_len,
                                     std::string& guess_lang_output) {
  int num_ngrams = 0;
  Corpus test_corpus;
  if ((num_ngrams = m_ngrams_generator.GetNgrams(text.c_str(), text_len, test_corpus)) < 0) {
    std::cerr << "ERROR: could not find ngrams" << std::endl;
    return -1;
  }

  if (num_ngrams == 0) {
    std::cout << "no ngrams found\n";
    return 0;
  }

  if (m_naive_bayes_classifier.GuessClass(m_corpus_manager.m_corpus_map,
                                          test_corpus,
                                          guess_lang_output) < 0) {
    std::cout << "ERROR: naive bayes classifiers could not guess the language\n";
  }

  test_corpus.clear();

  return 0;
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
    if (m_ngrams_generator.GetNgrams(line.c_str(), line.length(), corpus) <= 0) {
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
  std::ofstream ofs(output_tweets_file_name.c_str());
  if (!ofs) {
    std::cout << "ERROR: could not open tweets output file " << output_tweets_file_name << std::endl;
    handles.clear();
    return -1;
  }

  std::set<std::string>::iterator handle_iter;
  unsigned int num_tweets = 0;
  for (handle_iter = handles.begin(); handle_iter != handles.end(); handle_iter++) {
    if (twitter_searcher.GetTweetsFromUser(*handle_iter, tweets) > 0) {
      num_tweets += tweets.size();
      std::set<std::string>::iterator set_iter;
      for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {
        ofs << *set_iter << std::endl;
      }
      tweets.clear();
    }
  }
  ofs.close();
  handles.clear();

  if (num_tweets == 0) {
    std::cout << "No tweets found for handles in file " << twitter_handles_file_name << std::endl;
    return 0;
  }

  int ret_value = 0;
  if ((ret_value = GenerateLangModel(output_tweets_file_name, output_file_name)) < 0) {
    std::cout << "ERROR: could not generate lang model for tweets in file " << output_tweets_file_name << std::endl;
    return -1;
  }

  return ret_value;
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

