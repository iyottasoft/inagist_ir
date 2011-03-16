/* x_lang_test_data_generator.cc

   the idea behind this program is to ping twitter search for the public timeline,
   and find the relative frequency of languages. note that the same langdetect
   program which this code attempts to improve will be the one used in this code.
   but this is fine, since the data we generate is the posteri.

*/

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <cstring>
#include "language_detector.h"
#include "corpus_manager.h"
#include "trends_manager.h"
#include "twitter_api.h"

#define MAX_BUFFER_LEN 1024

inagist_trends::KeywordsExtract g_ke;

int Init(std::string& root_dir) {

  std::string stopwords_file = root_dir + "/data/static_data/stopwords.txt";
  std::string dictionary_file = root_dir + "/data/static_data/dictionary.txt";
  std::string unsafe_dictionary_file = root_dir + "/data/static_data/unsafe_dictionary.txt";
  std::string lang_detect_config_file = root_dir + "/configs/language_detection.config";
  std::string channels_dictionary_file = root_dir + "/data/static_data/channels_dictionary.txt";

  if (Init(stopwords_file.c_str(),
           dictionary_file.c_str(),
           unsafe_dictionary_file.c_str(),
           lang_detect_config_file.c_str(),
           channels_dictionary_file.c_str()) < 0) {
    std::cout << "ERROR: could not initialize keywords extract\n";
    return -1;
  }

  return 0;
}

int GetLangTestData(inagist_classifiers::Corpus& test_data_map,
                    unsigned int& tweets_num,
                    unsigned int& detected_num,
                    unsigned int& undefined_num,
                    unsigned int& non_latin_num,
                    unsigned int& unknown_script_num) {

  tweets_num = 0;
  detected_num = 0;
  undefined_num = 0;
  non_latin_num = 0;
  unknown_script_num = 0;

  std::set<std::string> tweets;
  if (inagist_api::TwitterAPI::GetPublicTimeLine(tweets) < 0) {
    std::cout << "ERROR: could not get tweets\n";
    return -1;
  }
  tweets_num = tweets.size();
  if (tweets_num < 1) {
    std::cout << "ERROR: no tweets found\n";
    return 0;
  }

  unsigned char tweet_buffer[MAX_BUFFER_LEN];
  tweet_buffer[MAX_BUFFER_LEN-1] = '\0';
  unsigned int tweet_len = 0;

  char safe_status[10];
  char script[4];
  unsigned char keywords[MAX_BUFFER_LEN];
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned char hashtags[MAX_BUFFER_LEN];
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;
  unsigned char keyphrases[MAX_BUFFER_LEN];
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
  char buffer1[MAX_BUFFER_LEN];
  char buffer2[MAX_BUFFER_LEN];
  char buffer3[MAX_BUFFER_LEN];
  char buffer4[MAX_BUFFER_LEN];

  std::set<std::string>::iterator set_iter;
  for (set_iter = tweets.begin(); set_iter != tweets.end(); set_iter++) {

    tweet_len = set_iter->length();
    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
      std::cout << "ERROR: invalid tweet\n";
      continue;
    }

    strcpy((char *) tweet_buffer, set_iter->c_str());
    tweet_buffer[tweet_len] = '\0';
    memset(safe_status, 0, 10);
    memset(script, 0, 4);
    keywords[0] = '\0';
    hashtags[0] = '\0';
    keyphrases[0] = '\0';
    buffer1[0] = '\0';
    buffer2[0] = '\0';
    buffer3[0] = '\0';
    buffer4[0] = '\0';
    int ret_value = 0;
    if ((ret_value = SubmitTweet((const unsigned char*) tweet_buffer, tweet_len,
                    (char *) safe_status, 10,
                    (char *) script, 4,
                    keywords, MAX_BUFFER_LEN,
                    &keywords_len, &keywords_count,
                    hashtags, MAX_BUFFER_LEN,
                    &hashtags_len, &hashtags_count,
                    keyphrases, MAX_BUFFER_LEN,
                    &keyphrases_len, &keyphrases_count,
                    (char *) buffer1, MAX_BUFFER_LEN,
                    (char *) buffer2, MAX_BUFFER_LEN,
                    (char *) buffer3, MAX_BUFFER_LEN,
                    (char *) buffer4, MAX_BUFFER_LEN)) < 0) {
      std::cout << "ERROR: submit tweet failed\n";
      tweets.clear();
      return -1;
    }

    if ((strcmp("RR", script) == 0) ||
        (strcmp("xx", script) == 0) ||
        (strcmp("uu", script) == 0)) {
      unknown_script_num++;
    } else if (strcmp("en", script) != 0) {
      non_latin_num++;
    } else if ((strlen(buffer1) < 2) ||
               (strcmp("RR", buffer1) == 0) ||
               (strcmp("xx", buffer1) == 0) ||
               (strcmp("uu", buffer1) == 0)) {
      undefined_num++;
    } else {
      std::string lang = std::string(buffer1);
      if (test_data_map.find(lang) != test_data_map.end())
        test_data_map[lang] += 1;
      else
        test_data_map[lang] = 1;
      detected_num++;
    }
    std::cout << "Tweet: " << tweet_buffer << std::endl;
    std::cout << "script: " << script << std::endl;
    std::cout << "lang guess 1: " << buffer1 << std::endl;
    std::cout << "lang guess 2: " << buffer2 << std::endl;
  }
  tweets.clear();

  return tweets_num;
}

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cout << "Usage: " << argv[0] << " <config_file_name>\n";
    return -1;
  }

  std::string config_file_name = argv[1];

  std::ifstream ifs(config_file_name.c_str());
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open config file " << config_file_name << std::endl;
    return -1;
  }

  std::string arguments(argv[0]);
  std::string::size_type loc = arguments.find("bin", 0);
  std::string root_dir;
  if (loc != std::string::npos) {
    loc-=1;
    root_dir.assign(argv[0], loc);
  }

  std::string test_data_file_name;
  std::string line;
  std::string key;
  std::string value;
  while (getline(ifs, line)) {
    loc = line.find("=", 0);
    if (loc == std::string::npos) {
      std::cout << "ERROR: invalid config file entry\n";
      break;
    }
    key.assign(line.c_str(), loc);
    value.assign(line.c_str(), loc+1, (line.length()-loc-1));
    if (key.compare(0, 8, "testdata") == 0) {
      test_data_file_name = value;
      break;
    }
  }
  ifs.close();

  if (test_data_file_name.length() < 1) {
    std::cout << "ERROR: no test_data_file_name given\n";
    return -1;
  }

  // if the test data file doesn't exist, create a dummy file
  std::ifstream test_data_ifs(test_data_file_name.c_str());
  if (!test_data_ifs.is_open()) {
    std::cout << "WARNING: could not open test_data file: " << test_data_file_name << std::endl;
    std::cout << "INFO: creating a test_data_file: " << test_data_file_name << std::endl;
    std::ofstream test_data_ofs(test_data_file_name.c_str());
    if (!test_data_ofs.is_open()) {
      std::cout << "ERROR: could not create dummy file: " << test_data_file_name << std::endl;
      return -1;
    } else {
      test_data_ofs.close();
    }
  } else {
    test_data_ifs.close();
  }

  if (Init(root_dir) < 0) {
    std::cout << "ERROR: could not initialize keywords extract\n";
    return -1;
  }

  unsigned int tweets_num = 0;
  unsigned int detected_num = 0;
  unsigned int undefined_num = 0;
  unsigned int non_latin_num = 0;
  unsigned int unknown_script_num = 0;
  unsigned int total_tweets_num = 0;
  unsigned int total_detected_num = 0;
  unsigned int total_undefined_num = 0;
  unsigned int total_non_latin_num = 0;
  unsigned int total_unknown_script_num = 0;
  inagist_classifiers::Corpus test_data_map;

  if (inagist_classifiers::CorpusManager::LoadCorpus(test_data_file_name, test_data_map) < 0) {
    std::cout << "ERROR: could not load test_data from file: " << test_data_file_name << std::endl;
    return -1;
  }

  tweets_num = 0;
  detected_num = 0;
  undefined_num = 0;
  if (GetLangTestData(test_data_map, tweets_num, detected_num,
                      undefined_num, non_latin_num, unknown_script_num) < 0) {
    std::cout << "ERROR: could not get lang test data\n";
  } else {
    total_tweets_num += tweets_num;
    total_detected_num += detected_num;
    total_undefined_num += undefined_num;
    total_non_latin_num += non_latin_num;
    total_unknown_script_num += unknown_script_num;
  }
  
  std::cout << std::endl << "total tweets: " << total_tweets_num << std::endl;
  std::cout << "total detected: " << total_detected_num << std::endl;
  std::cout << "total undefined lang: " << total_undefined_num << std::endl;
  std::cout << "total non latin script: " << total_non_latin_num << std::endl;
  std::cout << "total unknown script: " << total_unknown_script_num << std::endl;
  std::cout << "total failed: " << (total_tweets_num - total_undefined_num - total_detected_num - total_non_latin_num - total_unknown_script_num) << std::endl;

  if (!test_data_map.empty()) {
    std::ofstream ofs(test_data_file_name.c_str());
    if (!ofs.is_open()) {
      std::cout << "ERROR: could not open test data file: " << test_data_file_name << std::endl;
    } else {
      inagist_classifiers::CorpusIter corpus_iter;
      unsigned int sum = 0;
      for (corpus_iter = test_data_map.begin(); corpus_iter != test_data_map.end(); corpus_iter++) {
        if (corpus_iter->first.compare("all_classes") != 0) {
          ofs << corpus_iter->first << "=" << corpus_iter->second << std::endl;
          sum += corpus_iter->second;
        }
      }
      ofs << "all_classes=" << sum << std::endl;
      ofs.close();
    }
    test_data_map.clear();
  }

  return 0;
}

