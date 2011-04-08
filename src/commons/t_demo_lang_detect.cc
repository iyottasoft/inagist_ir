#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <set>
#include <cstring>
#include "language_detector.h"
#include "keytuples_extracter.h"
#include "twitter_searcher.h"
#include "config_reader.h"

inagist_trends::KeyTuplesExtracter g_kt;

int Init(std::string& root_dir) {

  std::string stopwords_file = root_dir + "/data/static_data/stopwords.txt";
  std::string dictionary_file = root_dir + "/data/static_data/dictionary.txt";
  std::string unsafe_dictionary_file = root_dir + "/data/static_data/unsafe_dictionary.txt";
  std::string lang_detect_config_file = root_dir + "/configs/language_detection.config";
  std::string channels_dictionary_file = root_dir + "/data/static_data/channels_dictionary.txt";

  if (g_kt.Init(stopwords_file.c_str(),
           dictionary_file.c_str(),
           unsafe_dictionary_file.c_str(),
           lang_detect_config_file.c_str(),
           channels_dictionary_file.c_str()) < 0) {
    std::cout << "ERROR: could not initialize keywords extract\n";
    return -1;
  }

  return 0;
}

int TestLangForHandle(std::string& handle, const char* expected_lang,
                      unsigned int& tweets_num, unsigned int& detected_num,
                      unsigned int& undefined_num,
                      const unsigned int output_type, std::ostream& ostream_ptr) {

  if (output_type == 1) {
    ostream_ptr << "<table width=100%>" << std::endl;
    ostream_ptr << "<tr width=100%><td width=100%>" << std::endl;
  }
  ostream_ptr << "handle: " << handle << std::endl;
  if (output_type == 1)
    ostream_ptr << "</td></tr>" << std::endl;

  std::set<std::string> tweets;
  std::set<std::string>::iterator tweet_iter;

  inagist_api::TwitterSearcher twitter_searcher;
  if (twitter_searcher.GetTweetsFromUser(handle, tweets) <= 0) {
    std::cerr << "ERROR: could not get tweets for handle: " << handle << std::endl;
    return -1;
  }

  char tweet_buffer[MAX_BUFFER_LEN];
  unsigned int tweet_len = 0;

  char safe_status[10];
  unsigned int safe_status_buffer_len = 10;
  char script[4];
  unsigned int script_buffer_len = 4;
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
  unsigned int buffer_len = MAX_BUFFER_LEN;
  int flag = 0;

  for (tweet_iter = tweets.begin(); tweet_iter != tweets.end(); tweet_iter++) {
    tweet_len = (*tweet_iter).length();
    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
      std::cout << "ERROR: invalid tweet\n";
    }

    strcpy(tweet_buffer, (*tweet_iter).c_str());

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
    tweets_num++;
    if ((ret_value = g_kt.GetKeyTuples((unsigned char *) tweet_buffer, tweet_len,
                    (char *) safe_status, safe_status_buffer_len,
                    (char *) script, script_buffer_len,
                    (unsigned char*) keywords, buffer_len,
                    keywords_len, keywords_count,
                    (unsigned char*) hashtags, buffer_len,
                    hashtags_len, hashtags_count,
                    (unsigned char*) keyphrases, buffer_len,
                    keyphrases_len, keyphrases_count,
                    (char *) buffer1, buffer_len,
                    (char *) buffer2, buffer_len,
                    (char *) buffer3, buffer_len,
                    (char *) buffer4, buffer_len)) < 0) {
      if (output_type == 1)
        ostream_ptr << "</table>" << std::endl;
      return -1;
    }

    if (strcmp("en", script) != 0) {
      if (strcmp(expected_lang, buffer1) == 0)
        flag = 1;
      else
        flag = 0;
    } else if ((strcmp(expected_lang, buffer1) == 0) ||
        ((strcmp(expected_lang, script) == 0) &&
         (strcmp("en", script) != 0))) {
      detected_num++;
      flag = 2;
    } else if ((strlen(buffer1) < 2) ||
               (strcmp("RR", buffer1) == 0) ||
               (strcmp("xx", buffer1) == 0) ||
               (strcmp("uu", buffer1) == 0)) {
      undefined_num++;
      flag = -1;
    } else {
      flag = -2;
    }

    if (output_type == 1) {
      if (2 == flag) {
        ostream_ptr << "<tr><td bgcolor=#07B133>";
      } else if (1 == flag) {
        ostream_ptr << "<tr><td bgcolor=#3EA99F>";
      } else if (0 == flag) {
        ostream_ptr << "<tr><td bgcolor=#FFFFFF>";
      } else if (-1 == flag) {
        ostream_ptr << "<tr><td bgcolor=#C71585>";
      } else if (-2 == flag) {
        ostream_ptr << "<tr><td bgcolor=#FF0000>";
      }
      ostream_ptr << "<br/>" << tweet_buffer << "<br/>expected: " << expected_lang << "<br/>script: " << script << "<br/>guess 1: " << buffer1 << "<br/>guess 2: " << buffer2 << std::endl;
      ostream_ptr << "</td></tr>\n" << std::endl;
    } else {
      ostream_ptr << "Tweet: " << tweet_buffer << std::endl;
      ostream_ptr << "expected lang: " << expected_lang << std::endl;
      ostream_ptr << "script: " << script << std::endl;
      ostream_ptr << "lang guess 1: " << buffer1 << std::endl;
      ostream_ptr << "lang guess 2: " << buffer2 << std::endl;
    }
  }
  tweets.clear();

  if (output_type == 1)
    ostream_ptr << "<tr><td>" << std::endl;

  ostream_ptr << std::endl << "tweets: " << tweets_num \
      << " detected: " << detected_num \
      << " undefined: " << undefined_num << std::endl;

  if (output_type == 1)
    ostream_ptr << "</td></tr>" << std::endl;

  if (output_type == 1) {
    ostream_ptr << "<hr/>" << std::endl;
    ostream_ptr << "</table>\n" << std::endl;
  }

  return 0;
}

int main(int argc, char* argv[]) {

  if (argc != 2 && argc != 3) {
    std::cout << "Usage: " << argv[0] << " <config_file_name> [output_html_file]\n";
    return -1;
  }

  std::string config_file_name = argv[1];

  std::ifstream ifs(config_file_name.c_str());
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open config file " << config_file_name << std::endl;
    return -1;
  }

  std::ofstream ofs;
  std::ostream* ostream_ptr;
  std::string output_html_file_name;
  unsigned int output_type = 0;
  if (argc == 3) {
    output_html_file_name = std::string(argv[2]);
    ofs.open(output_html_file_name.c_str());
    output_type = 1;
    ostream_ptr = &ofs;
    *ostream_ptr << "<html>\n<head>\n" \
                 << "<title>Inagist Language Detection</title>\n" \
                 << "<h3>Inagist Language Detection</h3>\n" \
                 << "</head>\n<body>\n<table width=100%>" << std::endl;
    ofs.close();
    ofs.open(output_html_file_name.c_str(), std::ios_base::app);
    output_type = 1;
    ostream_ptr = &ofs;
  } else {
    ostream_ptr = &std::cout;
  }

  std::string arguments(argv[0]);
  std::string::size_type loc = arguments.find("bin", 0);
  std::string root_dir;
  if (loc != std::string::npos) {
    loc-=1;
    root_dir.assign(argv[0], loc);
  }

  if (Init(root_dir) < 0) {
    std::cout << "ERROR: could not initialize keywords extract\n";
    return -1;
  }

  inagist_classifiers::Config config;
  if (inagist_classifiers::ConfigReader::Read(config_file_name.c_str(), config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file_name << std::endl;
    return -1;
  }

  if (config.classes.empty()) {
    std::cerr << "ERROR: class structs could not be read from config file: " << config_file_name << std::endl;
    return -1;
  }

  unsigned int tweets_num = 0;
  unsigned int detected_num = 0;
  unsigned int undefined_num = 0;
  unsigned int total_tweets_num = 0;
  unsigned int total_detected_num = 0;
  unsigned int total_undefined_num = 0;
  char debug_str[255];
  memset(debug_str, '\0', 255);
  std::set<std::string> debug_str_set;
  std::set<std::string> handles_set;
  std::set<std::string>::iterator set_iter;

  for (config.iter = config.classes.begin(); config.iter != config.classes.end(); config.iter++) {
    std::string lang = config.iter->name;
    std::ifstream hfs(config.iter->handles_file.c_str());
    if (!hfs.is_open()) {
      std::cout << "ERROR: could not open handles file: " << config.iter->handles_file \
                << " for lang: " << lang << std::endl;
      continue;
    } else {

      std::string handle;
      while (getline(hfs, handle)) {
        handles_set.insert(handle);
      }
      hfs.close();

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

      tweets_num = 0;
      detected_num = 0;
      undefined_num = 0;
      if (output_type == 1) {
        *ostream_ptr << "<tr width=100%><td width=100%>" << std::endl;
      }
      if (TestLangForHandle(handle, lang.c_str(),
                            tweets_num, detected_num, undefined_num,
                            output_type, *ostream_ptr) < 0) {
        std::cout << "ERROR: TestLangForHandle failed for lang: " \
                  << lang << "on handle: " << handle << std::endl;
      }
      if (output_type == 1) {
        *ostream_ptr << "</td></tr>" << std::endl;
      }
      total_tweets_num += tweets_num;
      total_detected_num += detected_num;
      total_undefined_num += undefined_num;
      memset(debug_str, '\0', 255);
      if (output_type == 1) {
        sprintf(debug_str, "<tr><td>%s</td><td>%u</td><td>%u</td><td>%u</td></tr>", lang.c_str(), tweets_num, detected_num, undefined_num);
      } else {
        sprintf(debug_str, "%s %u %u %u", lang.c_str(), tweets_num, detected_num, undefined_num);
      }
      debug_str_set.insert(std::string(debug_str));
    }
  }

  if (output_type == 1)
    *ostream_ptr << "<tr><td><br/>" << std::endl;
  *ostream_ptr << "Summary:" << std::endl;
  if (output_type == 1)
    *ostream_ptr << "<br/>" << std::endl;
  if (output_type == 1)
    *ostream_ptr << "<table border=1>" << std::endl;
  for (set_iter = debug_str_set.begin(); set_iter != debug_str_set.end(); set_iter++) {
    *ostream_ptr << *set_iter << std::endl;
  }
  if (output_type == 1)
    *ostream_ptr << "</table>" << std::endl;
  debug_str_set.clear();

  if (output_type == 1)
    *ostream_ptr << "<br/>" << std::endl;

  *ostream_ptr << std::endl << "total tweets: " << total_tweets_num << std::endl;
  if (output_type == 1)
    *ostream_ptr << "<br/>" << std::endl;
  *ostream_ptr << "total detected: " << total_detected_num << std::endl;
  if (output_type == 1)
    *ostream_ptr << "<br/>" << std::endl;
  *ostream_ptr << "total undefined: " << total_undefined_num << std::endl;
  if (output_type == 1)
    *ostream_ptr << "<br/>" << std::endl;
  *ostream_ptr << "total failed: " << (total_tweets_num - total_undefined_num - total_detected_num) << std::endl;
  if (output_type == 1)
    *ostream_ptr << "</td></tr>" << std::endl;

  if (output_type == 1) {
    *ostream_ptr << "</table></body>\n</html>" << std::endl;
  }

  if (argc == 3) {
    flush(*ostream_ptr);
    ofs.close();
  }

  return 0;
}

