#include "language_detector.h"
#include "trends_manager.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <string>
#include <cstring>

#define MAX_BUFFER_LEN 10240

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

  char tweets_buffer[MAX_BUFFER_LEN];
  unsigned int tweets_len = 0;
  if (GetTestTweets(handle.c_str(), MAX_BUFFER_LEN, tweets_buffer, &tweets_len) < 0) {
    std::cout << "ERROR: could not get tweets for handle: " << handle << std::endl;
    ostream_ptr << "<tr><td>ERROR: could not get tweets for handle: " << handle << "</td></tr>" << std::endl;
    ostream_ptr << "</table>" << std::endl;
    return -1;
  }

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

  char *tweet_start = tweets_buffer;
  char *tweet_end = strstr(tweet_start, "|");
  unsigned int tweet_len = 0;
  int flag = 0;

  while (tweet_start && tweet_end && *tweet_end != '\0') {

    tweet_end = strstr(tweet_start, "|");
    if (!tweet_end)
      break;
    *tweet_end = '\0';
    tweet_len = tweet_end - tweet_start;

    if (tweet_len <= 0 || tweet_len >= MAX_BUFFER_LEN) {
      std::cout << "ERROR: invalid tweet\n";
    }

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
    if ((ret_value = SubmitTweet((const unsigned char *) tweet_start, tweet_len,
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
      ostream_ptr << "<br/>" << tweet_start << "<br/>expected: " << expected_lang << "<br/>script: " << script << "<br/>guess 1: " << buffer1 << "<br/>guess 2: " << buffer2 << std::endl;
      ostream_ptr << "</td></tr>\n" << std::endl;
    } else {
      ostream_ptr << "Tweet: " << tweet_start << std::endl;
      ostream_ptr << "expected lang: " << expected_lang << std::endl;
      ostream_ptr << "script: " << script << std::endl;
      ostream_ptr << "lang guess 1: " << buffer1 << std::endl;
      ostream_ptr << "lang guess 2: " << buffer2 << std::endl;
    }

    *tweet_end = '|';
    tweet_start = tweet_end + 1;
  }
  tweet_start = NULL;
  tweet_end = NULL;

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

  std::string line;
  std::string key;
  std::string value;
  int line_count = 0;
  std::string lang;
  std::string handles_file_name;
  std::string output_tweets_file_name;
  std::string output_corpus_file_name;
  std::string training_data_file_name;
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
  while (getline(ifs, line)) {
    if (key.compare(0, 8, "testdata") != 0) {
      line_count++;
    }
    loc = line.find("=", 0);
    if (loc == std::string::npos) {
      std::cout << "ERROR: invalid config file entry\n";
      break;
    }
    key.assign(line.c_str(), loc);
    value.assign(line.c_str(), loc+1, (line.length()-loc-1));
    if (key.compare(0, 4, "lang") == 0) {
      lang = value;
    } else if (key.compare(0, 7, "handles") == 0) {
      handles_file_name = value;
    } else if (key.compare(0, 6, "corpus") == 0) {
      output_corpus_file_name = value;
    } else if (key.compare(0, 6, "tweets") == 0) {
      output_tweets_file_name = value;
    } else if (key.compare(0, 12, "trainingdata") == 0) {
      training_data_file_name = value;
    }
    if (line_count == 5) {
      line_count = 0;
      std::ifstream hfs(handles_file_name.c_str());
      if (!hfs.is_open()) {
        std::cout << "ERROR: could not open handles file: " << handles_file_name \
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
  }
  ifs.close();

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

