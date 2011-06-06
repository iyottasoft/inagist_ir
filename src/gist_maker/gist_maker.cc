/* gist_maker.cc */

#include "gist_maker.h"

#ifdef DEBUG
#if DEBUG>0
#define GIST_DEBUG DEBUG
#endif
#endif
//#define GIST_DEBUG 3

#include <set>
#include <cstring>
#include <cstdlib>

#define MAX_BUFFER_LEN 1024
#define MAX_CLASS_NAME   32
#define MAX_LIST_LEN    255

namespace inagist {

GistMaker::GistMaker() {
}

GistMaker::~GistMaker() {
}

int GistMaker::Init(const char* keytuples_extracter_config_file,
                    const char* language_detection_config_file,
                    const char* text_classification_config_file,
                    const char* sentiment_analyser_config_file) {

  if (!keytuples_extracter_config_file ||
      !language_detection_config_file ||
      !text_classification_config_file) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif
    return -1;
  }

#ifdef GIST_DEBUG
  std::cout << "INFO: initializing keytuples_extracter with config: " \
            << keytuples_extracter_config_file << std::endl;
#endif
  if (m_keytuples_extracter.Init(keytuples_extracter_config_file) < 0) {
    std::cerr << "ERROR: could not initialize KeyTuplesExtracter\n";
    return -1;
  }

#ifdef GIST_DEBUG
  std::cout << "INFO: initializing language_detector with config: " \
            << language_detection_config_file << std::endl;
#endif
  if (m_language_detector.Init(language_detection_config_file) < 0) {
    std::cerr << "ERROR: could not initialize LanguageDetector\n";
    return -1;
  }

#ifdef GIST_DEBUG
  std::cout << "INFO: initializing text_classifier with config: " \
            << text_classification_config_file << std::endl;
#endif
  if (m_text_classifier.Init(text_classification_config_file) < 0) {
    std::cerr << "ERROR: could not initialize TextClassifier\n";
    return -1;
  }

  return 0;
}

int GistMaker::GetGist(const std::string& text) {

  std::string safe_status;
  std::string script;
  std::string lang;
  std::set<std::string> keywords;
  std::set<std::string> keyphrases;
  std::set<std::string> hashtags;
  std::set<std::string> text_classes;
  std::string sentiment;

  if (GetGist(text, safe_status, script, lang,
              keywords, keyphrases, hashtags,
              text_classes, sentiment) < 0) {
    std::cout << "ERROR: could not get the gist for:" << text << std::endl;
  } else {
    std::cout << "script: " << script << std::endl;
    std::cout << "lang: " << lang << std::endl;
    std::set<std::string>::iterator set_iter;
    std::cout << "keywords: ";
    for (set_iter = keywords.begin(); set_iter != keywords.end(); set_iter++) {
      std::cout << *set_iter << " | ";
    }
    std::cout << std::endl;
    keywords.clear();
    std::cout << "keyphrases:  ";
    for (set_iter = keyphrases.begin(); set_iter != keyphrases.end(); set_iter++) {
      std::cout << *set_iter << " | ";
    }
    std::cout << std::endl;
    keyphrases.clear();
    std::cout << "hashtags:  ";
    for (set_iter = hashtags.begin(); set_iter != hashtags.end(); set_iter++) {
      std::cout << *set_iter << " | ";
    }
    std::cout << std::endl;
    hashtags.clear();
    std::cout << "text_classes: ";
    for (set_iter = text_classes.begin(); set_iter != text_classes.end(); set_iter++) {
      std::cout << *set_iter << " | ";
    }
    std::cout << std::endl;
    //std::cout << "sentiment: " << sentiment << std::endl;
    std::cout << "safe status: " << safe_status << std::endl;
  }
  return 0;
}

int GistMaker::GetGist(const std::string& text,
                       std::string& safe_status,
                       std::string& script,
                       std::string& lang,
                       std::set<std::string>& keywords,
                       std::set<std::string>& hashtags,
                       std::set<std::string>& keyphrases,
                       std::set<std::string>& text_classes,
                       std::string& sentiment) {

  if (text.length() < 1) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  unsigned char buffer[MAX_BUFFER_LEN];
  memset(buffer, 0, MAX_BUFFER_LEN);
  char safe_status_buffer[10];
  memset(safe_status_buffer, 0, 10);
  char script_buffer[4];
  memset(script_buffer, 0, 4);
  char lang_buffer[4];
  memset(lang_buffer, 0, 4);
  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  memset(keywords_buffer, 0, MAX_BUFFER_LEN);
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned char hashtags_buffer[MAX_BUFFER_LEN];
  memset(hashtags_buffer, 0, MAX_BUFFER_LEN);
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;
  unsigned char keyphrases_buffer[MAX_BUFFER_LEN];
  memset(keyphrases_buffer, 0, MAX_BUFFER_LEN);
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
  char text_classes_buffer[MAX_LIST_LEN];
  memset(text_classes_buffer, 0, MAX_LIST_LEN);
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;
  char sentiment_buffer[MAX_CLASS_NAME];
  memset(sentiment_buffer, 0, MAX_CLASS_NAME);

  unsigned int safe_status_buffer_len = 10;
  unsigned int script_buffer_len = 10;
  unsigned int lang_buffer_len = 4;

  strcpy((char*) buffer, text.c_str());
  int ret_value = 0;
  if ((ret_value = GetGist((const unsigned char*) buffer, strlen((char*) buffer),
                (char*) safe_status_buffer, safe_status_buffer_len,
                (char*) script_buffer, script_buffer_len,
                (char*) lang_buffer, lang_buffer_len,
                (unsigned char*) keywords_buffer, MAX_BUFFER_LEN,
                &keywords_len, &keywords_count,
                (unsigned char*) hashtags_buffer, MAX_BUFFER_LEN,
                &hashtags_len, &hashtags_count,
                (unsigned char*) keyphrases_buffer, MAX_BUFFER_LEN,
                &keyphrases_len, &keyphrases_count,
                (char*) text_classes_buffer, MAX_LIST_LEN,
                &text_classes_len, &text_classes_count,
                (char*) sentiment_buffer, MAX_CLASS_NAME)) < 0) {
    std::cerr << "ERROR: could not get keywords\n";
  } else {
    safe_status = std::string(safe_status_buffer);
    script = std::string(script_buffer);
    lang = std::string(lang_buffer);
    if (text_classes_len > 0 && text_classes_count > 0) {
      text_classes.insert(std::string(text_classes_buffer));
    }
    sentiment = std::string(sentiment_buffer);
#ifdef GIST_DEBUG
    std::cout << "safe_status: " << safe_status << std::endl;
    std::cout << "scrips: " << script << std::endl;
    std::cout << "lang: " << lang << std::endl;
    std::set<std::string>::iterator set_iter;
    std::cout << "text_classes: " << std::endl;
    for (set_iter = text_classes.begin(); set_iter != text_classes.end(); set_iter++) {
      std::cout << *set_iter << " ";
    }
    std::cout << std::endl;
#endif
  }

  return ret_value;
}

// keywords and keyphrases are output parameters
int GistMaker::GetGist(const unsigned char* tweet, const unsigned int tweet_len,
                char* safe_status_buffer, const unsigned int safe_status_buffer_len,
                char* script_buffer, const unsigned int script_buffer_len,
                char* lang_buffer, const unsigned int lang_buffer_len,
                unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
                unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr,
                unsigned char* hashtags_buffer, const unsigned int hashtags_buffer_len,
                unsigned int* hashtags_len_ptr, unsigned int* hashtags_count_ptr,
                unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
                unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr,
                char* text_classes_buffer, const unsigned int text_classes_buffer_len,
                unsigned int* text_classes_len_ptr, unsigned int* text_classes_count_ptr,
                char* sentiment_buffer, const unsigned int sentiment_buffer_len) {

#ifdef GIST_DEBUG
  std::cout << tweet << std::endl;
#endif

  *keywords_buffer = '\0';
  *keywords_len_ptr = 0;
  *keywords_count_ptr = 0;
  *hashtags_buffer = '\0';
  *hashtags_len_ptr = 0;
  *hashtags_count_ptr = 0;
  *keyphrases_buffer = '\0';
  *keyphrases_len_ptr = 0;
  *keyphrases_count_ptr = 0;
  *text_classes_buffer = '\0';
  *text_classes_len_ptr = 0;
  *text_classes_count_ptr = 0;
  *sentiment_buffer = '\0';

  // this can be global. keeping it local for the time being
  unsigned char buffer[MAX_BUFFER_LEN];
  unsigned int buffer_content_len = 0;
  unsigned int buffer_content_count = 0;
  if (tweet_len > 0 && tweet_len < MAX_BUFFER_LEN) {
    memcpy((char *) buffer, (char *) tweet, tweet_len);
    buffer[tweet_len] = '\0';
  } else {
    memset(buffer, '\0', MAX_BUFFER_LEN);
    return -1;
  }

  int ret_value = 0;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;

  unsigned char lang_words_buffer[MAX_BUFFER_LEN];
  lang_words_buffer[0] = '\0';
  unsigned int lang_words_buffer_len = MAX_BUFFER_LEN;
  unsigned int lang_words_len = 0;
  unsigned int lang_words_count = 0;

  ret_value = m_keytuples_extracter.GetKeyTuples(buffer, tweet_len,
                   safe_status_buffer, safe_status_buffer_len,
                   script_buffer, script_buffer_len,
                   keywords_buffer, keywords_buffer_len, keywords_len, keywords_count,
                   hashtags_buffer, hashtags_buffer_len, hashtags_len, hashtags_count,
                   keyphrases_buffer, keyphrases_buffer_len, keyphrases_len, keyphrases_count,
                   lang_words_buffer, lang_words_buffer_len, lang_words_len, lang_words_count);

  if (ret_value <= 0) {
    if (ret_value < 0 ) {
#ifdef GIST_DEBUG
      std::cout << "ERROR: could not get keywords from KeyTuplesExtracter\n";
      return -1;
#endif
    }
    *keywords_buffer = '\0';
    *keywords_len_ptr = 0;
    *keywords_count_ptr = 0;
    *hashtags_buffer = '\0';
    *hashtags_len_ptr = 0;
    *hashtags_count_ptr = 0;
    *keyphrases_buffer = '\0';
    *keyphrases_len_ptr = 0;
    *keyphrases_count_ptr = 0;
    return ret_value;
  } else {
    *keywords_len_ptr = keywords_len;
    *keywords_count_ptr = keywords_count;
    *hashtags_len_ptr = hashtags_len;
    *hashtags_count_ptr = hashtags_count;
    *keyphrases_len_ptr = keyphrases_len;
    *keyphrases_count_ptr = keyphrases_count;
  }

  if (m_language_detector.Classify(lang_words_buffer,
                                   lang_words_len,
                                   lang_words_count,
                                   lang_buffer,
                                   lang_buffer_len) < 0) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: could not detect language. assigning RR\n";
#endif
    strcpy(lang_buffer, "RR");
  }

  strcpy((char *) buffer, (char *) keywords_buffer);
  buffer_content_len = keywords_len;
  buffer_content_count = keywords_count;
  strcpy((char *) buffer + buffer_content_len, (char *) hashtags_buffer);
  buffer_content_len += hashtags_len;
  buffer_content_count += hashtags_count;
  buffer[buffer_content_len] = 0;

  int ret_val = 0;
  if ((ret_val = m_text_classifier.Classify(buffer,
                                 buffer_content_len,
                                 buffer_content_count,
                                 text_classes_buffer,
                                 text_classes_buffer_len)) < 0) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: could not find text class. assigning RR\n";
#endif
    strcpy(text_classes_buffer, "RR");
    *text_classes_count_ptr = 1;
    *text_classes_len_ptr = 2;
  } else if (ret_val == 0) {
    if (m_text_classifier.Classify(lang_words_buffer,
                                 lang_words_len,
                                 lang_words_count,
                                 text_classes_buffer,
                                 text_classes_buffer_len) < 0) {
#ifdef GIST_DEBUG
      std::cerr << "ERROR: could not find text class from lang_words either. assigning UU\n";
#endif
      strcpy(text_classes_buffer, "UU");
      *text_classes_count_ptr = 1;
      *text_classes_len_ptr = 2;
    }
  }
 
  if (ret_val > 0) {
    *text_classes_len_ptr = strlen(text_classes_buffer);
    if (*text_classes_len_ptr > 0) {
      *text_classes_count_ptr = 1;
      strcpy(text_classes_buffer + *text_classes_len_ptr, "|");
      *text_classes_len_ptr += 1;
#ifdef GIST_DEBUG
      if (GIST_DEBUG > 1)
        std::cout << "text_class: " << text_classes_buffer << std::endl;
#endif
    }
  }

  buffer[0] = '\0';
  lang_words_buffer[0] = '\0';

  return ret_value;
}

int GistMaker::FindTextClasses(inagist_classifiers::Corpus& corpus,
                char* text_classes_buffer, const unsigned int text_classes_buffer_len,
                unsigned int& text_classes_len, unsigned int& text_classes_count) {

  int ret_val = 0;

  text_classes_buffer[0] = '\0';
  text_classes_len = 0;
  text_classes_count = 0;

  std::string text_class;
  if ((ret_val = m_text_classifier.Classify(corpus, text_class)) < 0) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: could not find text class. assigning RR\n";
#endif
    strcpy(text_classes_buffer, "RR");
    text_classes_count = 1;
    text_classes_len = 2;
  } else {
    if ((text_classes_len = text_class.length()) > 0) {
      strcpy(text_classes_buffer, text_class.c_str());
      strcpy(text_classes_buffer + text_classes_len, "|");
      text_classes_len += 1;
      text_classes_count = 1;
    }
  }

  return ret_val;

}

} // namespace inagist_trends
