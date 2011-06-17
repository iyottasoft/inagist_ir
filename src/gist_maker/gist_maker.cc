/* gist_maker.cc */

#include "gist_maker.h"
#include "string_utils.h"

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
  std::map<std::string, std::string> text_class_contributors_map;
  std::string sentiment;

  if (GetGist(text, safe_status, script, lang,
              keywords, keyphrases, hashtags,
              text_classes,
              text_class_contributors_map,
              sentiment) < 0) {
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
    std::cout << "text_class_contributors: ";
    std::map<std::string, std::string>::iterator map_iter;
    for (map_iter = text_class_contributors_map.begin();
         map_iter != text_class_contributors_map.end();
         map_iter++) {
      std::cout << map_iter->first << " : " << map_iter->second << " | ";
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
                       std::set<std::string>& text_classes
#ifdef CLASS_CONTRIBUTORS_ENABLED
                       , std::map<std::string, std::string>& text_class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                       , std::string& sentiment) {

  if (text.length() < 1) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  unsigned char buffer[MAX_BUFFER_LEN];
  memset(buffer, 0, MAX_BUFFER_LEN);

  char safe_status_buffer[10];
  memset(safe_status_buffer, 0, 10);
  unsigned int safe_status_buffer_len = 10;

  char script_buffer[4];
  memset(script_buffer, 0, 4);
  unsigned int script_buffer_len = 10;

  char lang_buffer[4];
  memset(lang_buffer, 0, 4);
  unsigned int lang_buffer_len = 4;

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

#ifdef CLASS_CONTRIBUTORS_ENABLED
  unsigned char text_class_contributors_buffer[MAX_BUFFER_LEN];
  memset(text_class_contributors_buffer, 0, MAX_BUFFER_LEN);
  unsigned int text_class_contributors_len = 0;
  unsigned int text_class_contributors_count = 0;
#endif // CLASS_CONTRIBUTORS_ENABLED

  char sentiment_buffer[MAX_CLASS_NAME];
  memset(sentiment_buffer, 0, MAX_CLASS_NAME);

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
                &text_classes_len, &text_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                , (unsigned char *) text_class_contributors_buffer, ULTIMATE_BUFFER_LEN,
                &text_class_contributors_len, &text_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
                , (char*) sentiment_buffer, MAX_CLASS_NAME)) < 0) {
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
    std::cout << "text_classes: " << std::endl;
    std::set<std::string>::iterator set_iter;
    for (set_iter = text_classes.begin(); set_iter != text_classes.end(); set_iter++) {
      std::cout << *set_iter << " ";
    }
    std::cout << std::endl;
    std::cout << "text_class_contributors: (conversion not implemented yet!)";
    std::map<std::string, std::string>::iterator map_iter;
    for (map_iter = text_class_contributors_map.begin();
         map_iter != text_class_contributors_map.end();
         map_iter++) {
      std::cout << map_iter->first << " : " << map_iter->second << " | ";
    }
    std::cout << std::endl
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
      unsigned int* text_classes_len_ptr, unsigned int* text_classes_count_ptr
#ifdef CLASS_CONTRIBUTORS_ENABLED
      , unsigned char* text_class_contributors_buffer, const unsigned int text_class_contributors_buffer_len,
      unsigned int* text_class_contributors_len_ptr, unsigned int* text_class_contributors_count_ptr
#endif // CLASS_CONTRIBUTORS_ENABLED
      , char* sentiment_buffer, const unsigned int sentiment_buffer_len) {

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
  *text_class_contributors_buffer = '\0';
  *text_class_contributors_len_ptr = 0;
  *text_class_contributors_count_ptr = 0;
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

  unsigned char text_class_words_buffer[MAX_BUFFER_LEN];
  text_class_words_buffer[0] = '\0';
  unsigned int text_class_words_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_class_words_len = 0;
  unsigned int text_class_words_count = 0;

  ret_value = m_keytuples_extracter.GetKeyTuples(buffer, tweet_len,
                   safe_status_buffer, safe_status_buffer_len,
                   script_buffer, script_buffer_len,
                   keywords_buffer, keywords_buffer_len, keywords_len, keywords_count,
                   hashtags_buffer, hashtags_buffer_len, hashtags_len, hashtags_count,
                   keyphrases_buffer, keyphrases_buffer_len, keyphrases_len, keyphrases_count,
                   lang_words_buffer, lang_words_buffer_len, lang_words_len, lang_words_count,
                   text_class_words_buffer, text_class_words_buffer_len,
                   text_class_words_len, text_class_words_count);

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

  char top_lang_classes_buffer[MAX_BUFFER_LEN];
  top_lang_classes_buffer[0] = '\0';
  unsigned int top_lang_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int top_lang_classes_len = 0;
  unsigned int top_lang_classes_count = 0;
  unsigned char lang_class_contributors_buffer[MAX_BUFFER_LEN];
  lang_class_contributors_buffer[0] = '\0';
  unsigned int lang_class_contributors_buffer_len = MAX_BUFFER_LEN;
  unsigned int lang_class_contributors_len = 0;
  unsigned int lang_class_contributors_count = 0;

  if (m_language_detector.Classify(lang_words_buffer, lang_words_len, lang_words_count,
                                   lang_buffer, lang_buffer_len,
                                   top_lang_classes_buffer, top_lang_classes_buffer_len,
                                   top_lang_classes_len, top_lang_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                   , lang_class_contributors_buffer,
                                   lang_class_contributors_buffer_len,
                                   lang_class_contributors_len,
                                   lang_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
                                  ) < 0) {
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

  char text_class_buffer[MAX_BUFFER_LEN];
  text_class_buffer[0] = '\0';
  unsigned int text_class_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_classes_len = 0;
  unsigned int text_classes_count = 0;
  unsigned int text_class_contributors_len = 0;
  unsigned int text_class_contributors_count = 0;

  int ret_val = 0;
  if ((ret_val = m_text_classifier.Classify(buffer, buffer_content_len, buffer_content_count,
                                 text_class_buffer, text_class_buffer_len,
                                 text_classes_buffer, text_classes_buffer_len,
                                 text_classes_len, text_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                 , text_class_contributors_buffer,
                                 text_class_contributors_buffer_len,
                                 text_class_contributors_len,
                                 text_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
                                )) < 0) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: could not find text class. assigning RR\n";
#endif
  }
  *text_classes_len_ptr = text_classes_len;
  *text_classes_count_ptr = text_classes_count;
  *text_class_contributors_len_ptr = text_class_contributors_len;
  *text_class_contributors_count_ptr = text_class_contributors_count;

  buffer[0] = '\0';
  lang_words_buffer[0] = '\0';
  top_lang_classes_buffer[0] = '\0';
  text_class_buffer[0] = '\0';

  return ret_value;
}

int GistMaker::FindTextClasses(inagist_classifiers::Corpus& corpus,
      char* text_classes_buffer, const unsigned int text_classes_buffer_len,
      unsigned int& text_classes_len, unsigned int& text_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
      , unsigned char* text_class_contributors_buffer, const unsigned int& text_class_contributors_buffer_len,
      unsigned int& text_class_contributors_len, unsigned int& text_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
     ) {

  int ret_val = 0;

  text_classes_buffer[0] = '\0';
  text_classes_len = 0;
  text_classes_count = 0;

  std::string text_class;
  std::string top_classes;
  unsigned int top_classes_count = 0;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> text_class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED

  if ((ret_val = m_text_classifier.Classify(corpus, text_class,
                                            top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                            , text_class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                           )) < 0) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: could not find text class. assigning RR\n";
#endif
    strcpy(text_classes_buffer, top_classes.c_str());
    text_classes_len = top_classes.length();
  } else {
    if ((text_classes_len = top_classes.length()) > 0) {
      strcpy(text_classes_buffer, top_classes.c_str());
      text_classes_count = top_classes_count;
    }
#ifdef CLASS_CONTRIBUTORS_ENABLED
    if (text_class_contributors_map.empty()) {
#ifdef GIST_DEBUG
      std::cerr << "WARNING: no class_contributors found\n";
#endif
    } else {
      if (inagist_utils::StringMapToPipeList(text_class_contributors_map,
                            text_class_contributors_buffer, text_class_contributors_buffer_len,
                            text_class_contributors_len, text_class_contributors_count) < 0) {
#ifdef GIST_DEBUG
        std::cerr << "ERROR: could not make a list of class_contributors\n";
#endif
      }
    }
#endif // CLASS_CONTRIBUTORS_ENABLED
  }
  text_class_contributors_map.clear();

  return ret_val;

}

} // namespace inagist_trends
