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
#ifdef GIST_DEBUG
  m_debug_level = GIST_DEBUG;
#endif
}

GistMaker::~GistMaker() {
}

int GistMaker::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  return 0;
}

int GistMaker::Init(const char* keytuples_extracter_config_file
#ifdef LANG_ENABLED
                    , const char* language_detection_config_file
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                    , const char* text_classification_config_file
#endif // TEXT_CLASSIFICATION_ENABLED
                   ) {


  if (!keytuples_extracter_config_file) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif // GIST_DEBUG
    return -1;
  }

#ifdef LANG_ENABLED
  if (!language_detection_config_file) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif // GIST_DEBUG
    return -1;
  }
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  if (!text_classification_config_file) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: invalid input file name(s)\n";
#endif // GIST_DEBUG
    return -1;
  }
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef GIST_DEBUG
  if (m_debug_level > 2) {
    std::cout << "INFO: initializing keytuples_extracter with config: " \
              << keytuples_extracter_config_file << std::endl;
  }
#endif // GIST_DEBUG

  bool load_classifier_dictionary = false;
  if (m_keytuples_extracter.Init(keytuples_extracter_config_file, load_classifier_dictionary=false) < 0) {
    std::cerr << "ERROR: could not initialize KeyTuplesExtracter\n";
    return -1;
  }

#ifdef LANG_ENABLED
#ifdef GIST_DEBUG
  if (m_debug_level > 2) {
    std::cout << "INFO: initializing language_detector with config: " \
              << language_detection_config_file << std::endl;
  }
#endif // GIST_DEBUG
  if (m_language_detector.Init(language_detection_config_file) < 0) {
    std::cerr << "ERROR: could not initialize LanguageDetector\n";
    return -1;
  }
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
#ifdef GIST_DEBUG
  if (m_debug_level > 2) {
    std::cout << "INFO: initializing text_classifier with config: " \
              << text_classification_config_file << std::endl;
  }
#endif // GIST_DEBUG
  if (m_text_classifier.Init(text_classification_config_file) < 0) {
    std::cerr << "ERROR: could not initialize TextClassifier\n";
    return -1;
  }
#endif // TEXT_CLASSIFICATION_ENABLED

  return 0;
}

int GistMaker::GetGist(const std::string& text) {

  std::string safe_status;
  std::string script;
#ifdef LANG_ENABLED
  std::string lang;
#endif // LANG_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
  std::set<std::string> named_entities;
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYPHRASE_ENABLED
  std::set<std::string> keyphrases;
#endif // KEYPHRASE_ENABLED
#ifdef KEYWORDS_ENABLED
  std::set<std::string> keywords;
#endif // KEYWORDS_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  std::set<std::string> text_class_words;
  std::set<std::string> text_classes;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> text_class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
  std::string intent;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  std::string sentiment;
#endif // SENTIMENT_ENABLED

  if (GetGist(text, safe_status, script
#ifdef LANG_ENABLED
              , lang
#endif // LANG_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
              , named_entities
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
              , keywords
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
              , keyphrases
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              , text_class_words
              , text_classes
#ifdef CLASS_CONTRIBUTORS_ENABLED
              , text_class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
              , intent
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
              , sentiment
#endif // SENTIMENT_ENABLED
             ) < 0) {
    std::cout << "ERROR: could not get the gist for:" << text << std::endl;
  } else {
    std::cout << "text: " << text << std::endl;
    std::cout << "safe status: " << safe_status << std::endl;
    std::cout << "script: " << script << std::endl;
#ifdef LANG_ENABLED
    std::cout << "lang: " << lang << std::endl;
#endif // LANG_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
    std::set<std::string>::iterator set_iter;
    std::cout << "named_entities: ";
    for (set_iter = named_entities.begin(); set_iter != named_entities.end(); set_iter++) {
      std::cout << *set_iter << " | ";
    }
    std::cout << std::endl;
    named_entities.clear();
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
    std::cout << "keywords:  ";
    for (set_iter = keywords.begin(); set_iter != keywords.end(); set_iter++) {
      std::cout << *set_iter << " | ";
    }
    std::cout << std::endl;
    keywords.clear();
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
    std::cout << "keyphrases:  ";
    for (set_iter = keyphrases.begin(); set_iter != keyphrases.end(); set_iter++) {
      std::cout << *set_iter << " | ";
    }
    std::cout << std::endl;
    keyphrases.clear();
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    std::cout << "text_class_words: ";
    for (set_iter = text_class_words.begin(); set_iter != text_class_words.end(); set_iter++) {
      std::cout << *set_iter << " | ";
    }
    std::cout << std::endl;
    std::cout << "text_classes: ";
    for (set_iter = text_classes.begin(); set_iter != text_classes.end(); set_iter++) {
      std::cout << *set_iter << " | ";
    }
    std::cout << std::endl;
#ifdef CLASS_CONTRIBUTORS_ENABLED
    std::cout << "text_class_contributors: ";
    std::map<std::string, std::string>::iterator map_iter;
    for (map_iter = text_class_contributors_map.begin();
         map_iter != text_class_contributors_map.end();
         map_iter++) {
      std::cout << map_iter->first << " : " << map_iter->second << " | ";
    }
    std::cout << std::endl;
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
    std::cout << "intent: " << intent << std::endl;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
    std::cout << "sentiment: " << sentiment << std::endl;
#endif // SENTIMENT_ENABLED
    std::cout << std::endl;
  }
  return 0;
}

int GistMaker::GetGist(const std::string& text,
                       std::string& safe_status,
                       std::string& script
#ifdef LANG_ENABLED
                       , std::string& lang
#endif // LANG_ENABLED
#ifdef NAMED_ENTITIES_ENABLED
                       , std::set<std::string>& named_entities
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                       , std::set<std::string>& keywords
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                       , std::set<std::string>& keyphrases
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                       , std::set<std::string>& text_class_words
                       , std::set<std::string>& text_classes
#ifdef CLASS_CONTRIBUTORS_ENABLED
                       , std::map<std::string, std::string>& text_class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                       , std::string& intent
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                       , std::string& sentiment
#endif // SENTIMENT_ENABLED
                      ) {

  if (text.length() < 1) {
    std::cerr << "ERROR: invalid input text\n";
    return -1;
  }

  unsigned char text_buffer[MAX_BUFFER_LEN];
  memset(text_buffer, 0, MAX_BUFFER_LEN);
  strcpy((char*) text_buffer, text.c_str());
  unsigned int text_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_len = text.length();

  char safe_status_buffer[10];
  memset(safe_status_buffer, 0, 10);
  unsigned int safe_status_buffer_len = 10;

  char script_buffer[4];
  memset(script_buffer, 0, 4);
  unsigned int script_buffer_len = 10;

#ifdef NAMED_ENTITIES_ENABLED
  unsigned char named_entities_buffer[MAX_BUFFER_LEN];
  memset(named_entities_buffer, 0, MAX_BUFFER_LEN);
  unsigned int named_entities_buffer_len = MAX_BUFFER_LEN;
  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;
#endif // NAMED_ENTITIES_ENABLED

#ifdef KEYWORDS_ENABLED
  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  memset(keywords_buffer, 0, MAX_BUFFER_LEN);
  unsigned int keywords_buffer_len = MAX_BUFFER_LEN;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
#endif // KEYWORDS_ENABLED

#ifdef KEYPHRASE_ENABLED
  unsigned char keyphrases_buffer[MAX_BUFFER_LEN];
  memset(keyphrases_buffer, 0, MAX_BUFFER_LEN);
  unsigned int keyphrases_buffer_len = MAX_BUFFER_LEN;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
#endif // KEYPHRASE_ENABLED

#ifdef LANG_ENABLED
  unsigned char lang_class_words_buffer[MAX_BUFFER_LEN];
  memset(lang_class_words_buffer, 0, MAX_BUFFER_LEN);
  unsigned int lang_class_words_buffer_len = MAX_BUFFER_LEN;
  unsigned int lang_class_words_len = 0;
  unsigned int lang_class_words_count = 0;

  char lang_class_buffer[MAX_BUFFER_LEN];
  lang_class_buffer[0] = '\0';
  unsigned int lang_class_buffer_len = MAX_BUFFER_LEN;
  unsigned int lang_class_len = 0;
  unsigned int lang_class_count = 0;

  char top_lang_classes_buffer[MAX_BUFFER_LEN];
  top_lang_classes_buffer[0] = '\0';
  unsigned int top_lang_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int top_lang_classes_len = 0;
  unsigned int top_lang_classes_count = 0;

#ifdef CLASS_CONTRIBUTORS_ENABLED
  unsigned char lang_class_contributors_buffer[ULTIMATE_BUFFER_LEN];
  memset(lang_class_contributors_buffer, 0, ULTIMATE_BUFFER_LEN);
  unsigned int lang_class_contributors_buffer_len = ULTIMATE_BUFFER_LEN;
  unsigned int lang_class_contributors_len = 0;
  unsigned int lang_class_contributors_count = 0;
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  unsigned char text_class_words_buffer[MAX_BUFFER_LEN];
  memset(text_class_words_buffer, 0, MAX_BUFFER_LEN);
  unsigned int text_class_words_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_class_words_len = 0;
  unsigned int text_class_words_count = 0;

  char text_class_buffer[MAX_BUFFER_LEN];
  memset(text_class_buffer, 0, MAX_BUFFER_LEN);
  unsigned int text_class_buffer_len = MAX_BUFFER_LEN;
  unsigned int text_class_len = 0;
  unsigned int text_class_count = 0;

  char top_text_classes_buffer[MAX_BUFFER_LEN];
  top_text_classes_buffer[0] = '\0';
  unsigned int top_text_classes_buffer_len = MAX_BUFFER_LEN;
  unsigned int top_text_classes_len = 0;
  unsigned int top_text_classes_count = 0;

#ifdef CLASS_CONTRIBUTORS_ENABLED
  unsigned char text_class_contributors_buffer[ULTIMATE_BUFFER_LEN];
  memset(text_class_contributors_buffer, 0, ULTIMATE_BUFFER_LEN);
  unsigned int text_class_contributors_buffer_len = ULTIMATE_BUFFER_LEN;
  unsigned int text_class_contributors_len = 0;
  unsigned int text_class_contributors_count = 0;
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef INTENT_ENABLED
  char intent_buffer[MAX_CLASS_NAME];
  memset(intent_buffer, 0, MAX_CLASS_NAME);
  unsigned int intent_buffer_len = MAX_CLASS_NAME;
#endif // INTENT_ENABLED

#ifdef SENTIMENT_ENABLED
  char sentiment_buffer[MAX_CLASS_NAME];
  memset(sentiment_buffer, 0, MAX_CLASS_NAME);
  unsigned int sentiment_buffer_len = MAX_CLASS_NAME;
#endif // SENTIMENT_ENABLED

  int ret_value = 0;
  if ((ret_value = GetGist((unsigned char*) text_buffer, text_buffer_len, text_len,
                (char*) safe_status_buffer, safe_status_buffer_len,
                (char*) script_buffer, script_buffer_len
#ifdef NAMED_ENTITIES_ENABLED
                , (unsigned char*) named_entities_buffer, named_entities_buffer_len,
                &named_entities_len, &named_entities_count
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                , (unsigned char*) keywords_buffer, keywords_buffer_len,
                &keywords_len, &keywords_count
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                , (unsigned char*) keyphrases_buffer, keyphrases_buffer_len,
                &keyphrases_len, &keyphrases_count
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                , (unsigned char*) lang_class_words_buffer, lang_class_words_buffer_len,
                &lang_class_words_len, &lang_class_words_count,
                (char*) lang_class_buffer, lang_class_buffer_len,
                &lang_class_len, &lang_class_count,
                (char*) top_lang_classes_buffer, top_lang_classes_buffer_len,
                &top_lang_classes_len, &top_lang_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                , (unsigned char *) lang_class_contributors_buffer, lang_class_contributors_buffer_len,
                &lang_class_contributors_len, &lang_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                , (unsigned char*) text_class_words_buffer, text_class_words_buffer_len,
                &text_class_words_len, &text_class_words_count,
                (char*) text_class_buffer, text_class_buffer_len,
                &text_class_len, &text_class_count,
                (char*) top_text_classes_buffer, top_text_classes_buffer_len,
                &top_text_classes_len, &top_text_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                , (unsigned char *) text_class_contributors_buffer, text_class_contributors_buffer_len,
                &text_class_contributors_len, &text_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                , (char *) intent_buffer, intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                , (char *) sentiment_buffer, sentiment_buffer_len
#endif // SENTIMENT_ENABLED
               )) < 0) {
    std::cerr << "ERROR: could not get gist\n";
  } else {
    safe_status = std::string(safe_status_buffer);
    script = std::string(script_buffer);
#ifdef NAMED_ENTITIES_ENABLED
    inagist_utils::PipeListToSet(named_entities_buffer, named_entities);
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYPHRASE_ENABLED
    inagist_utils::PipeListToSet(keyphrases_buffer, keyphrases);
#endif // KEYPHRASE_ENABLED
#ifdef KEYWORDS_ENABLED
    inagist_utils::PipeListToSet(keywords_buffer, keywords);
#endif // KEYWORDS_ENABLED
#ifdef LANG_ENABLED
    lang = std::string(lang_class_buffer);
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    inagist_utils::PipeListToSet(text_class_words_buffer, text_class_words);
    if (top_text_classes_len > 0 && top_text_classes_count > 0) {
      text_classes.insert(std::string(top_text_classes_buffer));
    }
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
    if (strlen(intent_buffer) > 0) {
      intent = std::string(intent_buffer);
    } else {
      intent.clear();
    }
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
    if (strlen(sentiment_buffer) > 0) {
      sentiment = std::string(sentiment_buffer);
    } else {
      sentiment.clear();
    }
#endif // SENTIMENT_ENABLED
  }

  return ret_value;
}

// named_entities and keyphrases are output parameters
int GistMaker::GetGist(unsigned char* text_buffer,  const unsigned int text_buffer_len,
      const unsigned int text_len,
      char* safe_status_buffer, const unsigned int safe_status_buffer_len,
      char* script_buffer, const unsigned int script_buffer_len
#ifdef NAMED_ENTITIES_ENABLED
      , unsigned char* named_entities_buffer, const unsigned int named_entities_buffer_len,
      unsigned int* named_entities_len_ptr, unsigned int* named_entities_count_ptr
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
      , unsigned char* keywords_buffer, const unsigned int keywords_buffer_len,
      unsigned int* keywords_len_ptr, unsigned int* keywords_count_ptr
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
      , unsigned char* keyphrases_buffer, const unsigned int keyphrases_buffer_len,
      unsigned int* keyphrases_len_ptr, unsigned int* keyphrases_count_ptr
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
      , unsigned char* lang_class_words_buffer, const unsigned int lang_class_words_buffer_len,
      unsigned int* lang_class_words_len_ptr, unsigned int* lang_class_words_count_ptr,
      char* lang_class_buffer, const unsigned int lang_class_buffer_len,
      unsigned int* lang_class_len_ptr, unsigned int* lang_class_count_ptr,
      char* top_lang_classes_buffer, const unsigned int top_lang_classes_buffer_len,
      unsigned int* top_lang_classes_len_ptr, unsigned int* top_lang_classes_count_ptr
#ifdef CLASS_CONTRIBUTORS_ENABLED
      , unsigned char* lang_class_contributors_buffer, const unsigned int lang_class_contributors_buffer_len,
      unsigned int* lang_class_contributors_len_ptr, unsigned int* lang_class_contributors_count_ptr
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
      , unsigned char* text_class_words_buffer, const unsigned int text_class_words_buffer_len,
      unsigned int* text_class_words_len_ptr, unsigned int* text_class_words_count_ptr,
      char* text_class_buffer, const unsigned int text_class_buffer_len,
      unsigned int* text_class_len_ptr, unsigned int* text_class_count_ptr,
      char* top_text_classes_buffer, const unsigned int top_text_classes_buffer_len,
      unsigned int* top_text_classes_len_ptr, unsigned int* top_text_classes_count_ptr
#ifdef CLASS_CONTRIBUTORS_ENABLED
      , unsigned char* text_class_contributors_buffer, const unsigned int text_class_contributors_buffer_len,
      unsigned int* text_class_contributors_len_ptr, unsigned int* text_class_contributors_count_ptr
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
      , char* intent_buffer, const unsigned int intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
      , char* sentiment_buffer, const unsigned int sentiment_buffer_len
#endif // SENTIMENT_ENABLED
     ) {

#ifdef NAMED_ENTITIES_ENABLED
  *named_entities_buffer = '\0';
  *named_entities_len_ptr = 0;
  *named_entities_count_ptr = 0;
#endif // NAMED_ENTITIES_ENABLED

#ifdef KEYWORDS_ENABLED
  *keywords_buffer = '\0';
  *keywords_len_ptr = 0;
  *keywords_count_ptr = 0;
#endif // KEYWORDS_ENABLED

#ifdef KEYPHRASE_ENABLED
  *keyphrases_buffer = '\0';
  *keyphrases_len_ptr = 0;
  *keyphrases_count_ptr = 0;
#endif // KEYPHRASE_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  *text_class_words_buffer = '\0';
  *text_class_words_len_ptr = 0;
  *text_class_words_count_ptr = 0;
  *text_class_buffer = '\0';
  *text_class_len_ptr = 0;
  *text_class_count_ptr = 0;
  *top_text_classes_buffer = '\0';
  *top_text_classes_len_ptr = 0;
  *top_text_classes_count_ptr = 0;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  *text_class_contributors_buffer = '\0';
  *text_class_contributors_len_ptr = 0;
  *text_class_contributors_count_ptr = 0;
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LANG_ENABLED
  *lang_class_words_buffer = '\0';
  *lang_class_words_len_ptr = 0;
  *lang_class_words_count_ptr = 0;
  *lang_class_buffer = '\0';
  *lang_class_len_ptr = 0;
  *lang_class_count_ptr = 0;
  *top_lang_classes_buffer = '\0';
  *top_lang_classes_len_ptr = 0;
  *top_lang_classes_count_ptr = 0;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  *lang_class_contributors_buffer = '\0';
  *lang_class_contributors_len_ptr = 0;
  *lang_class_contributors_count_ptr = 0;
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // LANG_ENABLED
#ifdef INTENT_ENABLED
  *intent_buffer = '\0';
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  *sentiment_buffer = '\0';
#endif // SENTIMENT_ENABLED

  int ret_value = 0;
#ifdef NAMED_ENTITIES_ENABLED
  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  unsigned int text_class_words_len = 0;
  unsigned int text_class_words_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
  unsigned int lang_class_words_len = 0;
  unsigned int lang_class_words_count = 0;
#endif // LANG_ENABLED

#ifdef GIST_DEBUG
  std::cout << "calling GetKeytuples" << std::endl;
#endif // GIST_DEBUG
  ret_value = m_keytuples_extracter.GetKeyTuples(text_buffer, text_buffer_len, text_len,
                   safe_status_buffer, safe_status_buffer_len,
                   script_buffer, script_buffer_len
#ifdef NAMED_ENTITIES_ENABLED
                   , named_entities_buffer, named_entities_buffer_len, named_entities_len, named_entities_count
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                   , keywords_buffer, keywords_buffer_len, keywords_len, keywords_count
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                   , keyphrases_buffer, keyphrases_buffer_len, keyphrases_len, keyphrases_count
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                   , lang_class_words_buffer, lang_class_words_buffer_len,
                   lang_class_words_len, lang_class_words_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                   , text_class_words_buffer, text_class_words_buffer_len,
                   text_class_words_len, text_class_words_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                   , intent_buffer, intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                  , sentiment_buffer, sentiment_buffer_len
#endif // SENTIMENT_ENABLED
                 );

  if (ret_value <= 0) {
    if (ret_value < 0 ) {
#ifdef GIST_DEBUG
      std::cout << "ERROR: could not get named_entities from KeyTuplesExtracter\n";
      return -1;
#endif // GIST_DEBUG
    }
#ifdef GIST_DEBUG
    std::cout << "WARNING: no keytuples found\n";
#endif // GIST_DEBUG
    return ret_value;
  } else {
#ifdef NAMED_ENTITIES_ENABLED
    *named_entities_len_ptr = named_entities_len;
    *named_entities_count_ptr = named_entities_count;
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
    *keywords_len_ptr = keywords_len;
    *keywords_count_ptr = keywords_count;
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
    *keyphrases_len_ptr = keyphrases_len;
    *keyphrases_count_ptr = keyphrases_count;
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
    *lang_class_words_len_ptr = lang_class_words_len;
    *lang_class_words_count_ptr = lang_class_words_count;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    *text_class_words_len_ptr = text_class_words_len;
    *text_class_words_count_ptr = text_class_words_count;
#endif // TEXT_CLASSIFICATION_ENABLED
  }

#ifdef LANG_ENABLED
  unsigned int top_lang_classes_len = 0;
  unsigned int top_lang_classes_count = 0;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  unsigned int lang_class_contributors_len = 0;
  unsigned int lang_class_contributors_count = 0;
#endif // CLASS_CONTRIBUTORS_ENABLED

  if (strcmp(script_buffer, "en") != 0) {
    strcpy(lang_class_buffer, script_buffer);
  } else {
    if (m_language_detector.Classify(lang_class_words_buffer, lang_class_words_len, lang_class_words_count,
                                     lang_class_buffer, lang_class_buffer_len,
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
      strcpy(lang_class_buffer, "RR");
#endif // GIST_DEBUG
    } else {
#ifdef CLASS_CONTRIBUTORS_ENABLED
      *lang_class_contributors_len_ptr = lang_class_contributors_len;
      *lang_class_contributors_count_ptr = lang_class_contributors_count;
#endif // CLASS_CONTRIBUTORS_ENABLED
    }
  }
#endif // LANG_ENABLED

  if (strcmp(lang_class_buffer, "en") != 0) {
#ifdef NAMED_ENTITIES_ENABLED
    *named_entities_buffer = '\0';
    *named_entities_len_ptr = 0;
    *named_entities_count_ptr = 0;
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
    *keywords_buffer = '\0';
    *keywords_len_ptr = 0;
    *keywords_count_ptr = 0;
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
    *keyphrases_buffer = '\0';
    *keyphrases_len_ptr = 0;
    *keyphrases_count_ptr = 0;
#endif // KEYPHRASE_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    *text_class_words_buffer = '\0';
    *text_class_words_len_ptr = 0;
    *text_class_words_count_ptr = 0;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
    *intent_buffer = '\0';
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
    *sentiment_buffer = '\0';
#endif // SENTIMENT_ENABLED
  } else {
#ifdef TEXT_CLASSIFICATION_ENABLED
    unsigned int top_text_classes_len = 0;
    unsigned int top_text_classes_count = 0;
#ifdef CLASS_CONTRIBUTORS_ENABLED
    unsigned int text_class_contributors_len = 0;
    unsigned int text_class_contributors_count = 0;
#endif // CLASS_CONTRIBUTORS_ENABLED

    int ret_val = 0;
    if ((ret_val = m_text_classifier.Classify(text_class_words_buffer, text_class_words_len, text_class_words_count,
                                   text_class_buffer, text_class_buffer_len,
                                   top_text_classes_buffer, top_text_classes_buffer_len,
                                   top_text_classes_len, top_text_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                   , text_class_contributors_buffer,
                                   text_class_contributors_buffer_len,
                                   text_class_contributors_len,
                                   text_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
                                  )) < 0) {
#ifdef GIST_DEBUG
      std::cerr << "ERROR: could not find text class. assigning RR\n";
#endif // GIST_DEBUG
    } else {
      *top_text_classes_len_ptr = top_text_classes_len;
      *top_text_classes_count_ptr = top_text_classes_count;
#ifdef CLASS_CONTRIBUTORS_ENABLED
      *text_class_contributors_len_ptr = text_class_contributors_len;
      *text_class_contributors_count_ptr = text_class_contributors_count;
#endif // CLASS_CONTRIBUTORS_ENABLED
    }
#endif // TEXT_CLASSIFICATION_ENABLED
  } // check for english

#ifdef GIST_DEBUG
  std::cout << "text: " << text_buffer << std::endl;
  std::cout << "safe_status: " << safe_status_buffer << std::endl;
  std::cout << "script: " << script_buffer << std::endl;
#ifdef LANG_ENABLED
  std::cout << "lang: " << lang_class_buffer << std::endl;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  std::cout << "top_text_classes: " << top_text_classes_buffer << std::endl;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::cout << "text_class_contributors: " << text_class_contributors_buffer << std::endl;
#endif // CLASS_CONTRIBUTORS_ENABLED
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
  std::cout << "intent: ";
  if (intent_buffer) {
    std::cout << intent_buffer;
  }
  std::cout << std::endl;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  std::cout << "sentiment: ";
  if (sentiment_buffer) {
    std::cout << sentiment_buffer;
  }
  std::cout << std::endl;
#endif // SENTIMENT_ENABLED
  std::cout << std::endl;
#endif // GIST_DEBUG

  return ret_value;
}

#ifdef TEXT_CLASSIFICATION_ENABLED
int GistMaker::FindTextClasses(inagist_classifiers::Corpus& corpus,
      char* text_classes_buffer, const unsigned int text_classes_buffer_len,
      unsigned int& text_classes_len, unsigned int& text_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
      , unsigned char* text_class_contributors_buffer, const unsigned int& text_class_contributors_buffer_len,
      unsigned int& text_class_contributors_len, unsigned int& text_class_contributors_count
#endif // CLASS_CONTRIBUTORS_ENABLED
     ) {

  if (!text_classes_buffer) {
    std::cerr << "ERROR: invalid text_classes_buffer\n";
    return -1;
  }
  text_classes_buffer[0] = '\0';
  text_classes_len = 0;
  text_classes_count = 0;

#ifdef CLASS_CONTRIBUTORS_ENABLED
  if (!text_class_contributors_buffer) {
    std::cerr << "ERROR: invalid text_class_contributors_buffer\n";
    return -1;
  }
  text_class_contributors_buffer[0] = '\0';
  text_class_contributors_count = 0;
  text_class_contributors_len = 0;
#endif // CLASS_CONTRIBUTORS_ENABLED

  if (corpus.empty()) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: empty corpus. cannot find text classes\n";
#endif
    return -1;
  }

  std::string text_class;
  std::string top_classes;
  unsigned int top_classes_count = 0;
#ifdef CLASS_CONTRIBUTORS_ENABLED
  std::map<std::string, std::string> text_class_contributors_map;
#endif // CLASS_CONTRIBUTORS_ENABLED

  int ret_val = 0;
  if ((ret_val = m_text_classifier.Classify(corpus, text_class,
                                            top_classes, top_classes_count
#ifdef CLASS_CONTRIBUTORS_ENABLED
                                            , text_class_contributors_map
#endif // CLASS_CONTRIBUTORS_ENABLED
                                           )) < 0) {
#ifdef GIST_DEBUG
    std::cerr << "ERROR: could not find text class. assigning RR\n";
#endif
  } else {
    if ((text_classes_len = top_classes.length()) > 0) {
      strcpy(text_classes_buffer, top_classes.c_str());
      text_classes_count = top_classes_count;
    }
#ifdef CLASS_CONTRIBUTORS_ENABLED
    if (text_class_contributors_map.empty()) {
#ifdef GIST_DEBUG
      if (m_debug_level > 1) {
        std::cerr << "WARNING: no class_contributors found\n";
      }
#endif // GIST_DEBUG
    } else {
      if (inagist_utils::StringMapToPipeList(text_class_contributors_map,
                            text_class_contributors_buffer, text_class_contributors_buffer_len,
                            text_class_contributors_len, text_class_contributors_count) < 0) {
#ifdef GIST_DEBUG
        std::cerr << "ERROR: could not make a list of class_contributors\n";
#endif // GIST_DEBUG
      } else {
#ifdef GIST_DEBUG
        if (m_debug_level > 2) {
          std::cout << "INFO: " << text_class_contributors_count \
                    << "strings from map written to piped list\n";
        }
#endif // GIST_DEBUG
      }
    }
#endif // CLASS_CONTRIBUTORS_ENABLED
  }
#ifdef CLASS_CONTRIBUTORS_ENABLED
  text_class_contributors_map.clear();
#endif // CLASS_CONTRIBUTORS_ENABLED

  return ret_val;

}
#endif // TEXT_CLASSIFICATION_ENABLED

} // namespace inagist_trends
