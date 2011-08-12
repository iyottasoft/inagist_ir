#include "keytuples_extracter.h"
#include "script_detector_utils.h"
#include <cstring>
#include "utf8.h"
#include "keytuples_config.h"

#define MAX_DEBUG_BUFFER_LEN 1024
//#define I18N_ENABLED 0

extern int DetectScript(int code_point, std::string &script);

namespace inagist_trends {
  // unless otherwise specified functions return 0 or NULL or false as default
  // return values less than 0 are likely error codes

using std::cout;
using std::endl;
using std::string;

KeyTuplesExtracter::KeyTuplesExtracter() {
}

KeyTuplesExtracter::~KeyTuplesExtracter() {
  if (DeInit() < 0)
    std::cerr << "ERROR: DeInit() failed\n";
}

int KeyTuplesExtracter::Init(std::string config_file,
                             bool load_classifier_dictionary) {

/*
  if (config_file.size() < 5) {
    std::cerr << "ERROR: invalid config file name\n";
    return -1;
  }
*/

  inagist_trends::Config config;
  if (inagist_trends::KeyTuplesConfig::Read(config_file.c_str(), config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file << std::endl;
    return -1;
  }

  if (Init(config.stopwords_file.c_str(),
           config.dictionary_file.c_str(),
           config.unsafe_dictionary_file.c_str(),
#ifdef INTENT_ENABLED
           config.intent_words_file.c_str(),
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
           config.sentiment_words_file.c_str(),
#endif // SENTIMENT_ENABLED
           config.stemmer_dictionary_file.c_str()) < 0) {
    std::cerr << "ERROR: could not initialize KeyTuplesExtracter\n";
    return -1;
  }

  if (load_classifier_dictionary) {
    LoadClassifierDictionary(config.classifier_dictionary_file.c_str());
  }

  inagist_trends::KeyTuplesConfig::Clear(config);

  return 0;
}

int KeyTuplesExtracter::LoadClassifierDictionary(const char* classifier_dictionary_file) {
  int ret = m_dictionary.Load(classifier_dictionary_file);
  if (ret < 0) {
    std::cerr << "ERROR: could not load classifier dictionary file: " \
              << classifier_dictionary_file;
    return -1;
  }
  return 0;
}

// every input parameter is optional!
//
// stopwords, dictionary, intent-words and stemmer dictionary file paths, if not given will
// just mean that those dictionaries will not be populated
// 
// if input_file or output_file are given, they will be initialized
// the above two are typically used by a test program which will
// subsequently call GetKeyTuples(), GetKeywords and PrintKeywords()
//
int KeyTuplesExtracter::Init(const char *stopwords_file,
    const char *dictionary_file,
    const char *unsafe_dictionary_file,
#ifdef INTENT_ENABLED
    const char *intent_words_file,
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
    const char *sentiment_words_file,
#endif // SENTIMENT_ENABLED
    const char *stemmer_dictionary_file,
    const char *input_file,
    const char *output_file) {

  // load dictionaries
  if (stopwords_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: loading stopwords file - " << stopwords_file << std::endl;
    }
#endif // KE_DEBUG
    int ret = m_stopwords_dictionary.Load(stopwords_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load stopwords file " \
                << stopwords_file << " into dictionary set\n";
      return -1;
    }
  }

  if (dictionary_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: loading dictionary file - " << dictionary_file << std::endl;
    }
#endif // KE_DEBUG
    int ret = m_dictionary.Load(dictionary_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load dictionary file " \
                << dictionary_file << " into dictionary set\n";
      return -1;
    }
  }

  if (unsafe_dictionary_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: loading unsafe dictionary file - " << unsafe_dictionary_file << std::endl;
    }
#endif // KE_DEBUG
    int ret = m_unsafe_dictionary.Load(unsafe_dictionary_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load unsafe file " \
                << unsafe_dictionary_file << " into dictionary set\n";
      return -1;
    }
  }

#ifdef INTENT_ENABLED
  if (intent_words_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: loading intent words file - " << intent_words_file << std::endl;
    }
#endif // KE_DEBUG
    int ret = m_intent_words_dictionary.Load(intent_words_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load intent words file " \
                << intent_words_file << " into dictionary map\n";
      return -1;
    }
  }
#endif // INTENT_ENABLED

#ifdef SENTIMENT_ENABLED
  if (sentiment_words_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: loading sentiment words file - " << sentiment_words_file << std::endl;
    }
#endif // KE_DEBUG
    int ret = m_sentiment_words_dictionary.Load(sentiment_words_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load sentiment words file: " \
                << sentiment_words_file << " into dictionary map\n";
      return -1;
    }
  }
#endif // SENTIMENT_ENABLED

  if (input_file) {
    m_tweet_stream.open(input_file, std::ifstream::in);

    m_tweet_stream.seekg(0, std::ios::end);
    unsigned int length = m_tweet_stream.tellg();
    if (length < 2) {
      std::cerr << "ERROR: empty file\n";
      m_tweet_stream.close();
    }
  }

  // open output file
  if (output_file) {
    std::ofstream m_out_stream(output_file, std::ofstream::out);
  }
  return 0;
}

int KeyTuplesExtracter::DeInit() {
  //std::cout << "closing input stream\n";
  if (m_tweet_stream && m_tweet_stream.is_open())
    m_tweet_stream.close();

  //std::cout << "closing output stream\n";
  if (m_out_stream && m_out_stream.is_open())
    m_out_stream.close();

  //std::cout << "deinit done\n";
  return 0;
}

void KeyTuplesExtracter::PrintKeywords(std::set<std::string> &named_entities_set) {
  std::set<std::string>::iterator iter;

  if (!named_entities_set.empty()) {
    for (iter = named_entities_set.begin(); iter != named_entities_set.end(); iter++)
      std::cout << *iter << "|"; 
    std::cout << std::endl;
  }
}

// this function isn't unicode safe
// TODO (balaji) for ascii, we can ofcourse use an array lookup to speed up
bool KeyTuplesExtracter::IsPunct(char*& ptr, char* prev, char* next/*, bool word_has_apostrophe*/) {

  if (!ptr || *ptr == ' ' || *ptr == '\0')
    return true;

  if (!ispunct(*ptr))
    return false;

  switch (*ptr) {
    case '\'':
      if (!prev || IsPunct(prev)) {
        return true;
      }
      if (!next || IsPunct(next)) {
        return true;
      }
      if (strncmp(ptr, "'s ", 3) == 0) {
        // its callers responsibility to initialize this to false
        //word_has_apostrophe = true;
        ptr += 2;
        return true;
      } else {
        return false;
      }
      /*
      if (!strncmp(ptr, "'t", 2) || !strncmp(ptr, "'ve", 2) ||
          !strncmp(ptr, "'ll", 2) || !strncmp(ptr, "'re", 2) || !strncmp(ptr, "'m", 2) ||
          !strncmp(ptr, "'em", 3))
       return false;
      */
      break;
    case '@':
      if (prev && !IsPunct(prev))
        return true;
      return IsPunct(next);
      break;
    case '#':
      if (!next)
        return true;
      else
        if (*next == ' ' || IsPunct(next))
          return true;
      //if (prev)
      //  if (*prev != ' ' && *prev != '\0' && IsPunct(prev))
      //   return true;
      return false;
      break;
    case '-':
      if (prev && next)
        if (isalnum(*prev) && (isalnum(*next)))
          return false;
      break;
    case ':':
    case ',':
      if (prev && next)
        if (isdigit(*prev) && isdigit(*next))
          return false;
      break;
    case '.':
      if (prev && next)
        if (isdigit(*prev) && isdigit(*next))
          return false;
      if (next) {
        if (*next == ' ')
          return true;
        if (!strncmp(ptr, ".com", 4) || !strncmp(ptr, ".org", 4))
          return false; // not handling .come on or .organization etc
      }
      break;
    case '&':
      return true;
      if (next) {
        //if (*next == '#' && isdigit(*(next+1)))
        if (*next != ' ')
          return false;
      }
      break;
    case '_':
      return false;
    default:
      break;
  }

  return true;
}

bool KeyTuplesExtracter::IsIgnore(char *&ptr) {
  if (!ptr || '\0' == *ptr)
    return false;
  // anyword with starts with punct (except #) is ignore word
  if ((ispunct(*ptr) && '#' != *ptr && ' ' != *(ptr+1)) ||
      !strncmp(ptr, "#fb", 3) ||
      !strncmp(ptr, "#FB", 3) ||
      !strncmp(ptr, "FB ", 3) ||
      !strncmp(ptr, "fb ", 3) ||
      !strncmp(ptr, "http://", 7) ||
      !strncmp(ptr, "www.", 4) ||
      !strncmp(ptr, "RT ", 3)) {
    while (' ' != *(ptr+1) && '\0' != *(ptr+1)) {
      ptr++;
    }
    return true;
  }
  return false;
}

#ifdef NAMED_ENTITIES_ENABLED
int KeyTuplesExtracter::GetKeywords(char* str,
                                  std::string& safe_status,
                                  std::string& script,
                                  std::set<std::string>& named_entities_set) {

#ifdef KEYPHRASE_ENABLED
  std::set<std::string> keyphrases_set;
#endif // KEYPHRASE_ENABLED
#ifdef KEYWORDS_ENABLED
  std::set<std::string> keywords_set;
#endif // KEYWORDS_ENABLED
#ifdef LANG_ENABLED
  std::set<std::string> lang_words_set;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  std::set<std::string> text_class_words_set;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
  std::string intent;
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
  std::string sentiment;
#endif // SENTIMENT_ENABLED

  int ret_val = GetKeyTuples(str, safe_status, script, named_entities_set
#ifdef KEYWORDS_ENABLED
                             , keywords_set
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                             , keyphrases_set
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                             , lang_words_set
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                             , text_class_words_set
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                             , intent
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                             , sentiment
#endif // SENTIMENT_ENABLED
                            );

#ifdef KEYPHRASE_ENABLED
  keyphrases_set.clear();
#endif // KEYPHRASE_ENABLED
#ifdef KEYWORDS_ENABLED
  keywords_set.clear();
#endif // KEYWORDS_ENABLED
#ifdef LANG_ENABLED
  lang_words_set.clear();
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
  text_class_words_set.clear();
#endif // TEXT_CLASSIFICATION_ENABLED

  return ret_val;

}
#endif // NAMED_ENTITIES_ENABLED

#ifdef NAMED_ENTITIES_ENABLED
int KeyTuplesExtracter::GetKeywords(char* str,
                                   std::string& user,
                                   std::set<std::string>& named_entities_set,
                                   std::map<std::string, std::string>& script_user_map,
                                   std::map<std::string, std::string>& keyword_user_map) {
  std::string safe_status;
  std::string script;
  if (GetKeywords(str, safe_status, script, named_entities_set) < 0) {
    cout << "ERROR: could not get named_entities\n";
    return -1;
  }

  std::map<std::string, std::string>::iterator map_iter;
  if (script != "en") {
    if ((map_iter = script_user_map.find(script)) != script_user_map.end()) {
      script_user_map[script]+= ", " + user;
    } else {
      script_user_map[script] = user;
    }
  }
  std::set<std::string>::iterator set_iter;
  for (set_iter = named_entities_set.begin(); set_iter != named_entities_set.end(); set_iter++) {
    if ((map_iter = keyword_user_map.find(*set_iter)) != keyword_user_map.end()) {
      keyword_user_map[*set_iter]+= ", " + user;
    } else {
      keyword_user_map[*set_iter] = user;
    }
  }

  return 0;
}
#endif // NAMED_ENTITIES_ENABLED

int KeyTuplesExtracter::GetKeyTuples(char* str, 
                                     std::string& safe_status,
                                     std::string& script

#ifdef NAMED_ENTITIES_ENABLED
                                     , std::set<std::string>& named_entities_set
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                                     , std::set<std::string>& keywords_set
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                                     , std::set<std::string>& keyphrases_set
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                                     , std::set<std::string>& lang_words_set
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                                     , std::set<std::string>& text_class_words_set
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                                     , std::string& intent
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                                     , std::string& sentiment
#endif // SENTIMENT_ENABLED
                                    ) {

  unsigned int text_len = strlen(str);
  if (text_len > MAX_DEBUG_BUFFER_LEN) {
    std::cout << "ERROR: invalid input size\n";
    return -1;
  }

  unsigned char text_buffer[MAX_DEBUG_BUFFER_LEN];
  unsigned int text_buffer_len = MAX_DEBUG_BUFFER_LEN;
  memset(text_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  strcpy((char *) text_buffer, str);

  char safe_status_buffer[10];
  memset(safe_status_buffer, '\0', 10);
  unsigned int safe_status_buffer_len = 10;

  char script_buffer[4];
  unsigned int script_buffer_len = 4;
  memset(script_buffer, '\0', 4);

#ifdef NAMED_ENTITIES_ENABLED
  unsigned char named_entities_buffer[MAX_DEBUG_BUFFER_LEN];
  memset(named_entities_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  unsigned int named_entities_buffer_len = MAX_DEBUG_BUFFER_LEN;
  unsigned int named_entities_len = 0;
  unsigned int named_entities_count = 0;
#endif

#ifdef KEYWORDS_ENABLED
  unsigned char keywords_buffer[MAX_DEBUG_BUFFER_LEN];
  memset(keywords_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  unsigned int keywords_buffer_len = MAX_DEBUG_BUFFER_LEN;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
#endif

#ifdef KEYPHRASE_ENABLED
  unsigned char keyphrases_buffer[MAX_DEBUG_BUFFER_LEN];
  memset(keyphrases_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  unsigned int keyphrases_buffer_len = MAX_DEBUG_BUFFER_LEN;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
#endif

#ifdef LANG_ENABLED
  unsigned char lang_words_buffer[MAX_DEBUG_BUFFER_LEN];
  unsigned int lang_words_buffer_len = MAX_DEBUG_BUFFER_LEN;
  memset(lang_words_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  unsigned int lang_words_len = 0;
  unsigned int lang_words_count = 0;
#endif

#ifdef TEXT_CLASSIFICATION_ENABLED
  unsigned char text_class_words_buffer[MAX_DEBUG_BUFFER_LEN];
  unsigned int text_class_words_buffer_len = MAX_DEBUG_BUFFER_LEN;
  memset(text_class_words_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  unsigned int text_class_words_len = 0;
  unsigned int text_class_words_count = 0;
#endif

#ifdef INTENT_ENABLED
  char intent_buffer[MAX_DEBUG_BUFFER_LEN];
  unsigned int intent_buffer_len = MAX_DEBUG_BUFFER_LEN;
  memset(intent_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
#endif // INTENT_ENABLED

#ifdef SENTIMENT_ENABLED
  char sentiment_buffer[MAX_DEBUG_BUFFER_LEN];
  unsigned int sentiment_buffer_len = MAX_DEBUG_BUFFER_LEN;
  memset(sentiment_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
#endif // SENTIMENT_ENABLED

  int count = 0;

  if ((count = GetKeyTuples(text_buffer, text_buffer_len, text_len,
                  safe_status_buffer, safe_status_buffer_len,
                  script_buffer, script_buffer_len
#ifdef NAMED_ENTITIES_ENABLED
                  , named_entities_buffer, named_entities_buffer_len,
                  named_entities_len, named_entities_count
#endif
#ifdef KEYWORDS_ENABLED
                  , keywords_buffer, keywords_buffer_len,
                  keywords_len, keywords_count
#endif
#ifdef KEYPHRASE_ENABLED
                  , keyphrases_buffer, keyphrases_buffer_len,
                  keyphrases_len, keyphrases_count
#endif
#ifdef LANG_ENABLED
                  , lang_words_buffer, lang_words_buffer_len,
                  lang_words_len, lang_words_count
#endif
#ifdef TEXT_CLASSIFICATION_ENABLED
                  , text_class_words_buffer, text_class_words_buffer_len,
                  text_class_words_len, text_class_words_count
#endif
#ifdef INTENT_ENABLED
                  , intent_buffer, intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                  , sentiment_buffer, sentiment_buffer_len
#endif // SENTIMENT_ENABLED
                )) < 0) {
    std::cout << "ERROR: could not get keytuples\n";
    text_buffer[0] = '\0';
#ifdef NAMED_ENTITIES_ENABLED
    named_entities_buffer[0] = '\0';
#endif
#ifdef KEYWORDS_ENABLED
    keywords_buffer[0] = '\0';
#endif
#ifdef KEYPHRASE_ENABLED
    keyphrases_buffer[0] = '\0';
#endif
#ifdef LANG_ENABLED
    lang_words_buffer[0] = '\0';
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    text_class_words_buffer[0] = '\0';
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
    intent_buffer[0] = '\0';
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
    sentiment_buffer[0] = '\0';
#endif // SENTIMENT_ENABLED
    return -1;
  }

  text_buffer[0] = '\0';

  script.assign(script_buffer, script_buffer_len);
  safe_status.assign(safe_status_buffer, safe_status_buffer_len);

#if defined NAMED_ENTITIES_ENABLED || defined KEYWORDS_ENABLED || defined KEYPHRASE_ENABLED || defined TEXT_CLASSIFICATION_ENABLED || defined LANG_ENABLED
  unsigned char* pch1 = NULL;
  unsigned char* pch2 = NULL;
  unsigned char ch;
#endif

#ifdef NAMED_ENTITIES_ENABLED
  if (named_entities_len > 0) {
    pch1 = named_entities_buffer;
    pch2 = pch1;
    pch1 = (unsigned char*) strchr((char *) pch2, '|');
    while (pch1 && pch1 != '\0') {
      ch = *pch1;
      *pch1 = '\0';
      named_entities_set.insert((char *) pch2);
      *pch1 = ch;
      pch2 = pch1 + 1;
      pch1 = (unsigned char*) strchr((char *) pch2, '|');
    }
  }
  named_entities_buffer[0] = '\0';
#endif // NAMED_ENTITIES_ENABLED

#ifdef KEYWORDS_ENABLED
  if (keywords_len > 0) {
    pch1 = keywords_buffer;
    pch2 = pch1;
    pch1 = (unsigned char*) strchr((char *) pch2, '|');
    while (pch1 && pch1 != '\0') {
      ch = *pch1;
      *pch1 = '\0';
      keywords_set.insert((char *) pch2);
      *pch1 = ch;
      pch2 = pch1 + 1;
      pch1 = (unsigned char*) strchr((char *) pch2, '|');
    }
  }
  keywords_buffer[0] = '\0';
#endif // KEYWORDS_ENABLED

#ifdef KEYPHRASE_ENABLED
  if (keyphrases_len > 0) {
    pch1 = keyphrases_buffer;
    pch2 = pch1;
    pch1 = (unsigned char*) strchr((char *) pch2, '|');
    while (pch1 && pch1 != '\0') {
      ch = *pch1;
      *pch1 = '\0';
      keyphrases_set.insert((char *) pch2);
      *pch1 = ch;
      pch2 = pch1 + 1;
      pch1 = (unsigned char*) strchr((char *) pch2, '|');
    }
  }
  keyphrases_buffer[0] = '\0';
#endif // KEYPHRASE_ENABLED

#ifdef LANG_ENABLED
  if (lang_words_len > 0) {
    pch1 = lang_words_buffer;
    pch2 = pch1;
    pch1 = (unsigned char*) strchr((char *) pch2, '|');
    while (pch1 && pch1 != '\0') {
      ch = *pch1;
      *pch1 = '\0';
      lang_words_set.insert((char *) pch2);
      *pch1 = ch;
      pch2 = pch1 + 1;
      pch1 = (unsigned char*) strchr((char *) pch2, '|');
    }
  }
  lang_words_buffer[0] = '\0';
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  if (text_class_words_len > 0) {
    pch1 = text_class_words_buffer;
    pch2 = pch1;
    pch1 = (unsigned char*) strchr((char *) pch2, '|');
    while (pch1 && pch1 != '\0') {
      ch = *pch1;
      *pch1 = '\0';
      text_class_words_set.insert((char *) pch2);
      *pch1 = ch;
      pch2 = pch1 + 1;
      pch1 = (unsigned char*) strchr((char *) pch2, '|');
    }
  }
  text_class_words_buffer[0] = '\0';
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef INTENT_ENABLED
  intent.assign(intent_buffer, intent_buffer_len);
#endif // INTENT_ENABLED

#ifdef SENTIMENT_ENABLED
  sentiment.assign(sentiment_buffer, sentiment_buffer_len);
#endif // SENTIMENT_ENABLED

  return count;
}

// this is an helper function used to make a|b|c| kind of strings given a, b, c in consecutive calls
inline void KeyTuplesExtracter::Insert(unsigned char* buffer, unsigned int& current_len,
                   unsigned char* str_to_add, const unsigned int& str_len,
                   unsigned int& buffer_content_count) { 
  if (strstr((char *) buffer, (char *) str_to_add) == NULL) {
    strncpy((char *) buffer + current_len, (char *) str_to_add, str_len);
    current_len += str_len;
    strcpy((char *) buffer + current_len, "|");
    current_len += 1;
    buffer_content_count++;
  }
}

inline void KeyTuplesExtracter::Insert(unsigned char* buffer, unsigned int& current_len,
                                    std::string& str, unsigned int& buffer_content_count) { 
  unsigned int len = str.length();
  if (len > 0) {
    if (strstr((char *) buffer, str.c_str()) == NULL) {
      strncpy((char *) buffer + current_len, str.c_str(), len);
      current_len += len;
      strcpy((char *) buffer + current_len, "|");
      current_len += 1;
      buffer_content_count++;
    }
  }
}

int KeyTuplesExtracter::GetKeyTuples(unsigned char* text_buffer,
                                     const unsigned int& text_buffer_len,
                                     const unsigned int& text_len,
                                     char* safe_status_buffer,
                                     const unsigned int& safe_status_buffer_len,
                                     char* script_buffer,
                                     const unsigned int& script_buffer_len
#ifdef NAMED_ENTITIES_ENABLED
                                     , unsigned char* named_entities_buffer,
                                     const unsigned int& named_entities_buffer_len,
                                     unsigned int& named_entities_len,
                                     unsigned int& named_entities_count
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
                                     , unsigned char* keywords_buffer,
                                     const unsigned int& keywords_buffer_len,
                                     unsigned int& keywords_len,
                                     unsigned int& keywords_count
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
                                     , unsigned char* keyphrases_buffer,
                                     const unsigned int& keyphrases_buffer_len,
                                     unsigned int& keyphrases_len,
                                     unsigned int& keyphrases_count
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
                                     , unsigned char* lang_words_buffer,
                                     const unsigned int& lang_words_buffer_len,
                                     unsigned int& lang_words_len,
                                     unsigned int& lang_words_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                                     , unsigned char* text_class_words_buffer,
                                     const unsigned int& text_class_words_buffer_len,
                                     unsigned int& text_class_words_len,
                                     unsigned int& text_class_words_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef INTENT_ENABLED
                                     , char* intent_buffer,
                                     const unsigned int& intent_buffer_len
#endif // INTENT_ENABLED
#ifdef SENTIMENT_ENABLED
                                     , char* sentiment_buffer,
                                     const unsigned int& sentiment_buffer_len
#endif // SENTIMENT_ENABLED
                                    ) {

  if (!text_buffer || text_buffer_len < 1 || text_len < 1 || !script_buffer || !safe_status_buffer) {
    std::cout << "ERROR: invalid buffer(s) at input\n";
    return -1;
  }

  // initialize output parameters
  *safe_status_buffer = '\0';
  *script_buffer = '\0';

#ifdef NAMED_ENTITIES_ENABLED
  if (!named_entities_buffer) {
#ifdef KE_DEBUG
    std::cerr << "ERROR: invalid named_entities buffer\n";
#endif // KE_DEBUG
    return -1;
  }
  *named_entities_buffer = '\0';
  named_entities_len = 0;
  named_entities_count = 0;
#endif // NAMED_ENTITIES_ENABLED

#ifdef KEYWORDS_ENABLED
  if (!keywords_buffer) {
#ifdef KE_DEBUG
    std::cerr << "ERROR: invalid keywords buffer\n";
#endif // KE_DEBUG
    return -1;
  }
  *keywords_buffer = '\0';
  keywords_len = 0;
  keywords_count = 0;
#endif // KEYWORDS_ENABLED

#ifdef KEYPHRASE_ENABLED
  if (!keyphrases_buffer) {
#ifdef KE_DEBUG
    std::cerr << "ERROR: invalid keyphrases buffer\n";
#endif // KE_DEBUG
    return -1;
  }
  *keyphrases_buffer = '\0';
  keyphrases_len = 0;
  keyphrases_count = 0;
#endif // KEYPHRASE_ENABLED

#ifdef LANG_ENABLED
  if (!lang_words_buffer) {
#ifdef KE_DEBUG
    std::cerr << "ERROR: invalid lang_words buffer\n";
#endif // KE_DEBUG
    return -1;
  }
  *lang_words_buffer = '\0';
  lang_words_len = 0;
  lang_words_count = 0;
  std::set<std::string> lang_words_set;
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  if (!text_class_words_buffer) {
#ifdef KE_DEBUG
    std::cerr << "ERROR: invalid text_class_words buffer\n";
#endif // KE_DEBUG
    return -1;
  }
  *text_class_words_buffer = '\0';
  text_class_words_len = 0;
  text_class_words_count = 0;
  std::set<std::string> text_class_words_set;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef INTENT_ENABLED
  if (!intent_buffer) {
#ifdef KE_DEBUG
    std::cerr << "ERROR: invalid intent buffer\n";
#endif // KE_DEBUG
    return -1;
  }
  intent_buffer[0] = '\0';
#endif // INTENT_ENABLED

#ifdef SENTIMENT_ENABLED
  if (!sentiment_buffer) {
#ifdef KE_DEBUG
    std::cerr << "ERROR: invalid sentiment buffer\n";
#endif // KE_DEBUG
    return -1;
  }
  sentiment_buffer[0] = '\0';
#endif // SENTIMENT_ENABLED

  unsigned char *ptr = NULL;
  unsigned char *probe = NULL;
  unsigned char current_word_delimiter;
  unsigned char prev_word_delimiter;
  unsigned char next_word_delimiter;

  unsigned int current_word_len = 0;
  unsigned int next_word_len = 0;
  int num_mixed_words = 0;
  int num_caps_words = 0;
  int num_words = 0;
  int num_stop_words = 0;
  int num_dict_words = 0;
  int num_numeric_words = 0;
  int num_normal_words = 0; // not caps or stop or dict or numeric

  unsigned char *current_word_start = NULL;
  unsigned char *current_word_end = NULL;
  unsigned char *prev_word_start = NULL;
  unsigned char *prev_word_end = NULL;
  unsigned char *next_word_start = NULL;
  unsigned char *next_word_end = NULL;

#if defined NAMED_ENTITIES_ENABLED || defined TEXT_CLASSIFICATION_ENABLED
  unsigned char *caps_entity_start = NULL;
  unsigned char *caps_entity_end = NULL;
  unsigned char *stopwords_entity_start = NULL;
  unsigned char *stopwords_entity_end = NULL;
#endif // NAMED_ENTITIES_ENABLED || TEXT_CLASSIFICATION_ENABLED

#ifdef KEYPHRASE_ENABLED
  unsigned char *stopwords_keyphrase_start = NULL;
  unsigned char *stopwords_keyphrase_end = NULL;
#endif // KEYPHRASE_ENABLED

  unsigned char *sentence_start = NULL;

#ifdef INTENT_ENABLED
  unsigned char *intent_start = NULL;
  unsigned char *intent_end = NULL;
  std::string intent_phrase;
  std::map<std::string, int> intent_words;
  unsigned char *orig_start = NULL;
  unsigned int orig_offset = 0;
#endif // INTENT_ENABLED

  // TODO (balaji) use bit map and masks to reduce comparisons
  bool current_word_caps = false;
  bool current_word_all_caps = false;
  bool current_word_has_mixed_case = false;
  bool current_word_starts_num = false;
  bool current_word_precedes_punct = false;
  bool current_word_precedes_ignore_word = false;
  unsigned char* current_word_has_apostropheS = NULL;
  bool current_word_hashtag = false;
  bool prev_word_caps = false;
  bool prev_word_all_caps = false;
  bool prev_word_starts_num = false;
  bool prev_word_has_mixed_case = false;
  bool prev_word_precedes_punct = false;
  bool prev_word_precedes_ignore_word = false;
  unsigned char* prev_word_has_apostropheS = NULL;
  bool prev_word_hashtag = false;
  bool next_word_caps = false;
  bool next_word_all_caps = false;
  bool next_word_starts_num = false;
  bool next_word_has_mixed_case = false;
  bool next_word_precedes_punct = false;
  bool next_word_precedes_ignore_word = false;
  unsigned char* next_word_has_apostropheS = NULL;
  bool next_word_hashtag = false;

  bool current_word_stop = false;
  bool current_word_dict = false;
  bool prev_word_stop = false;
  bool prev_word_dict = false;
  bool next_word_stop = false;
  bool next_word_dict = false;
  bool is_ignore_word = false;
  bool is_punct = false;

  // misc
#if defined NAMED_ENTITIES_ENABLED || defined KEYWORDS_ENABLED || defined KEYPHRASE_ENABLED || defined TEXT_CLASSIFICATION_ENABLED
  unsigned char *pch = NULL;
  unsigned char ch;
#endif

#if defined SENTIMENT_ENABLED || defined INTENT_ENABLED
  int dict_value = 0;
#endif // SENTIMENT_ENABLED || INTENT_ENABLED

#ifdef SENTIMENT_ENABLED
  int sentiment_valence = 0;
  strcpy(sentiment_buffer, "neutral");
#endif // SENTIMENT_ENABLED

#ifdef INTENT_ENABLED
  int intent_valence = 0;
  std::string intent_str;
  if ((2 * (text_len + 1)) < text_buffer_len) {
    text_buffer[(2 * text_len) + 1] = '\0';
    orig_offset = text_len + 1;
    orig_start = text_buffer + orig_offset;
    memcpy(orig_start, text_buffer, text_len);
  }
#endif // INTENT_ENABLED

  // unsafe
  strcpy(safe_status_buffer, "safe");
  bool text_has_unsafe_words = false;

  // script detection
  unsigned char *end = (unsigned char*) strchr((char *) text_buffer, '\0');
  std::string script = "UU";
  strcpy(script_buffer, "UU");
  unsigned int code_point = 0;
  string script_temp;
  unsigned int script_count = 0;
  unsigned int english_count = 0;
  bool current_word_ascii = false;
  bool next_word_ascii = false;

  // the whole thing starts here
  ptr = text_buffer;

#ifdef KE_DEBUG
  if (KE_DEBUG > 3)
    cout << endl << "original query: " << std::string((char *) text_buffer) << endl << endl;
#endif // KE_DEBUG

  // go to the first word, ignoring handles and punctuations
  unsigned char *prev = NULL;
  while (ptr && '\0' != *ptr && (' ' == *ptr || (ispunct(*ptr) && IsPunct((char *&) ptr, (char *) prev, (char *) ptr+1)) || IsIgnore((char *&) ptr))) {
    prev = ptr;
    ptr++;
  }

  if (!ptr || '\0' == *ptr) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0)
      cout << "either the input is empty or has ignore words only" << endl;
#endif // KE_DEBUG
    return 0;
  }

  if ('#' == *ptr) {
    current_word_hashtag = true;
    ptr++;
  }

  current_word_start = ptr;
  sentence_start = ptr;
#ifdef INTENT_ENABLED
  if (orig_start) {
    intent_start = orig_start + (ptr - text_buffer);
  }
#endif // INTENT_ENABLED

#ifdef KE_DEBUG
  if (KE_DEBUG > 3) {
    cout << "sentence start: " << sentence_start << endl;
  }
#endif // KE_DEBUG

  if (isupper(*ptr)) {
    current_word_caps = true;
    num_caps_words++;
    if (isupper(*(ptr+1))) {
      current_word_all_caps = true;
    }
    //*ptr += 32;
  } else if (isdigit(*ptr)) {
    current_word_starts_num = true; 
    num_numeric_words++;
  }

  // now lets find the end of the current word - while loop works from the second letter
  //ptr++;
  try {
    code_point = utf8::next(ptr, end);
    if (code_point > 0x7F) {
      if (inagist_classifiers::DetectScript(code_point, script_temp) > 0) {
        if (script_temp != "en") {
          if (script_temp != script) {
            script_count = 0;
            script = script_temp;
          }
          else {
            script_count++;
          }
        }
      }
    } else {
      if (code_point > 0x40 && code_point < 0x7B)
        english_count++;
    }
  } catch (...) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0) {
      std::cout << "EXCEPTION 1: utf8 returned exception" << std::endl;
      cout << endl << "original query: " << std::string((char *) text_buffer) << endl << endl;
    }
#endif // KE_DEBUG
    return -1;
  }

  while (ptr && ' ' != *ptr && '\0' != *ptr &&
         !(is_punct = IsPunct((char *&) ptr, (char *) ptr-1, (char *) ptr+1/*, current_word_has_apostropheS*/))) {

    if (!ptr || '\0' == *ptr) {
#ifdef KE_DEBUG
      if (KE_DEBUG > 1) {
        cout << "either the input is empty or has ignore words only" << endl;
      }
#endif // KE_DEBUG
      return 0;
    }


    if (isupper(*ptr)) {
      if (!current_word_all_caps) {
        current_word_has_mixed_case = true;
      }
      //*ptr += 32;
    } else if (islower(*ptr)) {
      if (current_word_all_caps) {
        current_word_has_mixed_case = true;
        current_word_all_caps = false;
      }
    }

    //ptr++;
    try {
      code_point = utf8::next(ptr, end);
      if (code_point > 0xFF) {
        if (inagist_classifiers::DetectScript(code_point, script_temp) > 0) {
          if (script_temp != "en") {
            if (script_temp != script) {
              script_count = 0;
              script = script_temp;
            }
            else {
              script_count++;
            }
          }
        }
      } else {
        if (code_point > 0x40 && code_point < 0x7B)
          english_count++;
      }
    } catch (...) {
#ifdef KE_DEBUG
      if (KE_DEBUG > 1) {
        std::cout << "EXCEPTION 2: utf8 returned exception" << std::endl;
        cout << endl << "original query: " << std::string((char *) text_buffer) << endl << endl;
      }
#endif // KE_DEBUG
      return -1;
    }
  }

  if (!ptr) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0) {
      cout << "either the input is corrupt or the only word is ignore word" << endl;
    }
#endif // KE_DEBUG
    return 0;
  }

  current_word_end = ptr;
  current_word_delimiter = *ptr;
  current_word_len = current_word_end - current_word_start;
  *ptr = '\0';
  if (current_word_len > 2 && strcmp((char *) current_word_end - 2, "'s") == 0) {
    current_word_has_apostropheS = current_word_end-2;
  }
  current_word_precedes_punct = is_punct;
  num_words++;

#ifndef I18N_ENABLED
  code_point = *current_word_start;
  if (code_point > 0x40 && code_point < 0x7B) {
    current_word_ascii = true;
  } else if (code_point == 0x23) {
    code_point = *(current_word_start+1);
    if (code_point > 0x40 && code_point < 0x7B)
      current_word_ascii = true;
  } else if (inagist_classifiers::ExtendedAsciiText(code_point)) {
    next_word_ascii = true;
  }

  if (current_word_ascii) {
#endif // I18N_ENABLED
    // stop words
    if (m_stopwords_dictionary.Find(current_word_start) == 1) {
      current_word_stop = true;
      num_stop_words++;
#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        cout << "current word: " << current_word_start << " :stopword" << endl;
      }
#endif // KE_DEBUG
    } else {
      current_word_stop = false;
    }

    // dictionary words
    if (m_dictionary.Find(current_word_start) == 1) {
      current_word_dict = true;
      num_dict_words++;
#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        cout << "current word: " << current_word_start << " :dictionary word" << endl;
      }
#endif // KE_DEBUG
    } else {
      current_word_dict = false;
    }
  
    // unsafe words
    if (m_unsafe_dictionary.Find(current_word_start) == 1) {
      text_has_unsafe_words = true;
    }

#ifdef SENTIMENT_ENABLED
    if (m_sentiment_words_dictionary.Find(current_word_start, dict_value) == 1) {
      sentiment_valence += dict_value;
#ifdef KE_DEBUG
      std::cout << "word:" << current_word_start << " dict_value: " << dict_value << std::endl;
#endif // KE_DEBUG
    }
#endif // SENTIMENT_ENABLED

#ifdef INTENT_ENABLED
    bool flag = false;
    if (m_intent_words_dictionary.Find(current_word_start, dict_value) == 1) {
      intent_words[std::string((const char*) current_word_start)] = dict_value;
      flag = true;
      // std::cout << "intent_sentence: " << current_word_start << " value: " << dict_value << std::endl;
    }
    if (orig_start) {
      if (flag) {
        intent_start = orig_start + (current_word_start - text_buffer);
      }
    }
#endif

#ifndef I18N_ENABLED
  }
#endif // I18N_ENABLED

#ifdef LANG_ENABLED
  if (current_word_start &&
      (current_word_stop || current_word_dict) &&
      !current_word_hashtag) {
    lang_words_set.insert(std::string((char *) current_word_start));
  }
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  //bool is_apostrophe;
  //is_apostrophe = false;
  if (current_word_start &&
      !current_word_stop &&
      !current_word_dict &&
      !current_word_starts_num &&
      !current_word_hashtag &&
      current_word_len > 1) {
    if (current_word_has_apostropheS) {
      *current_word_has_apostropheS = '\0';
    }
    /*
    if (strncmp((char *) current_word_end-2, "\'s", 2) == 0) {
      is_apostrophe = true;
      ch = *(current_word_end-2);
      *(current_word_end-2) = '\0';
    }
    */
    if (!current_word_caps || current_word_all_caps || current_word_has_mixed_case) {
      text_class_words_set.insert(std::string((char *) current_word_start));
    } else { 
      *current_word_start += 32;
      text_class_words_set.insert(std::string((char *) current_word_start));
      *current_word_start -= 32;
    }
    if (current_word_has_apostropheS) {
      *current_word_has_apostropheS = '\'';
    }
    /*
    if (is_apostrophe) {
      *(current_word_end-2) = ch;
    }
    */
  }
#endif // TEXT_CLASSIFICATION_ENABLED

  if ('\0' != current_word_delimiter) {
    // this is one gigantic if ending at the end of this function

  // go to the next word, ignoring punctuation and ignore words.
  // however passing over ignorewords must be recorded
  //ptr++;
  try {
    code_point = utf8::next(ptr, end);
    if (code_point > 0xFF) {
      if (inagist_classifiers::DetectScript(code_point, script_temp) > 0) {
        if (script_temp != "en") {
          if (script_temp != script) {
            script_count = 0;
            script = script_temp;
          }
          else {
            script_count++;
          }
        }
      }
    } else {
      if (code_point > 0x40 && code_point < 0x7B)
        english_count++;
    }
  } catch (...) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0) {
      std::cout << "EXCEPTION 3: utf8 returned exception" << std::endl;
      cout << endl << "original query: " << std::string((char *) text_buffer) << endl << endl;
    }
#endif // KE_DEBUG
    return -1;
  }

  is_ignore_word = false;
  is_punct = false;
  while ('\0' != *ptr &&
         (' ' == *ptr ||
          (ispunct(*ptr) && (is_punct = IsPunct((char *&) ptr, (char *) ptr-1, (char *) ptr+1))) ||
          (is_ignore_word = IsIgnore((char *&) ptr))
         )
        ) {
    current_word_precedes_ignore_word |= is_ignore_word;
    current_word_precedes_punct |= is_punct;
    ptr++;
  }

  if (!ptr || '\0' == *ptr) {
    next_word_start = NULL;
#ifdef KE_DEBUG
    if (KE_DEBUG > 1) {
      std::cout << "only one word found\n";
    }
#endif // KE_DEBUG
    return 0;
  } else {
    if ('#' == *ptr) {
      next_word_hashtag = true;
      ptr++;
    }
    next_word_start = ptr;
#ifdef INTENT_ENABLED
    if (orig_start && !intent_start) {
      intent_start = orig_start + (next_word_start - text_buffer);
    }
#endif // INTENT_ENABLED
    num_words++;
    if (current_word_precedes_ignore_word || current_word_precedes_punct) {
      sentence_start = next_word_start;
#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        cout << "sentence start: " << sentence_start << endl;
      }
#endif // KE_DEBUG
    }

    // remember - this is start of word
    if (isupper(*next_word_start)) {
      next_word_caps = true;
      num_caps_words++;
      if (isupper(*(next_word_start + 1))) {
        next_word_all_caps = true;
      }
      //*next_word_start += 32;
    } else if (isdigit(*next_word_start)) {
      next_word_starts_num = true;
      num_numeric_words++;
    }

  }

  // now we need to achieve the following
  // probe = ptr + 1;
  probe = ptr;
  try {
    if (probe < end) {
      code_point = utf8::next(probe, end);
    } else {
#ifdef KE_DEBUG
      if (KE_DEBUG > 0) {
        std::cout << "ERROR: invalid pointers?" << std::endl;
      }
#endif // KE_DEBUG
      return 0;
    }
  } catch (...) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0) {
      std::cout << "EXCEPTION 4: utf8 returned exception" << std::endl;
      cout << endl << "original query: " << std::string((char *) text_buffer) << endl << endl;
    }
#endif // KE_DEBUG
    return -1;
  }

  while (ptr && probe/* && *ptr != '\n' && *ptr != '\0'*/) {
    // this loop works between second letter to end punctuation for each word
    is_punct = false;
    if (' ' == *probe || '\0' == *probe ||
        (ispunct(*probe) &&
         (is_punct = IsPunct((char *&) probe, (char *) probe-1, (char *) probe+1/*, next_word_has_apostropheS*/))
        )
       ) {

/*
      if (next_word_has_apostropheS) {
        std::cout << next_word_start << " has apostropheS" << std::endl;
      }
*/

      // word boundary
      if (next_word_start) {
        if (is_punct)
          next_word_precedes_punct = true;
        next_word_delimiter = *probe;
        next_word_end = probe;
        *probe = '\0';
        next_word_len = next_word_end - next_word_start;
        if (next_word_len > 2 && strcmp((char *) next_word_end - 2, "'s") == 0) {
          next_word_has_apostropheS = next_word_end-2;
        }
      }

#ifdef LANG_ENABLED
      // TODO (balaji) - not sure what is being tried here!
      /*
      if (current_word_start == sentence_start &&
          current_word_caps &&
          (!next_word_start || !next_word_caps)) {
        *current_word_start += 32;
        lang_words_set.insert(std::string((char *) current_word_start));
        *current_word_start -= 32;
      }

      if (!next_word_start) {
        if (!current_word_caps) {
          lang_words_set.insert(std::string((char *) current_word_start));
        }
        if (current_word_caps && current_word_start == sentence_start) {
          *current_word_start += 32;
          lang_words_set.insert(std::string((char *) current_word_start));
          *current_word_start -= 32;
        }
      }
      */
#endif // LANG_ENABLED

#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        cout << endl;
        if (prev_word_start)
          cout << "prev word: " << prev_word_start << endl;
        else
          cout << "prev word: NULL" << endl;
        cout << "current word: " << current_word_start << endl;
        if (next_word_start)
          cout << "next word: " << next_word_start << endl;
        else
          cout << "next word: NULL" << endl;
        cout << endl;
      }
#endif // KE_DEBUG

#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        if (prev_word_caps)
          cout << "prev word: " << prev_word_start << " :starts with caps" << endl;
        if (current_word_all_caps) {
          if (current_word_len > 1 && current_word_len < 6) {
            cout << "current word: " << current_word_start << " :all caps" << endl;
          } else {
            cout << "current word: " << current_word_start << " :all caps but bad length" << endl;
          }
        } else if (current_word_has_mixed_case) {
          cout << "current word: " << current_word_start << " :mixed case" << endl;
        } else if (current_word_caps) {
          cout << "current word: " << current_word_start << " :starts with caps" << endl;
        }
        if (next_word_caps)
          cout << "next word: " << next_word_start << " :starts with caps" << endl;
      }
#endif // KE_DEBUG

      if (next_word_start) {
#ifndef I18N_ENABLED
        code_point = *next_word_start;
        if (code_point > 0x40 && code_point < 0x7B) {
          next_word_ascii = true;
        } else if (code_point == 0x23) {
          code_point = *(next_word_start+1);
          if (code_point > 0x40 && code_point < 0x7B)
            next_word_ascii = true;
        } else if (inagist_classifiers::ExtendedAsciiText(code_point)) {
          next_word_ascii = true;
        }

        if (next_word_ascii) {
#endif // I18N_ENABLED
        // stop words
        if (m_stopwords_dictionary.Find(next_word_start) == 1) {
          next_word_stop = true;
          num_stop_words++;
#ifdef KE_DEBUG
          if (KE_DEBUG > 5) {
            cout << "next word: " << next_word_start << " :stopword" << endl;
          }
#endif // KE_DEBUG
        } else {
          next_word_stop = false;
        }

        // dictionary words
        if (m_dictionary.Find(next_word_start) == 1) {
          next_word_dict = true;
          num_dict_words++;
#ifdef KE_DEBUG
          if (KE_DEBUG > 5) {
            cout << "next word: " << next_word_start << " :dictionary word" << endl;
          }
#endif // KE_DEBUG
        } else {
          next_word_dict = false;
        }

        // dictionary words
        if (m_unsafe_dictionary.Find(next_word_start) == 1) {
          text_has_unsafe_words = true;
        }

#ifdef SENTIMENT_ENABLED
        if (m_sentiment_words_dictionary.Find(next_word_start, dict_value) == 1) {
          sentiment_valence += dict_value;
#ifdef KE_DEBUG
          std::cout << "word:" << next_word_start << " dict_value: " << dict_value << std::endl;
#endif // KE_DEBUG
        }
#endif // SENTIMENT_ENABLED

#ifdef INTENT_ENABLED
       bool flag = false;
       if (m_intent_words_dictionary.Find(next_word_start, dict_value) == 1) {
         intent_words[std::string((const char*) next_word_start)] = dict_value;
         flag = true;
         // std::cout << "intent_sentence: " << next_word_start << " value: " << dict_value << std::endl;
       }
       if (orig_start) {
         if (flag) {
           if (intent_start) {
             intent_end = orig_start + (next_word_end - text_buffer);
             intent_phrase.assign((const char*) intent_start, 0, intent_end-intent_start);
             if (m_intent_words_dictionary.Find(intent_phrase, dict_value) == 1) {
               intent_words[intent_phrase] = dict_value;
             }
           } else {
             intent_start = orig_start + (next_word_start - text_buffer);
           }
         } else {
           intent_start = NULL;
         }
         if (!flag) {
           intent_start = NULL;
         }
       }
#endif // INTENT_ENABLED

#ifdef LANG_ENABLED
        if ((next_word_stop ||
             next_word_dict) &&
             !next_word_hashtag) {
          lang_words_set.insert(std::string((char *) next_word_start));
        }
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
        if (!next_word_stop &&
            !next_word_dict &&
            !next_word_starts_num &&
            !next_word_hashtag &&
            next_word_len > 1) {
          //is_apostrophe = false;
          /*
          if (strncmp((char *) next_word_end-2, "\'s", 2) == 0) {
            is_apostrophe = true;
            ch = *(next_word_end-2);
            *(next_word_end-2) = '\0';
          }
          */
          if (next_word_has_apostropheS) {
            *next_word_has_apostropheS = '\0';
          }
          if (!next_word_caps || next_word_all_caps || next_word_has_mixed_case) {
            text_class_words_set.insert(std::string((char *) next_word_start));
          } else {
            *next_word_start += 32;
            text_class_words_set.insert(std::string((char *) next_word_start));
            *next_word_start -= 32;
          }
          /*
          if (is_apostrophe) {
            *(next_word_end-2) = ch;
          }
          */
          if (next_word_has_apostropheS) {
            *next_word_has_apostropheS = '\'';
          }
        }
#endif // TEXT_CLASSIFICATION_ENABLED

#ifndef I18N_ENABLED
        }
#endif // I18N_ENABLED
      }

      if (prev_word_end)
        *prev_word_end = prev_word_delimiter;

#ifndef I18N_ENABLED
      if (current_word_ascii) {
#endif // I18N_ENABLED

      if (!current_word_stop && !current_word_dict && !current_word_caps &&
          !current_word_starts_num && !current_word_has_mixed_case &&
          (current_word_len > 1) && !current_word_hashtag) {
#ifdef KE_DEBUG
        if (KE_DEBUG > 5) {
          cout << current_word_start << ": normal word" << endl;
        }
#endif // KE_DEBUG
        num_normal_words++;
#ifdef LANG_ENABLED
        lang_words_set.insert(std::string((char *) current_word_start));
#endif // LANG_ENABLED
      }
      if (current_word_has_mixed_case)
        num_mixed_words++;

#if defined NAMED_ENTITIES_ENABLED || defined TEXT_CLASSIFICATION_ENABLED
      if (NULL == stopwords_entity_start) {
        if (current_word_stop) {
          // X of Y case
          if (strcmp((char *) current_word_start, "of") == 0 && NULL != next_word_start && NULL != prev_word_start) {
            if ((prev_word_caps && next_word_caps) &&
                (!prev_word_stop && !next_word_stop) &&
                (!prev_word_dict && !next_word_dict) &&
                (!prev_word_dict && !next_word_dict) &&
                !current_word_precedes_ignore_word &&
                !prev_word_precedes_ignore_word) {
              if (caps_entity_start && caps_entity_start < prev_word_start)
                stopwords_entity_start = caps_entity_start;
              else
                stopwords_entity_start = prev_word_start;
            }
          }
          if (caps_entity_start && (strcmp((char *) current_word_start, "and") == 0) &&
              next_word_start && next_word_caps && !next_word_dict && !next_word_stop &&
              !prev_word_precedes_ignore_word && !prev_word_precedes_punct &&
              !current_word_precedes_ignore_word && !current_word_precedes_punct) {
            stopwords_entity_start = caps_entity_start;
          }
        } else if (NULL != prev_word_start && current_word_starts_num &&
                   (' ' == current_word_delimiter || '\0' == current_word_delimiter)) {
          // handling numbers that occur with cap entities
          if (prev_word_caps && !prev_word_stop && !prev_word_dict &&
              !prev_word_precedes_ignore_word && !prev_word_precedes_punct && 
              (current_word_len > 1 || prev_word_all_caps)) {

            if (caps_entity_start && caps_entity_start < prev_word_start)
              stopwords_entity_start = caps_entity_start;
            else
              stopwords_entity_start = prev_word_start;

            if (!next_word_start || !next_word_caps || next_word_stop || next_word_dict ||
                current_word_precedes_ignore_word || current_word_precedes_punct) {
                stopwords_entity_end = current_word_end;
            }
          }
        } else if (prev_word_stop) {
          if ((NULL == next_word_start || next_word_start == sentence_start) &&
              prev_word_start && strncmp((char *) prev_word_start, "at", 2) == 0 && current_word_caps &&
              !current_word_stop && !current_word_dict) {
            // TODO (balaji) dangerous! don't use strncmp. instead preserve prev_word_delimiter
            stopwords_entity_start = current_word_start;
            stopwords_entity_end = current_word_end;
          } else if (!caps_entity_start && prev_word_start && next_word_start) {
            // Experimental location detection - TODO (balaji) use regex if this experiment succeeds
            if (current_word_caps &&
                strcmp((char *) prev_word_start, "in") == 0 && ',' == current_word_delimiter &&
                next_word_caps && !current_word_dict &&
                !next_word_stop && !next_word_dict && !current_word_stop
               ) {
              stopwords_entity_start = current_word_start;
              stopwords_entity_end = current_word_end;
            } else if (next_word_caps &&
                       ((strcmp((char *) prev_word_start, "place") == 0 &&
                         strcmp((char *) current_word_start, "called") == 0 &&
                         !next_word_stop &&
                         (',' == next_word_delimiter || '.' == next_word_delimiter || '\0' == next_word_delimiter)
                        ) ||
                        (strcmp((char *) prev_word_start, "town") == 0 &&
                         (strcmp((char *) current_word_start, "of") == 0 ||
                          strcmp((char *) current_word_start, "called") == 0) &&
                         !next_word_stop &&
                         (',' == next_word_delimiter || '.' == next_word_delimiter || '\0' == next_word_delimiter)
                        )
                       )
                      ) {
              stopwords_entity_start = next_word_start;
              stopwords_entity_end = next_word_end;
            }
          }
        } else if (caps_entity_start &&
                   next_word_start && next_word_caps && !next_word_stop && !next_word_dict) {
          // Experimental sports event detection - TODO (balaji) use regex if this experiment succeeds
          if ((strcmp((char *) current_word_start, "vs") == 0) ||
              (strcmp((char *) current_word_start, "v") == 0) ||
              (strcmp((char *) current_word_start, "beat") == 0) ||
              (strcmp((char *) current_word_start, "def") == 0) ||
              (strcmp((char *) current_word_start, "defeat") == 0) ||
              (strcmp((char *) current_word_start, "beats") == 0) ||
              (strcmp((char *) current_word_start, "defeats") == 0)) {
            stopwords_entity_start = caps_entity_start;
          }
        }
      } else {
#ifdef KE_DEBUG
        if (KE_DEBUG > 5) {
          cout << "stopword entity candidate: " << stopwords_entity_start << endl;
        }
#endif // KE_DEBUG

        if (!current_word_caps || current_word_stop || current_word_dict || current_word_starts_num) {
          if (stopwords_entity_start != prev_word_start) {
            stopwords_entity_end = prev_word_end;
          }
          else {
            stopwords_entity_start = NULL;
            stopwords_entity_end = NULL;
          }
        } else if (NULL == next_word_start ||
                   current_word_precedes_ignore_word ||
                   ((current_word_end + 1) != next_word_start)) {
          if (stopwords_entity_start != current_word_start) {
            stopwords_entity_end = current_word_end;
          }
          else {
            stopwords_entity_start = NULL;
            stopwords_entity_end = NULL;
          }
        }
      }
#endif // NAMED_ENTITIES_ENABLED || TEXT_CLASSIFICATION_ENABLED

#ifdef KEYPHRASE_ENABLED
      if (NULL == stopwords_keyphrase_start) {
        if ('\0' != current_word_delimiter &&
#ifndef I18N_ENABLED
            current_word_ascii &&
#endif // I18N_ENABLED
            !current_word_stop &&
            !current_word_dict &&
            //!current_word_hashtag && // NOTE: hashtags can be start of keyphrases
            NULL != next_word_start &&
            !current_word_precedes_ignore_word &&
            ((current_word_len > 1) || isdigit(*current_word_start))) {
          if (' ' == current_word_delimiter &&
              ((current_word_end + 1) == next_word_start)) {
            stopwords_keyphrase_start = current_word_start;
          }
        }
        stopwords_keyphrase_end = NULL;
      } else {
        if (current_word_stop ||
            current_word_dict ||
            current_word_hashtag || // NOTE: using hashtag to end keyphrase
            ((current_word_len < 2) && !isdigit(*current_word_start))
           ) {
          if (stopwords_keyphrase_start != prev_word_start) {
            stopwords_keyphrase_end = prev_word_end;
          }
          else {
            stopwords_keyphrase_start = NULL;
            stopwords_keyphrase_end = NULL;
          }
        } else {
          if (' ' != current_word_delimiter ||
              NULL == next_word_start ||
              current_word_precedes_ignore_word ||
              ((current_word_end + 1) != next_word_start)) {
            if (stopwords_keyphrase_start != current_word_start) {
              stopwords_keyphrase_end = current_word_end;
            }
            else {
              stopwords_keyphrase_start = NULL;
              stopwords_keyphrase_end = NULL;
            }
          }
        }
      }
#endif // KEYPHRASE_ENABLED

#if defined NAMED_ENTITIES_ENABLED || defined TEXT_CLASSIFICATION_ENABLED
      if (NULL == caps_entity_start) {
        caps_entity_end = NULL;
        if ((current_word_len > 1 && (current_word_caps) &&
            !current_word_stop && !current_word_dict) && 
            (!prev_word_start ||
             prev_word_precedes_ignore_word ||
             prev_word_precedes_punct ||
             !prev_word_caps ||
             current_word_hashtag
            )
           ) {

          if (' ' == current_word_delimiter &&
              NULL != next_word_start &&
              ((current_word_end + 1) == next_word_start)) {
            if (current_word_start == sentence_start) {
              if (next_word_caps && !next_word_stop && !next_word_dict)
                caps_entity_start = current_word_start;
            } else {
              caps_entity_start = current_word_start;
            }
            caps_entity_end = NULL;

          } else if (prev_word_stop &&
              ('\0' == current_word_delimiter ||
              NULL == next_word_start ||
              current_word_precedes_ignore_word ||
              current_word_precedes_punct)) { 

              // this is a single word entity. so not inserting into named_entities
              // caps_entity_start = current_word_start;
              // caps_entity_end = current_word_end;
              
              // instead insert into keywords set
#ifdef KEYWORDS_ENABLED
              if ((keywords_len + current_word_len + 1) < keywords_buffer_len) {
                Insert(keywords_buffer, keywords_len, current_word_start, current_word_len, keywords_count);
              }
#endif // KEYWORDS_ENABLED
          }
        }
      } else {
#ifdef KE_DEBUG
        if (KE_DEBUG > 5) {
          cout << "caps entity candidate: " << caps_entity_start << endl;
        }
#endif // KE_DEBUG

        if (current_word_stop ||
            !current_word_caps ||
            current_word_dict ||
            ((current_word_len < 2) && !isdigit(*current_word_start))) {
          if (caps_entity_start != prev_word_start) {
            caps_entity_end = prev_word_end;
          }
          else {
            caps_entity_start = NULL;
            caps_entity_end = NULL;
          }
        } else {
          if (' ' != current_word_delimiter ||
              NULL == next_word_start ||
              current_word_precedes_ignore_word ||
              ((current_word_end + 1) != next_word_start)) {
            if (caps_entity_start != current_word_start) {
              caps_entity_end = current_word_end;
            }
            else {
              caps_entity_start = NULL;
              caps_entity_end = NULL;
            }
          }
        }
      }
#endif // NAMED_ENTITIES_ENABLED || TEXT_CLASSIFICATION_ENABLED

      unsigned int temp_len;
      temp_len = 0;

#ifdef KEYPHRASE_ENABLED
      // write keyphrases
      if (NULL != stopwords_keyphrase_start && NULL != stopwords_keyphrase_end) {

#ifdef NAMED_ENTITIES_ENABLED
        if (stopwords_keyphrase_start != caps_entity_start || stopwords_keyphrase_end != caps_entity_end) {
#endif // NAMED_ENTITIES_ENABLED

#ifdef KE_DEBUG
          if (KE_DEBUG > 1) {
            temp_len = stopwords_keyphrase_end - stopwords_keyphrase_start;
            cout << endl << string((char *) stopwords_keyphrase_start, temp_len) << " :keyphrase";
          }
#endif // KE_DEBUG

          if (strncmp((char *) stopwords_keyphrase_end-2, "\'s", 2) == 0) {
            ch = *(stopwords_keyphrase_end-2);
            *(stopwords_keyphrase_end-2) = '\0';
            temp_len = ((stopwords_keyphrase_end-2) - stopwords_keyphrase_start);
            if ((keyphrases_len + temp_len + 1) < keyphrases_buffer_len) {
              Insert(keyphrases_buffer, keyphrases_len,
                     stopwords_keyphrase_start, temp_len,
                     keyphrases_count);
            }
            *(stopwords_keyphrase_end-2) = ch;
          }
          else if ((pch = (unsigned char*) strstr((char *) stopwords_keyphrase_start, "\'s")) &&
                   (pch < stopwords_keyphrase_end)) {
            ch = *pch;
            *pch = '\0';
            // but don't insert the X in X's if X is a single word!
            if (strstr((char *) stopwords_keyphrase_start, " ")) {
              temp_len = pch - stopwords_keyphrase_start;
              if ((keyphrases_len + temp_len + 1) < keyphrases_buffer_len) {
                Insert(keyphrases_buffer, keyphrases_len,
                       stopwords_keyphrase_start, temp_len,
                       keyphrases_count);
              }
            }
            *pch = ch;
            temp_len = stopwords_keyphrase_end - stopwords_keyphrase_start;
            if ((keyphrases_len + temp_len + 1) < keyphrases_buffer_len) {
              Insert(keyphrases_buffer, keyphrases_len,
                     stopwords_keyphrase_start, temp_len,
                     keyphrases_count);
            }
          }
          else { 
            temp_len = stopwords_keyphrase_end - stopwords_keyphrase_start;
            if ((keyphrases_len + temp_len + 1) < keyphrases_buffer_len) {
              Insert(keyphrases_buffer, keyphrases_len,
                     stopwords_keyphrase_start, temp_len,
                     keyphrases_count);
            }
          }
#ifdef NAMED_ENTITIES_ENABLED
        } else {
          if (stopwords_keyphrase_start > stopwords_keyphrase_end)
            cout << "ERROR: keyphrase markers are wrong\n";
        }
#endif // NAMED_ENTITIES_ENABLED
        stopwords_keyphrase_start = NULL;
        stopwords_keyphrase_end = NULL;
      }
#endif // KEYPHRASE_ENABLED

#if defined NAMED_ENTITIES_ENABLED || defined TEXT_CLASSIFICATION_ENABLED
      // write entities
      if (NULL != stopwords_entity_start && NULL != stopwords_entity_end) {
        if (stopwords_entity_start < stopwords_entity_end) {
#ifdef KE_DEBUG
          if (KE_DEBUG > 1) {
            cout << endl << string((char *) stopwords_entity_start, (stopwords_entity_end - stopwords_entity_start)) << " :entity by stopword";
          }
#endif // KE_DEBUG
          if (strncmp((char *) stopwords_entity_end-2, "\'s", 2) == 0) {
            ch = *(stopwords_entity_end-2);
            *(stopwords_entity_end-2) = '\0';
            temp_len = (stopwords_entity_end-2) - stopwords_entity_start;
#ifdef NAMED_ENTITIES_ENABLED
            if ((named_entities_len + temp_len + 1) < named_entities_buffer_len) {
              Insert(named_entities_buffer, named_entities_len,
                     stopwords_entity_start, temp_len,
                     named_entities_count);
            }
#elif defined TEXT_CLASSIFICATION_ENABLED
            if ((text_class_words_len + temp_len + 1) < text_class_words_buffer_len) {
              Insert(text_class_words_buffer, text_class_words_len,
                     stopwords_entity_start, temp_len,
                     text_class_words_count);
            }
#endif // NAMED_ENTITIES_ENABLED elif TEXT_CLASSIFICATION_ENABLED
            *(stopwords_entity_end-2) = ch;
          }
          else if ((pch = (unsigned char*) strstr((char *) stopwords_entity_start, "\'s")) && (pch < stopwords_entity_end)) {
            ch = *pch;
            *pch = '\0';
            // but don't insert the X in X's if X is a single word!
            temp_len = pch - stopwords_entity_start;
            if (strstr((char *) stopwords_entity_start, " ")) {
#ifdef NAMED_ENTITIES_ENABLED
              if ((named_entities_len + temp_len + 1) < named_entities_buffer_len) {
                Insert(named_entities_buffer, named_entities_len,
                       stopwords_entity_start, temp_len,
                       named_entities_count);
              }
#elif defined TEXT_CLASSIFICATION_ENABLED
              if ((text_class_words_len + temp_len + 1) < text_class_words_buffer_len) {
                Insert(text_class_words_buffer, text_class_words_len,
                       stopwords_entity_start, temp_len,
                       text_class_words_count);
              }
#endif // NAMED_ENTITIES_ENABLED elif TEXT_CLASSIFICATION_ENABLED
            } else {
#ifdef KEYWORDS_ENABLED
              if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
                Insert(keywords_buffer, keywords_len,
                       stopwords_entity_start, temp_len,
                       keywords_count);
              }
#elif defined TEXT_CLASSIFICATION_ENABLED
              if ((text_class_words_len + temp_len + 1) < text_class_words_buffer_len) {
                Insert(text_class_words_buffer, text_class_words_len,
                       stopwords_entity_start, temp_len,
                       text_class_words_count);
              }
#endif // KEYWORDS_ENABLED || TEXT_CLASSIFICATION_ENABLED
            }

            *pch = ch;
            temp_len = stopwords_entity_end - stopwords_entity_start;
#ifdef NAMED_ENTITIES_ENABLED
            if ((named_entities_len + temp_len + 1) < named_entities_buffer_len) {
              Insert(named_entities_buffer, named_entities_len,
                     stopwords_entity_start, temp_len,
                     named_entities_count);
            }
#elif defined TEXT_CLASSIFICATION_ENABLED
            if ((text_class_words_len + temp_len + 1) < text_class_words_buffer_len) {
              Insert(text_class_words_buffer, text_class_words_len,
                     stopwords_entity_start, temp_len,
                     text_class_words_count);
            }
#endif // NAMED_ENTITIES_ENABLED elif TEXT_CLASSIFICATION_ENABLED
          } else {
            temp_len = stopwords_entity_end - stopwords_entity_start;
#ifdef NAMED_ENTITIES_ENABLED
            if ((named_entities_len + temp_len + 1) < named_entities_buffer_len) {
              Insert(named_entities_buffer, named_entities_len,
                     stopwords_entity_start, temp_len,
                     named_entities_count);
            }
#elif defined TEXT_CLASSIFICATION_ENABLED
            if ((text_class_words_len + temp_len + 1) < text_class_words_buffer_len) {
              Insert(text_class_words_buffer, text_class_words_len,
                     stopwords_entity_start, temp_len,
                     text_class_words_count);
            }
#endif // NAMED_ENTITIES_ENABLED elif TEXT_CLASSIFICATION_ENABLED
          }
        } else {
          cout << "ERROR: stopwords entity markers are wrong\n";
        }
        stopwords_entity_start = NULL;
        stopwords_entity_end = NULL;
      }

      //  note, keyphrase code above refers to start of caps_entity_start; any change here, will also affect that
      if (NULL != caps_entity_start && NULL != caps_entity_end) {
        if (caps_entity_start < caps_entity_end) {
#ifdef KE_DEBUG
          if (KE_DEBUG > 2) {
            cout << endl \
                 << string((char *) caps_entity_start, (caps_entity_end - caps_entity_start)) \
                 << " :entity by caps";
          }
#endif // KE_DEBUG
          if (strncmp((char *) caps_entity_end-2, "\'s", 2) == 0) {
            ch = *(caps_entity_end-2);
            *(caps_entity_end-2) = '\0';
            temp_len = (caps_entity_end-2) - caps_entity_start;
#ifdef NAMED_ENTITIES_ENABLED
            if ((named_entities_len + temp_len + 1) < named_entities_buffer_len) {
              Insert(named_entities_buffer, named_entities_len,
                     caps_entity_start, temp_len,
                     named_entities_count);
            }
#elif defined TEXT_CLASSIFICATION_ENABLED
            if ((text_class_words_len + temp_len + 1) < text_class_words_buffer_len) {
              Insert(text_class_words_buffer, text_class_words_len,
                     caps_entity_start, temp_len,
                     text_class_words_count);
            }
#endif // NAMED_ENTITIES_ENABLED elif TEXT_CLASSIFICATION_ENABLED

            *(caps_entity_end-2) = ch;
          }
          else if ((pch = (unsigned char*) strstr((char *) caps_entity_start, "\'s"))
                    && (pch < caps_entity_end)) {
            ch = *pch;
            *pch = '\0';
            // don't insert the X in X's if X is a single word!
            temp_len = pch - caps_entity_start;
            if (strstr((char *) caps_entity_start, " ")) {
#ifdef NAMED_ENTITIES_ENABLED
              if ((named_entities_len + temp_len + 1) < named_entities_buffer_len) {
                Insert(named_entities_buffer, named_entities_len,
                       caps_entity_start, temp_len,
                       named_entities_count);
              }
#elif defined TEXT_CLASSIFICATION_ENABLED
              if ((text_class_words_len + temp_len + 1) < text_class_words_buffer_len) {
                Insert(text_class_words_buffer, text_class_words_len,
                       caps_entity_start, temp_len,
                       text_class_words_count);
              }
#endif // NAMED_ENTITIES_ENABLED elif TEXT_CLASSIFICATION_ENABLED
            } else {
              // but such single word X's can be keywords
#ifdef KEYWORDS_ENABLED
              if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
                Insert(keywords_buffer, keywords_len,
                       caps_entity_start, temp_len,
                       keywords_count);
              }
#endif // KEYWORDS_ENABLED
            }

            *pch = ch;
            temp_len = caps_entity_end - caps_entity_start;
#ifdef NAMED_ENTITIES_ENABLED
            if ((named_entities_len + temp_len + 1) < named_entities_buffer_len) {
              Insert(named_entities_buffer, named_entities_len,
                     caps_entity_start, temp_len,
                     named_entities_count);
            }
#elif defined TEXT_CLASSIFICATION_ENABLED
            if ((text_class_words_len + temp_len + 1) < text_class_words_buffer_len) {
              Insert(text_class_words_buffer, text_class_words_len,
                     caps_entity_start, temp_len,
                     text_class_words_count);
            }
#endif // NAMED_ENTITIES_ENABLED elif TEXT_CLASSIFICATION_ENABLED
          } else { 
            temp_len = caps_entity_end - caps_entity_start;
#ifdef NAMED_ENTITIES_ENABLED
            if ((named_entities_len + temp_len + 1) < named_entities_buffer_len) {
              Insert(named_entities_buffer, named_entities_len,
                     caps_entity_start, temp_len,
                     named_entities_count);
            }
#elif defined TEXT_CLASSIFICATION_ENABLED
            if ((text_class_words_len + temp_len + 1) < text_class_words_buffer_len) {
              Insert(text_class_words_buffer, text_class_words_len,
                     caps_entity_start, temp_len,
                     text_class_words_count);
            }
#endif // NAMED_ENTITIES_ENABLED elif TEXT_CLASSIFICATION_ENABLED
          }
        } else {
          cout << "ERROR: caps entity markers are wrong\n";
        }
        caps_entity_start = NULL;
        caps_entity_end = NULL;
      }
#endif // NAMED_ENTITIES_ENABLED || TEXT_CLASSIFICATION_ENABLED

#ifdef KEYWORDS_ENABLED 
      // hash tags
      if (current_word_hashtag) {
        if ((keywords_len + current_word_len + 1) < keywords_buffer_len) {
          Insert(keywords_buffer, keywords_len,
                 current_word_start, current_word_len,
                 keywords_count); 
        }
      }
      if (!current_word_stop && !current_word_dict && current_word_has_apostropheS) {
        if ((keywords_len + current_word_len - 1) < keywords_buffer_len) {
          Insert(keywords_buffer, keywords_len,
                 current_word_start, current_word_len-2,
                 keywords_count); 
        }
      }
#elif TEXT_CLASSIFICATION_ENABLED
      if (current_word_hashtag) {
        if ((text_class_words_len + current_word_len + 1) < text_class_words_buffer_len) {
          Insert(text_class_words_buffer, text_class_words_len,
                 current_word_start, current_word_len,
                 text_class_words_count); 
        }
      }
      if (!current_word_stop && !current_word_dict && current_word_has_apostropheS) {
        if ((text_class_words_len + current_word_len - 1) < text_class_words_buffer_len) {
          Insert(text_class_words_buffer, text_class_words_len,
                 current_word_start, current_word_len-2,
                 text_class_words_count); 
        }
      }
#endif // KEYWORDS_ENABLED elif TEXT_CLASSIFICATION_ENABLED

#ifndef I18N_ENABLED
      }
#endif // I18N_ENABLED

#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        cout << endl;
      }
#endif // KE_DEBUG

      // exit conditions
      if ('\0' == current_word_delimiter || !next_word_start || '\0' == *next_word_start) {
        if (current_word_end)
          *current_word_end = current_word_delimiter;
        break;
      }

      // ****************************** beginning of next cycle ******************************* //

      prev_word_end = current_word_end;
      prev_word_start = current_word_start;

      prev_word_caps = current_word_caps;
      prev_word_has_mixed_case = current_word_has_mixed_case;
      prev_word_all_caps = current_word_all_caps;
      prev_word_stop = current_word_stop;
      prev_word_dict = current_word_dict;
      prev_word_starts_num = current_word_starts_num;
      prev_word_delimiter = current_word_delimiter;
      prev_word_precedes_ignore_word = current_word_precedes_ignore_word;
      prev_word_precedes_punct = current_word_precedes_punct;
      prev_word_has_apostropheS = current_word_has_apostropheS;
      prev_word_hashtag = current_word_hashtag;

      current_word_end = next_word_end;
      current_word_start = next_word_start;

      current_word_caps = next_word_caps;
      current_word_has_mixed_case = next_word_has_mixed_case; 
      current_word_all_caps = next_word_all_caps;
      current_word_stop = next_word_stop;
      current_word_dict = next_word_dict;
      current_word_starts_num = next_word_starts_num;
      current_word_delimiter = next_word_delimiter;
      current_word_precedes_ignore_word = next_word_precedes_ignore_word;
      current_word_precedes_punct = next_word_precedes_punct;
      current_word_len = next_word_len;
      current_word_ascii = next_word_ascii;
      current_word_has_apostropheS = next_word_has_apostropheS;
      current_word_hashtag = next_word_hashtag;

      next_word_start = NULL;
      next_word_end = NULL;
      next_word_caps = false;
      next_word_has_mixed_case = false;
      next_word_all_caps = false;
      next_word_stop = false;
      next_word_dict = false;
      next_word_starts_num = false;
      next_word_delimiter = '\0';
      next_word_precedes_ignore_word = false;
      next_word_precedes_punct = false;
      next_word_ascii = false;
      next_word_has_apostropheS = NULL;
      next_word_hashtag = false;

      // BE CAREFUL ABOUT WHAT IS NEXT WORD OR CURRENT WORD NOW

      // if current word is not the known last word, briefly probe to see if next word exists
      if ('\0' != current_word_delimiter) {
        ptr = probe + 1;
        if (!ptr) {
          std::cerr << "ERROR: Fatal Exception trying to access unallocated memory space\n";
          return -1;
        }

        // find the next position of ptr
        // IsIgnore will literally ignore the word by changing the cursor to next word end
        is_ignore_word = false;
        is_punct = false;
        while (('\0' != *ptr)
               && (' ' == *ptr
                   || (ispunct(*ptr) &&
                      (is_punct = IsPunct((char *&) ptr, (char *) ptr-1, (char *) ptr+1))
                      )
                   || (is_ignore_word = IsIgnore((char *&) ptr))
                  )
              ) {
          current_word_precedes_ignore_word |= is_ignore_word;
          current_word_precedes_punct |= is_punct; 
          ptr++;
        }

        if (ptr && '\0' != *ptr) {
          if ('#' == *ptr) {
            next_word_hashtag = true;
            ptr++;
          }
          next_word_start = ptr;
          num_words++;
          if (current_word_precedes_ignore_word) {
            sentence_start = next_word_start;
#ifdef KE_DEBUG
            if (KE_DEBUG > 5) {
              cout << "sentence start: " << sentence_start << endl;
            }
#endif // KE_DEBUG
          }

          if (current_word_precedes_punct) {
            sentence_start = next_word_start;
#ifdef KE_DEBUG
            if (KE_DEBUG > 5) {
              cout << "sentence start: " << sentence_start << endl;
            }
#endif // KE_DEBUG
            if (':' == current_word_delimiter ||
                '>' == current_word_delimiter ||
                '-' == current_word_delimiter ||
                '(' == current_word_delimiter) {
              if (num_normal_words == 0) {
#ifdef NAMED_ENTITIES_ENABLED
                *named_entities_buffer = '\0';
                named_entities_len = 0;
#endif // NAMED_ENTITIES_ENABLED
#if defined NAMED_ENTITIES_ENABLED || defined TEXT_CLASSIFICATION_ENABLED
                caps_entity_start = NULL;
                caps_entity_end = NULL;
                stopwords_entity_start = NULL;
                stopwords_entity_end = NULL;
#endif // NAMED_ENTITIES_ENABLED || TEXT_CLASSIFICATION_ENABLED
#ifdef KEYPHRASE_ENABLED
                stopwords_keyphrase_start = NULL;
                stopwords_keyphrase_end = NULL;
#endif // KEYPHRASE_ENABLED
              }
            } else {
#if defined NAMED_ENTITIES_ENABLED || defined TEXT_CLASSIFICATION_ENABLED
              for (pch = current_word_end + 1; (pch != next_word_start); pch++) {
                if (':' == *pch || '>' == *pch || '-' == *pch || '(' == *pch) {
                  if (num_normal_words == 0) {
#ifdef NAMED_ENTITIES_ENABLED
                    *named_entities_buffer = '\0';
                    named_entities_len = 0;
#endif // NAMED_ENTITIES_ENABLED
                    caps_entity_start = NULL;
                    caps_entity_end = NULL;
                    stopwords_entity_start = NULL;
                    stopwords_entity_end = NULL;
#ifdef KEYPHRASE_ENABLED
                    stopwords_keyphrase_start = NULL;
                    stopwords_keyphrase_end = NULL;
#endif // KEYPHRASE_ENABLED
#ifdef INTENT_ENABLED
                    intent_start = NULL;
#endif // INTENT_ENABLED
                  }
                }
              }
#endif // NAMED_ENTITIES_ENABLED || TEXT_CLASSIFICATION_ENABLED
            }
          }

          // after finding the start of next word, probe shud be at the same place as ptr
          probe = ptr;

          if (isupper(*next_word_start)) {
            next_word_caps = true;
            num_caps_words++;
            if (isupper(*(next_word_start+1))) {
              next_word_all_caps = true;
            }
            //*next_word_start += 32;
          } else if (isdigit(*next_word_start)) {
            next_word_starts_num = true;
            num_numeric_words++;
          }
        } else {
          // placing the probe before '/0' so that loop will make it probe++
          // loop will terminate in the next cycle
          probe = ptr-1;
        }
      } // check for current word delimiter 

    } else {

      if (isupper(*probe)) {
        if (!next_word_all_caps) {
          next_word_has_mixed_case = true;
        }
        //*probe += 32;
      } else if (islower(*probe)) {
        if (next_word_all_caps) {
          next_word_has_mixed_case = true;
          next_word_all_caps = false;
        }
      }

    }

    // a mere cog in a loop wheel, but a giant killer if commented
    if (probe && *probe != '\0') {
      //if (script_count > 9 || english_count > 20) {
        //probe++;
      //} else {
        try {
          code_point = utf8::next(probe, end);
          if (code_point > 0xFF) {
            if (inagist_classifiers::DetectScript(code_point, script_temp) > 0) {
              if (script_temp != "en") {
                if (script_temp != script) {
                  script_count = 0;
                  script = script_temp;
                } else {
                  script_count++;
                }
              }
            }
          } else {
            if (code_point > 0x40 && code_point < 0x7B)
              english_count++;
          }
        } catch (...) {
#ifdef KE_DEBUG
          if (KE_DEBUG > 0) {
            std::cout << "Exception 5: " << code_point << " " << probe << std::endl;
            cout << endl << "original query: " << std::string((char *) text_buffer) << endl << endl;
          }
#endif // KE_DEBUG
          return -1;
        }
      //}
    }
  }
  } // end of gigantic if

#ifndef LANG_ENABLED
  if (num_mixed_words > 2) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0) {
      cout << "suspected non-english tweet. ignoring named_entities and keyphrases" << endl;
      cout << "num mixed words: " << num_mixed_words << endl;
    }
#endif // KE_DEBUG
#ifdef NAMED_ENTITIES_ENABLED
    *named_entities_buffer = '\0';
    named_entities_len = 0;
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYPHRASE_ENABLED
    *keyphrases_buffer = '\0';
    keyphrases_len = 0;
#endif // KEYPHRASE_ENABLED
  }
#endif // LANG_ENABLED

#ifdef KE_DEBUG
  if (KE_DEBUG > 5) {
    cout << endl << "\norginal query: " << std::string((char *) text_buffer) << endl;
    cout << "num words: " << num_words << endl;
    cout << "num caps words: " << num_caps_words << endl;
    cout << "num stop words: " << num_stop_words << endl;
    cout << "num dict words: " << num_dict_words << endl;
    cout << "num numeric words: " << num_numeric_words << endl;
    cout << "num mixed words: " << num_mixed_words << endl;
    cout << "num normal words: " << num_normal_words << endl;
  }
#endif // KE_DEBUG

#ifdef NAMED_ENTITIES_ENABLED
  if ((num_normal_words == 0) && (num_dict_words != 0 || num_words > 5)) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0) {
      cout << "no normal words. ignoring named_entities." << endl;
      cout << "num normal words: " << num_normal_words << endl;
      cout << "num words: " << num_words << endl;
      cout << "num dict words: " << num_dict_words << endl;
    }
#endif // KE_DEBUG
    *named_entities_buffer = '\0';
    named_entities_len = 0;
    named_entities_count = 0;
  }
#endif // NAMED_ENTITIES_ENABLED

  // prepare for return
  int ret_val = 0;

  // safe status
  if (text_has_unsafe_words)
    strcpy(safe_status_buffer, "unsafe");
  else
    strcpy(safe_status_buffer, "safe");

  if (script_count == 0 && english_count > 10) {
    script = "en";
  } else if (script_count > 0 && (script_count < 11 || script_count < english_count)) {
    script = "UU";
  }

  strcpy(script_buffer, script.c_str());

#ifdef NAMED_ENTITIES_ENABLED
  ret_val += named_entities_count;
#endif // NAMED_ENTITIES_ENABLED

#ifdef KEYWORDS_ENABLED
  ret_val += keywords_count;
#endif // KEYWORDS_ENABLED

#ifdef KEYPHRASE_ENABLED
  ret_val += keyphrases_count;
#endif // KEYPHRASE_ENABLED

  std::set<std::string>::iterator words_iter;
  std::string word;

#ifdef LANG_ENABLED
  for (words_iter = lang_words_set.begin(); words_iter != lang_words_set.end(); words_iter++) {
    word.assign(*words_iter);
    Insert(lang_words_buffer, lang_words_len, word, lang_words_count);
    word.clear();
  }
  ret_val += lang_words_count;
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  for (words_iter = text_class_words_set.begin();
       words_iter != text_class_words_set.end();
       words_iter++) {
    word.assign(*words_iter);
    Insert(text_class_words_buffer, text_class_words_len, word, text_class_words_count);
    word.clear();
  }
#ifdef NAMED_ENTITIES_ENABLED
  if (named_entities_count > 0) {
    strncpy((char *) text_class_words_buffer + text_class_words_len,
            (const char*) named_entities_buffer,
            named_entities_len);
    text_class_words_len += named_entities_len;
    text_class_words_count += named_entities_count;
  }
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
  if (keywords_count > 0) {
    strncpy((char *) text_class_words_buffer + text_class_words_len,
            (const char*) keywords_buffer,
            keywords_len);
    text_class_words_len += keywords_len;
    text_class_words_count += keywords_count;
  }
#endif // KEYWORDS_ENABLED
  ret_val += text_class_words_count;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef SENTIMENT_ENABLED
  if (sentiment_valence == 0) {
    strcpy(sentiment_buffer, "neutral");
  } else if (sentiment_valence < 0) {
    strcpy(sentiment_buffer, "negative");
  } else {
    strcpy(sentiment_buffer, "positive");
  }
  ret_val += 1;
#endif // SENTIMENT_ENABLED

#ifdef INTENT_ENABLED
  std::map<std::string, int>::iterator intent_iter;
  for (intent_iter = intent_words.begin(); intent_iter != intent_words.end(); intent_iter++) {
    intent_valence += intent_iter->second;
  }
  if (intent_valence < 4) {
    strcpy(intent_buffer, "none");
  } else {
    strcpy(intent_buffer, "present");
    ret_val += 1;
  }
#endif // INTENT_ENABELD

#ifdef KE_DEBUG
  if (KE_DEBUG) {
    std::cout << "keytuples_extracter summary:" << std::endl;
    std::cout << "input: " << text_buffer << std::endl;
#ifdef NAMED_ENTITIES_ENABLED
    std::cout << "named_entities (" << named_entities_count << "): " << named_entities_buffer << std::endl;
#endif // NAMED_ENTITIES_ENABLED
#ifdef KEYWORDS_ENABLED
    std::cout << "keywords (" << keywords_count << "): " << keywords_buffer << std::endl;
#endif // KEYWORDS_ENABLED
#ifdef KEYPHRASE_ENABLED
    std::cout << "keyphrases (" << keyphrases_count << "): " << keyphrases_buffer << std::endl;
#endif // KEYPHRASE_ENABLED
#ifdef LANG_ENABLED
    std::cout << "langwords (" << lang_words_count << "): " << lang_words_buffer << std::endl;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    std::cout << "text_class_words (" << text_class_words_count << "): " << text_class_words_buffer << std::endl;
#endif // TEXT_CLASSIFICATION_ENABLED
    std::cout << "ret_val: " << ret_val << std::endl;
  }
#endif // KE_DEBUG

  return ret_val;

}

} // namespace inagist_trends

