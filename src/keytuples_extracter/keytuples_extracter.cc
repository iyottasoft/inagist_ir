#include "keytuples_extracter.h"
#include "script_detector_utils.h"
#include <cstring>
#include "utf8.h"
#include "keytuples_config.h"

#ifdef DEBUG
#if DEBUG>0
#define KE_DEBUG DEBUG
#endif
#endif
//#define KE_DEBUG 5

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
  m_language_detector = (inagist_classifiers::LanguageDetector*) new inagist_classifiers::LanguageDetector;
}

KeyTuplesExtracter::~KeyTuplesExtracter() {
  if (DeInit() < 0)
    std::cerr << "ERROR: DeInit() failed\n";
}

int KeyTuplesExtracter::Init(std::string config_file) {

  if (config_file.size() < 5) {
    std::cerr << "ERROR: invalid config file name\n";
    return -1;
  }

  inagist_trends::Config config;
  if (inagist_trends::KeyTuplesConfig::Read(config_file.c_str(), config) < 0) {
    std::cerr << "ERROR: could not read config file: " << config_file << std::endl;
    return -1;
  }

  if (Init(config.stopwords_file.c_str(),
           config.dictionary_file.c_str(),
           config.unsafe_dictionary_file.c_str(),
           config.lang_detect_config_file.c_str(),
           NULL,
           config.stemmer_dictionary_file.c_str()) < 0) {
    std::cerr << "ERROR: could not initialize KeyTuplesExtracter\n";
    return -1;
  }
  inagist_trends::KeyTuplesConfig::Clear(config);

  return 0;
}

// every input parameter is optional!
//
// stopwords, dictionary and stemmer dictionary file paths, if not given will
// just mean that those dictionaries will not be populated
// 
// if input_file or output_file are given, they will be initialized
// the above two are typically used by a test program which will
// subsequently call GetKeyTuples(), GetKeywords and PrintKeywords()
//
int KeyTuplesExtracter::Init(const char *stopwords_file,
    const char *dictionary_file,
    const char *unsafe_dictionary_file,
    const char *lang_detect_config_file,
    const char *channels_dictionary_file,
    const char *stemmer_dictionary_file,
    const char *input_file,
    const char *output_file) {

  // load dictionaries
  if (stopwords_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: loading stopwords file - " << stopwords_file << std::endl;
    }
#endif
    int ret = m_stopwords_dictionary.Load(stopwords_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load stopwords file into dictionary\n";
      return -1;
    }
  }

  if (dictionary_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: loading dictionary file - " << dictionary_file << std::endl;
    }
#endif
    int ret = m_dictionary.Load(dictionary_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load dictionary file into dictionary\n";
      return -1;
    }
  }

  if (unsafe_dictionary_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: loading unsafe dictionary file - " << unsafe_dictionary_file << std::endl;
    }
#endif
    int ret = m_unsafe_dictionary.Load(unsafe_dictionary_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load dictionary file into dictionary\n";
      return -1;
    }
  }

#ifdef LANG_DETECT
  if (lang_detect_config_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: initialising lang detect with config file - " << lang_detect_config_file << std::endl;
    }
#endif
    if (m_language_detector->Init(std::string(lang_detect_config_file)) < 0) {
      std::cerr << "ERROR: could not initialize lang detect\n";
      return -1;
    }
  }
#endif

/*
  if (channels_dictionary_file) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 3) {
      std::cout << "Info: loading channels dictionary file - " << channels_dictionary_file << std::endl;
    }
#endif
    int ret = m_channels_dictionary_map.Load(channels_dictionary_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load channels file into dictionary\n";
      return -1;
    }
  }
*/

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

  if (m_language_detector) {
    delete m_language_detector;
    m_language_detector = NULL;
  }
  //std::cout << "deinit done\n";
  return 0;
}

void KeyTuplesExtracter::PrintKeywords(std::set<std::string> &keywords_set) {
  std::set<std::string>::iterator iter;

  for (iter = keywords_set.begin(); iter != keywords_set.end(); iter++)
//  std::cout << *iter << " ";
//std::cout << std::endl;
    std::cout << *iter << std::endl;
}

// this function isn't unicode safe
// TODO (balaji) for ascii, we can ofcourse use an array lookup to speed up
bool KeyTuplesExtracter::IsPunct(char *ptr, char *prev, char *next) {

  if (!ptr || *ptr == ' ' || *ptr == '\0')
    return true;

  if (!ispunct(*ptr))
    return false;

  switch (*ptr) {
    case '\'':
      if (prev)
        if (!IsPunct(prev) &&
            (!strncmp(ptr, "'s", 2) || !strncmp(ptr, "'t", 2) || !strncmp(ptr, "'ve", 2) ||
             !strncmp(ptr, "'ll", 2) || !strncmp(ptr, "'re", 2) || !strncmp(ptr, "'m", 2) ||
             !strncmp(ptr, "'em", 3))
           )
          return false;
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
      if (next)
        if (*next == '#' && isdigit(*(next+1)))
          return false;
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
  if ('@' == *ptr || !strncmp(ptr, "&#", 2) || !strncmp(ptr, "http://", 7) || !strncmp(ptr, "www.", 4)) {
    while (' ' != *(ptr+1) && '\0' != *(ptr+1)) {
      ptr++;
    }
    return true;
  }
  return false;
}

int KeyTuplesExtracter::GetKeywords(char* str, std::set<std::string>& keywords_set) {
  std::set<std::string> keyphrases_set;
  std::string safe_status;
  std::string script;
  std::string lang;
#ifndef HASHTAGS_ENABLED
  return GetKeyTuples(str, safe_status, script, lang, keywords_set, keyphrases_set);
#else
  std::set<std::string> hashtags_set;
  return GetKeyTuples(str, safe_status, script, lang, keywords_set, hashtags_set, keyphrases_set);
#endif
}

#ifdef KEYPHRASE_ENABLED
int KeyTuplesExtracter::GetKeywords(char* str,
                                  std::string& safe_status,
                                  std::string& script,
                                  std::set<std::string>& keywords_set) {
  std::string lang;
  std::set<std::string> keyphrases_set;
#ifndef HASHTAGS_ENABLED
  return GetKeyTuples(str, safe_status, script, lang, keywords_set, keyphrases_set);
#else
  std::set<std::string> hashtags_set;
  return GetKeyTuples(str, safe_status, script, lang, keywords_set, hashtags_set, keyphrases_set);
#endif
}
#endif

int KeyTuplesExtracter::GetKeywords(char* str,
                                   std::string& user,
                                   std::set<std::string>& keywords_set,
                                   std::map<std::string, std::string>& script_user_map,
                                   std::map<std::string, std::string>& keyword_user_map) {
  std::string safe_status;
  std::string script;
  if (GetKeywords(str, safe_status, script, keywords_set) < 0) {
    cout << "ERROR: could not get keywords\n";
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
  for (set_iter = keywords_set.begin(); set_iter != keywords_set.end(); set_iter++) {
    if ((map_iter = keyword_user_map.find(*set_iter)) != keyword_user_map.end()) {
      keyword_user_map[*set_iter]+= ", " + user;
    } else {
      keyword_user_map[*set_iter] = user;
    }
  }

  return 0;
}

#ifdef KEYPHRASE_ENABLED
int KeyTuplesExtracter::GetKeywords(char* str,
                                 std::string& script,
                                 std::set<std::string>& keywords_set) {
  std::set<std::string> keyphrases_set;
  std::string safe_status;
  std::string lang;
  int ret_value = 0;
#ifndef HASHTAGS_ENABLED
  GetKeyTuples(str, safe_status, script, lang, keywords_set, keyphrases_set);
#endif
  keyphrases_set.clear();
  return ret_value;
}
#endif

#ifdef KEYPHRASE_ENABLED
#ifdef HASHTAGS_ENABLED
int KeyTuplesExtracter::GetKeyTuples(char* str, 
                                   std::string& safe_status,
                                   std::string& script,
                                   std::string& lang,
                                   std::set<std::string>& keywords_set,
                                   std::set<std::string>& hashtags_set,
                                   std::set<std::string>& keyphrases_set) {
#else
int KeyTuplesExtracter::GetKeyTuples(char* str, 
                                   std::string& safe_status,
                                   std::string& script,
                                   std::string& lang,
                                   std::set<std::string>& keywords_set,
                                   std::set<std::string>& keyphrases_set) {
  std::set<std::string> hashtags_set;
#endif
#else
#ifdef HASHTAGS_ENABLED
int KeyTuplesExtracter::GetKeyTuples(char* str, 
                                   std::string& safe_status,
                                   std::string& script,
                                   std::string& lang,
                                   std::set<std::string>& keywords_set,
                                   std::set<std::string>& hashtags_set) {
  std::set<std::string> keyphrases_set;
#else
int KeyTuplesExtracter::GetKeyTuples(char* str, 
                                   std::string& safe_status,
                                   std::string& script,
                                   std::string& lang,
                                   std::set<std::string>& keywords_set) {
  std::set<std::string> hashtags_set;
  std::set<std::string> keyphrases_set;
#endif
#endif

  unsigned int buffer_len = strlen(str);
  if (buffer_len > MAX_DEBUG_BUFFER_LEN) {
    std::cout << "ERROR: invalid input size\n";
    return -1;
  }

  unsigned char buffer[MAX_DEBUG_BUFFER_LEN];
  memset(buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  strcpy((char *) buffer, str);
  char safe_status_buffer[10];
  memset(safe_status_buffer, '\0', 10);
  unsigned int safe_status_buffer_len = 10;
  char script_buffer[4];
  unsigned int script_buffer_len = 4;
  memset(script_buffer, '\0', 4);
  unsigned char keywords_buffer[MAX_DEBUG_BUFFER_LEN];
  memset(keywords_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  unsigned int keywords_buffer_len = MAX_DEBUG_BUFFER_LEN;
  unsigned char hashtags_buffer[MAX_DEBUG_BUFFER_LEN];
  memset(hashtags_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  unsigned int hashtags_buffer_len = MAX_DEBUG_BUFFER_LEN;
  unsigned char keyphrases_buffer[MAX_DEBUG_BUFFER_LEN];
  memset(keyphrases_buffer, '\0', MAX_DEBUG_BUFFER_LEN);
  unsigned int keyphrases_buffer_len = MAX_DEBUG_BUFFER_LEN;
  unsigned int keywords_len = 0;
  unsigned int keywords_count = 0;
  unsigned int hashtags_len = 0;
  unsigned int hashtags_count = 0;
  unsigned int keyphrases_len = 0;
  unsigned int keyphrases_count = 0;
  char buffer1[4];
  unsigned int buffer1_len = 4;
  memset(buffer1, '\0', 4);
  char buffer2[4];
  unsigned int buffer2_len = 4;
  memset(buffer2, '\0', 4);
  char buffer3[4];
  unsigned int buffer3_len = 4;
  memset(buffer3, '\0', 4);
  char buffer4[4];
  unsigned int buffer4_len = 4;
  memset(buffer4, '\0', 4);

  int count = 0;

  if ((count = GetKeyTuples(buffer, buffer_len,
                  safe_status_buffer, safe_status_buffer_len,
                  script_buffer, script_buffer_len,
                  keywords_buffer, keywords_buffer_len,
                  keywords_len, keywords_count,
                  hashtags_buffer, hashtags_buffer_len,
                  hashtags_len, hashtags_count,
                  keyphrases_buffer, keyphrases_buffer_len,
                  keyphrases_len, keyphrases_count,
                  buffer1, buffer1_len,
                  buffer2, buffer2_len,
                  buffer3, buffer3_len,
                  buffer4, buffer4_len)) < 0) {
    std::cout << "ERROR: could not get keywords\n";
    buffer[0] = '\0';
    keywords_buffer[0] = '\0';
    hashtags_buffer[0] = '\0';
    keyphrases_buffer[0] = '\0';
    buffer1[0] = '\0';
    buffer2[0] = '\0';
    buffer3[0] = '\0';
    buffer4[0] = '\0';
    return -1;
  }

  buffer[0] = '\0';

  script.assign(script_buffer, script_buffer_len);
  lang.assign(buffer1, buffer1_len);
  safe_status.assign(safe_status_buffer, safe_status_buffer_len);

  unsigned char* pch1 = NULL;
  unsigned char* pch2 = NULL;
  unsigned char ch;
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

  if (hashtags_len > 0) {
    pch1 = hashtags_buffer;
    pch2 = pch1;
    pch1 = (unsigned char*) strchr((char *) pch2, '|');
    while (pch1 && pch1 != '\0') {
      ch = *pch1;
      *pch1 = '\0';
      hashtags_set.insert((char *) pch2);
      *pch1 = ch;
      pch2 = pch1 + 1;
      pch1 = (unsigned char*) strchr((char *) pch2, '|');
    }
  }

  keywords_buffer[0] = '\0';
  hashtags_buffer[0] = '\0';
  keyphrases_buffer[0] = '\0';
  buffer1[0] = '\0';
  buffer2[0] = '\0';
  buffer3[0] = '\0';
  buffer4[0] = '\0';

  return count;
}

#ifdef KEYPHRASE_ENABLED
int KeyTuplesExtracter::GetKeyTuples(unsigned char* buffer, const unsigned int& buffer_len,
                                   char* safe_status_buffer, const unsigned int& safe_status_buffer_len,
                                   char* script_buffer, const unsigned int& script_buffer_len,
                                   unsigned char* keywords_buffer, const unsigned int& keywords_buffer_len,
                                   unsigned int& keywords_len, unsigned int& keywords_count,
                                   unsigned char* hashtags_buffer, const unsigned int& hashtags_buffer_len,
                                   unsigned int& hashtags_len, unsigned int& hashtags_count,
                                   unsigned char* keyphrases_buffer, const unsigned int& keyphrases_buffer_len,
                                   unsigned int& keyphrases_len, unsigned int& keyphrases_count,
                                   char* buffer1, const unsigned int& buffer1_len,
                                   char* buffer2, const unsigned int& buffer2_len,
                                   char* buffer3, const unsigned int& buffer3_len,
                                   char* buffer4, const unsigned int& buffer4_len) {

  // initialize output parameters
  *safe_status_buffer = '\0';
  *script_buffer = '\0';
  *keywords_buffer = '\0';
  keywords_len = 0;
  keywords_count = 0;
  *hashtags_buffer = '\0';
  hashtags_len = 0;
  hashtags_count = 0;
  *keyphrases_buffer = '\0';
  keyphrases_len = 0;
  keyphrases_count = 0;
  *buffer1 = '\0';
  *buffer2 = '\0';
  *buffer3 = '\0';
  *buffer4 = '\0';

  if (!buffer || buffer_len < 1 || !script_buffer || !keywords_buffer ||
      !hashtags_buffer || !keyphrases_buffer || !buffer1 || !buffer2 || !buffer3 || !buffer4) {
    std::cout << "ERROR: invalid buffer(s) at input\n";
    return -1;
  }
#else
int KeyTuplesExtracter::GetKeyTuples(unsigned char* buffer, const unsigned int& buffer_len,
                                   char* safe_status_buffer, const unsigned int& safe_status_buffer_len,
                                   char* script_buffer, const unsigned int& script_buffer_len,
                                   unsigned char* keywords_buffer, const unsigned int& keywords_buffer_len,
                                   unsigned int& keywords_len, unsigned int& keywords_count,
                                   char* buffer1, const unsigned int& buffer1_len,
                                   char* buffer2, const unsigned int& buffer2_len,
                                   char* buffer3, const unsigned int& buffer3_len,
                                   char* buffer4, const unsigned int& buffer4_len) {

  *safe_status_buffer = '\0';
  *script_buffer = '\0';
  *keywords_buffer = '\0';
  keywords_len = 0;
  keywords_count = 0;
  *buffer1 = '\0';
  *buffer2 = '\0';
  *buffer3 = '\0';
  *buffer4 = '\0';

  if (!buffer || buffer_len < 1 || !script_buffer || !keywords_buffer ||
      !buffer1 || !buffer2 || !buffer3 || !buffer4) {
    std::cout << "ERROR: invalid buffer(s) at input\n";
    return -1;
  }
#endif

  unsigned char *ptr = NULL;
  unsigned char *probe = NULL;
  unsigned char current_word_delimiter;
  unsigned char prev_word_delimiter;
  unsigned char next_word_delimiter;

  //unsigned in_len = 0;
  //unsigned out_len = 0;
  unsigned int current_word_len = 0;
  unsigned int next_word_len = 0;
#ifdef KE_DEBUG
  int score = 0;
#endif
  int num_mixed_words = 0;
  int num_caps_words = 0;
  int num_words = 0;
  int num_stop_words = 0;
  int num_dict_words = 0;
  int num_numeric_words = 0;
  int num_normal_words = 0; // not caps or stop or dict or numeric
  std::set<std::string> words_set;

  unsigned char *current_word_start = NULL;
  unsigned char *current_word_end = NULL;
  unsigned char *prev_word_start = NULL;
  unsigned char *prev_word_end = NULL;
  unsigned char *next_word_start = NULL;
  unsigned char *next_word_end = NULL;

  unsigned char *caps_entity_start = NULL;
  unsigned char *caps_entity_end = NULL;
  unsigned char *stopwords_entity_start = NULL;
  unsigned char *stopwords_entity_end = NULL;
#ifdef KEYPHRASE_ENABLED
  unsigned char *stopwords_keyphrase_start = NULL;
  unsigned char *stopwords_keyphrase_end = NULL;
#endif
  unsigned char *sentence_start = NULL;

  // TODO (balaji) use bit map and masks to reduce comparisons
  bool current_word_caps = false;
  bool current_word_all_caps = false;
  bool current_word_has_mixed_case = false;
  bool current_word_starts_num = false;
  bool current_word_precedes_punct = false;
  bool current_word_precedes_ignore_word = false;
  bool prev_word_caps = false;
  bool prev_word_all_caps = false;
  bool prev_word_starts_num = false;
  bool prev_word_has_mixed_case = false;
  bool prev_word_precedes_punct = false;
  bool prev_word_precedes_ignore_word = false;
  bool next_word_caps = false;
  bool next_word_all_caps = false;
  bool next_word_starts_num = false;
  bool next_word_has_mixed_case = false;
  bool next_word_precedes_punct = false;
  bool next_word_precedes_ignore_word = false;

  bool current_word_stop = false;
  bool current_word_dict = false;
  bool prev_word_stop = false;
  bool prev_word_dict = false;
  bool next_word_stop = false;
  bool next_word_dict = false;
  bool is_ignore_word = false;
  bool is_punct = false;

  //bool second_letter_has_caps = false;

  // misc
  unsigned char *pch = NULL;
  unsigned char ch;
  unsigned int temp_len = 0;

  // unsafe
  strcpy(safe_status_buffer, "safe");
  bool text_has_unsafe_words = false;

  // script detection
  unsigned char *end = (unsigned char*) strchr((char *) buffer, '\0');
  std::string script = "uu";
  strcpy(script_buffer, "uu");
  unsigned int code_point = 0;
  string script_temp;
  //std::map<std::string, int> script_map;
  unsigned int script_count = 0;
  unsigned int english_count = 0;
  bool current_word_ascii = false;
  bool next_word_ascii = false;

/*
  // channel classification
  unsigned int channels_count = 0;
  unsigned int channels_len = 0;
  std::string channel;
*/

  // the whole thing starts here
  ptr = buffer;

#ifdef KE_DEBUG
  if (KE_DEBUG > 0)
    cout << endl << "original query: " << std::string((char *) buffer) << endl << endl;
#endif

  // go to the first word, ignoring handles and punctuations
  unsigned char *prev = NULL;
  while (ptr && '\0' != *ptr && (' ' == *ptr || (ispunct(*ptr) && IsPunct((char *) ptr, (char *) prev, (char *) ptr+1)) || IsIgnore((char *&) ptr))) {
    prev = ptr;
    ptr++;
  }

  if (!ptr || '\0' == *ptr) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0)
      cout << "either the input is empty or has ignore words only" << endl;
#endif
    return 0;
  }

  current_word_start = ptr;
  sentence_start = ptr;
#ifdef KE_DEBUG
  if (KE_DEBUG > 0)
    cout << "sentence start: " << sentence_start << endl;
#endif

  if (isupper(*ptr)) {
    current_word_caps = true;
    current_word_all_caps = true;
    current_word_starts_num = false;
    num_caps_words++;
  } else {
    if (isdigit(*ptr)) {
      current_word_starts_num = true; 
      num_numeric_words++;
    } else {
      current_word_starts_num = false;
    }
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
      cout << endl << "original query: " << std::string((char *) buffer) << endl << endl;
    }
#endif
    return -1;
  }

  while (ptr && ' ' != *ptr && '\0' != *ptr && !(is_punct = IsPunct((char *) ptr, (char *) ptr-1, (char *) ptr+1))) {
    /*
    if (!strcmp((char *) ptr, "&#")) {
      ptr+=2;
      while (' ' != *ptr && '\0' != *ptr && (isdigit(*ptr) || ';' == *ptr))
        ptr++;
      if ('\0' == *ptr)
        break;
    }
    */
    if (!ptr || '\0' == *ptr) {
#ifdef KE_DEBUG
      if (KE_DEBUG > 1) {
        cout << "either the input is empty or has ignore words only" << endl;
      }
#endif
      return 0;
    }
    if (isupper(*ptr)) {
      if (!current_word_all_caps && !ispunct(*ptr)) {
          current_word_has_mixed_case = true;
      }
    } else {
      if (current_word_caps)
        current_word_has_mixed_case = false;
      current_word_all_caps = false;
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
        cout << endl << "original query: " << std::string((char *) buffer) << endl << endl;
      }
#endif
      return -1;
    }
  }

  if (!ptr || '\0' == *ptr) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0) {
      cout << "either the input has only one word or the other words are ignore words" << endl;
    }
#endif
    return 0;
  }
  current_word_end = ptr;
  current_word_delimiter = *ptr;
  current_word_len = current_word_end - current_word_start;
  *ptr = '\0';
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
#endif
    // stop words
    if (m_stopwords_dictionary.Find(current_word_start) == 1) {
      current_word_stop = true;
      num_stop_words++;
      words_set.insert(std::string((char *) current_word_start));
#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        cout << "current word: " << current_word_start << " :stopword" << endl;
      }
#endif
    } else {
      current_word_stop = false;
    }

    // dictionary words
    if (m_dictionary.Find(current_word_start) == 1) {
      current_word_dict = true;
      num_dict_words++;
      words_set.insert(std::string((char *) current_word_start));
#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        cout << "current word: " << current_word_start << " :dictionary word" << endl;
      }
#endif
    } else {
      current_word_dict = false;
    }
  
    // unsafe words
    if (m_unsafe_dictionary.Find(current_word_start) == 1) {
      text_has_unsafe_words = true;
    }
  
/*
    if (m_channels_dictionary_map.FindPart(current_word_start, channel) == 1) {
      Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
    }
*/
#ifndef I18N_ENABLED
  }
#endif

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
      cout << endl << "original query: " << std::string((char *) buffer) << endl << endl;
    }
#endif
    return -1;
  }

  is_ignore_word = false;
  is_punct = false;
  while ('\0' != *ptr &&
         (' ' == *ptr || (ispunct(*ptr) && (is_punct = IsPunct((char *) ptr, (char *) ptr-1, (char *) ptr+1))) || (is_ignore_word = IsIgnore((char *&) ptr)))) {
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
#endif
    return 0;
  } else {
    next_word_start = ptr;
    num_words++;
    if (current_word_precedes_ignore_word || current_word_precedes_punct) {
      sentence_start = next_word_start;
#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        cout << "sentence start: " << sentence_start << endl;
      }
#endif
    }

    if (isupper(*next_word_start)) {
      next_word_caps = true;
      num_caps_words++;
      next_word_all_caps = true;
      next_word_starts_num = false;
    } else {
      next_word_caps = false;
      next_word_all_caps = false;
      if (isdigit(*next_word_start)) {
        next_word_starts_num = true;
        num_numeric_words++;
      } else {
        next_word_starts_num = false;
      }
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
#endif
      return 0;
    }
  } catch (...) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 0) {
      std::cout << "EXCEPTION 4: utf8 returned exception" << std::endl;
      cout << endl << "original query: " << std::string((char *) buffer) << endl << endl;
    }
#endif
    return -1;
  }

  while (ptr && probe && *ptr != '\n' && *ptr != '\0') {
    // this loop works between second letter to end punctuation for each word
    is_punct = false;
    if (' ' == *probe || '\0' == *probe || (ispunct(*probe) && (is_punct = IsPunct((char *) probe, (char *) probe-1, (char *) probe+1)))) {

#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        if (NULL != stopwords_entity_end)
          cout << "ERROR: stopswords entity end is not null. did you not write it before?" << endl;
        if (NULL != caps_entity_end)
          cout << "ERROR: caps entity end is not null. did you not write it before?" << endl;
      }
#endif

      // word boundary
#ifdef KE_DEBUG
      score = 0;
#endif

      if (next_word_start) {
        if (is_punct)
          next_word_precedes_punct = true;
        next_word_delimiter = *probe;
        next_word_end = probe;
        *probe = '\0';
        next_word_len = next_word_end - next_word_start;
      }

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
#endif

#ifdef KE_DEBUG
      if ((current_word_len < 2) && !isdigit(*current_word_start))
        score-=5;

      if ('#' == *current_word_start) {
        score++;
      }
#endif

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
#endif

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
#endif
        // stop words
        if (m_stopwords_dictionary.Find(next_word_start) == 1) {
          next_word_stop = true;
          num_stop_words++;
          words_set.insert(std::string((char *) next_word_start));
#ifdef KE_DEBUG
          score--;
          if (KE_DEBUG > 5) {
            cout << "next word: " << next_word_start << " :stopword" << endl;
          }
#endif
        } else {
          next_word_stop = false;
        }

        // dictionary words
        if (m_dictionary.Find(next_word_start) == 1) {
          next_word_dict = true;
          num_dict_words++;
          words_set.insert(std::string((char *) next_word_start));
#ifdef KE_DEBUG
          score--;
          if (KE_DEBUG > 5) {
            cout << "next word: " << next_word_start << " :dictionary word" << endl;
          }
#endif
        } else {
          next_word_dict = false;
        }

        // dictionary words
        if (m_unsafe_dictionary.Find(next_word_start) == 1) {
          text_has_unsafe_words = true;
        }

/*
        // channels
        if (m_channels_dictionary_map.FindPart(next_word_start, channel) == 1) {
          Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
        }
*/
#ifndef I18N_ENABLED
        }
#endif
      }

      if (prev_word_end)
        *prev_word_end = prev_word_delimiter;

#ifndef I18N_ENABLED
      if (current_word_ascii) {
#endif
      if (!current_word_stop && !current_word_dict && !current_word_caps &&
          !current_word_starts_num && !current_word_has_mixed_case &&
          (current_word_len > 1) && '#' != *current_word_start) {
#ifdef KE_DEBUG
        if (KE_DEBUG > 5) {
          cout << current_word_start << ": normal word" << endl;
        }
#endif
        num_normal_words++;
        words_set.insert(std::string((char *) current_word_start));
      }
      if (current_word_has_mixed_case)
        num_mixed_words++;

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
                       ((strcmp((char *) prev_word_start, "place") == 0 && strcmp((char *) current_word_start, "called") == 0 &&
                         !next_word_stop && (',' == next_word_delimiter || '.' == next_word_delimiter || '\0' == next_word_delimiter)) ||
                        (strcmp((char *) prev_word_start, "town") == 0 &&
                         (strcmp((char *) current_word_start, "of") == 0 || strcmp((char *) current_word_start, "called") == 0) &&
                         !next_word_stop && (',' == next_word_delimiter || '.' == next_word_delimiter || '\0' == next_word_delimiter)))
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
#endif

/*
        // each candidate should be checked against channel dictionary
        if (m_channels_dictionary_map.FindPart(stopwords_entity_start, channel) == 1) {
          Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
        }
*/

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

#ifdef KEYPHRASE_ENABLED
      if (NULL == stopwords_keyphrase_start) {
        if ('\0' != current_word_delimiter &&
#ifndef I18N_ENABLED
            current_word_ascii &&
#endif
            !current_word_stop &&
            !current_word_dict &&
            '#' != *current_word_start &&
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
        if (current_word_stop || current_word_dict || '#' == *current_word_start || ((current_word_len < 2) && !isdigit(*current_word_start))) {
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
#endif

      if (NULL == caps_entity_start) {
        caps_entity_end = NULL;
        if ((current_word_len > 1 && (current_word_caps || ('#' == *current_word_start && isupper(*(current_word_start+1)))) &&
            !current_word_stop && !current_word_dict) && 
            (!prev_word_start || prev_word_precedes_ignore_word || prev_word_precedes_punct || !prev_word_caps || '#' == *current_word_start)) {

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

              // this is a single word entity. so not inserting into keywords
              // caps_entity_start = current_word_start;
              // caps_entity_end = current_word_end;
              
              // instead insert into hashtags set
              if ((hashtags_len + current_word_len + 1) < hashtags_buffer_len) {
                Insert(hashtags_buffer, hashtags_len, current_word_start, current_word_len, hashtags_count);
              }
          }
        }
      } else {
#ifdef KE_DEBUG
        if (KE_DEBUG > 5) {
          cout << "caps entity candidate: " << caps_entity_start << endl;
        }
#endif
/*
        // each candidate should be checked against channel dictionary
        if (m_channels_dictionary_map.FindPart(caps_entity_start, channel) == 1) {
          Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
        }
*/

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

#ifdef KEYPHRASE_ENABLED
      // write keyphrases
      if (NULL != stopwords_keyphrase_start && NULL != stopwords_keyphrase_end) {
        if (stopwords_keyphrase_start != caps_entity_start || stopwords_keyphrase_end != caps_entity_end) {
#ifdef KE_DEBUG
          if (KE_DEBUG > 1)
            cout << endl << string((char *) stopwords_keyphrase_start, (stopwords_keyphrase_end - stopwords_keyphrase_start)) << " :keyphrase";
#endif
          if (strncmp((char *) stopwords_keyphrase_end-2, "\'s", 2) == 0) {
            ch = *(stopwords_keyphrase_end-2);
            *(stopwords_keyphrase_end-2) = '\0';
            temp_len = ((stopwords_keyphrase_end-2) - stopwords_keyphrase_start);
            if ((keyphrases_len + temp_len + 1) < keyphrases_buffer_len) {
              strncpy((char *) keyphrases_buffer + keyphrases_len, (char *) stopwords_keyphrase_start, temp_len);
              keyphrases_len += temp_len;
              strcpy((char *) keyphrases_buffer + keyphrases_len, "|");
              keyphrases_len += 1;
              keyphrases_count++;
            }
            *(stopwords_keyphrase_end-2) = ch;
          }
          else if ((pch = (unsigned char*) strstr((char *) stopwords_keyphrase_start, "\'s")) && (pch < stopwords_keyphrase_end)) {
            ch = *pch;
            *pch = '\0';
            // but don't insert the X in X's if X is a single word!
            if (strstr((char *) stopwords_keyphrase_start, " ")) {
              temp_len = pch - stopwords_keyphrase_start;
              if ((keyphrases_len + temp_len + 1) < keyphrases_buffer_len) {
                strncpy((char *) keyphrases_buffer + keyphrases_len, (char *) stopwords_keyphrase_start, temp_len);
                keyphrases_len += temp_len;
                strcpy((char *) keyphrases_buffer + keyphrases_len, "|");
                keyphrases_len += 1;
                keyphrases_count++;
              }
            }
            *pch = ch;
            temp_len = stopwords_keyphrase_end - stopwords_keyphrase_start;
            if ((keyphrases_len + temp_len + 1) < keyphrases_buffer_len) {
              strncpy((char *) keyphrases_buffer + keyphrases_len, (char *) stopwords_keyphrase_start, temp_len);
              keyphrases_len += temp_len;
              strcpy((char *) keyphrases_buffer + keyphrases_len, "|");
              keyphrases_len += 1;
              keyphrases_count++;
            }
          }
          else { 
            temp_len = stopwords_keyphrase_end - stopwords_keyphrase_start;
            if ((keyphrases_len + temp_len + 1) < keyphrases_buffer_len) {
              strncpy((char *) keyphrases_buffer + keyphrases_len, (char *) stopwords_keyphrase_start, temp_len);
              keyphrases_len += temp_len;
              strcpy((char *) keyphrases_buffer + keyphrases_len, "|");
              keyphrases_len += 1;
              keyphrases_count++;
            }
          }
        } else {
          if (stopwords_keyphrase_start > stopwords_keyphrase_end)
            cout << "ERROR: keyphrase markers are wrong\n";
        }
        stopwords_keyphrase_start = NULL;
        stopwords_keyphrase_end = NULL;
      }
#endif

      // write entities
      if (NULL != stopwords_entity_start && NULL != stopwords_entity_end) {
        if (stopwords_entity_start < stopwords_entity_end) {
          if ('#' == *stopwords_entity_start)
            stopwords_entity_start++;
#ifdef KE_DEBUG
          if (KE_DEBUG > 1)
            cout << endl << string((char *) stopwords_entity_start, (stopwords_entity_end - stopwords_entity_start)) << " :entity by stopword";
#endif
          if (strncmp((char *) stopwords_entity_end-2, "\'s", 2) == 0) {
            ch = *(stopwords_entity_end-2);
            *(stopwords_entity_end-2) = '\0';
            temp_len = (stopwords_entity_end-2) - stopwords_entity_start;
            if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
              strncpy((char *) keywords_buffer + keywords_len, (char *) stopwords_entity_start, temp_len);
              keywords_len += temp_len;
              strcpy((char *) keywords_buffer + keywords_len, "|");
              keywords_len += 1;
              keywords_count++;
            }

/*
            if (m_channels_dictionary_map.FindPart(stopwords_entity_start, channel) == 1) {
              Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
            }
*/

            *(stopwords_entity_end-2) = ch;
          }
          else if ((pch = (unsigned char*) strstr((char *) stopwords_entity_start, "\'s")) && (pch < stopwords_entity_end)) {
            ch = *pch;
            *pch = '\0';
            // but don't insert the X in X's if X is a single word!
            temp_len = pch - stopwords_entity_start;
            if (strstr((char *) stopwords_entity_start, " ")) {
              if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
                strncpy((char *) keywords_buffer + keywords_len, (char *) stopwords_entity_start, temp_len);
                keywords_len += temp_len;
                strcpy((char *) keywords_buffer + keywords_len, "|");
                keywords_len += 1;
                keywords_count++;
              }
            } else {
              if ((hashtags_len + temp_len + 1) < hashtags_buffer_len) {
                Insert(hashtags_buffer, hashtags_len, stopwords_entity_start, temp_len, hashtags_count);
              }
            }

/*
            if (m_channels_dictionary_map.FindPart(stopwords_entity_start, channel) == 1) {
              Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
            }
*/

            *pch = ch;
            temp_len = stopwords_entity_end - stopwords_entity_start;
            if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
              strncpy((char *) keywords_buffer + keywords_len, (char *) stopwords_entity_start, temp_len);
              keywords_len += temp_len;
              strcpy((char *) keywords_buffer + keywords_len, "|");
              keywords_len += 1;
              keywords_count++;
            }
          } else {
            temp_len = stopwords_entity_end - stopwords_entity_start;
            if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
              strncpy((char *) keywords_buffer + keywords_len, (char *) stopwords_entity_start, temp_len);
              keywords_len += temp_len;
              strcpy((char *) keywords_buffer + keywords_len, "|");
              keywords_len += 1;
              keywords_count++;
            }

/*
            if (m_channels_dictionary_map.FindPart(stopwords_entity_start, channel) == 1) {
              Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
            }
*/
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
          if ('#' == *caps_entity_start)
            caps_entity_start++;
#ifdef KE_DEBUG
          if (KE_DEBUG > 2) {
            cout << endl << string((char *) caps_entity_start, (caps_entity_end - caps_entity_start)) << " :entity by caps";
          }
#endif
          if (strncmp((char *) caps_entity_end-2, "\'s", 2) == 0) {
            ch = *(caps_entity_end-2);
            *(caps_entity_end-2) = '\0';
            temp_len = (caps_entity_end-2) - caps_entity_start;
            if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
              strncpy((char *) keywords_buffer + keywords_len, (char *) caps_entity_start, temp_len);
              keywords_len += temp_len;
              strcpy((char *) keywords_buffer + keywords_len, "|");
              keywords_len += 1;
              keywords_count++;
            }

/*
            if (m_channels_dictionary_map.FindPart(caps_entity_start, channel) == 1) {
              Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
            }
*/

            *(caps_entity_end-2) = ch;
          }
          else if ((pch = (unsigned char*) strstr((char *) caps_entity_start, "\'s")) && (pch < caps_entity_end)) {
            ch = *pch;
            *pch = '\0';
            // don't insert the X in X's if X is a single word!
            temp_len = pch - caps_entity_start;
            if (strstr((char *) caps_entity_start, " ")) {
              if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
                strncpy((char *) keywords_buffer + keywords_len, (char *) caps_entity_start, temp_len);
                keywords_len += temp_len;
                strcpy((char *) keywords_buffer + keywords_len, "|");
                keywords_len += 1;
                keywords_count++;
              }
            } else {
              // but such single word X's can be hashtags
              strncpy((char *) hashtags_buffer + hashtags_len, (char *) caps_entity_start, temp_len);
              hashtags_len += temp_len;
              strcpy((char *) hashtags_buffer + hashtags_len, "|");
              hashtags_len += 1;
              hashtags_count++;
            }

/*
            if (m_channels_dictionary_map.FindPart(caps_entity_start, channel) == 1) {
              Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
            }
*/

            *pch = ch;
            temp_len = caps_entity_end - caps_entity_start;
            if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
              strncpy((char *) keywords_buffer + keywords_len, (char *) caps_entity_start, temp_len);
              keywords_len += temp_len;
              strcpy((char *) keywords_buffer + keywords_len, "|");
              keywords_len += 1;
              keywords_count++;
            }
          }
          else { 
            temp_len = caps_entity_end - caps_entity_start;
            if ((keywords_len + temp_len + 1) < keywords_buffer_len) {
              strncpy((char *) keywords_buffer + keywords_len, (char *) caps_entity_start, temp_len);
              keywords_len += temp_len;
              strcpy((char *) keywords_buffer + keywords_len, "|");
              keywords_len += 1;
              keywords_count++;
            }

/*
            if (m_channels_dictionary_map.FindPart(caps_entity_start, channel) == 1) {
              Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
            }
*/

          }
        } else {
          cout << "ERROR: caps entity markers are wrong\n";
        }
        caps_entity_start = NULL;
        caps_entity_end = NULL;
      }

      // hash tags
      if ('#' == *current_word_start && current_word_len >= 2) {
        if ((hashtags_len + current_word_len/* + 1*/) < hashtags_buffer_len) {
          strncpy((char *) hashtags_buffer + hashtags_len, (char *) current_word_start + 1, current_word_len-1);
          hashtags_len += (current_word_len-1);
          strcpy((char *) hashtags_buffer + hashtags_len, "|");
          hashtags_len += 1;
          hashtags_count++;
        }
/*
        if (m_channels_dictionary_map.FindPart(current_word_start+1, channel) == 1) {
          Insert((unsigned char*) buffer3, channels_len, channel, channels_count);
        }
*/
      }

#ifndef I18N_ENABLED
      }
#endif

#ifdef KE_DEBUG
      if (KE_DEBUG > 5) {
        cout << endl;
      }
#endif

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
        while ('\0' != *ptr && (' ' == *ptr || (ispunct(*ptr) && (is_punct = IsPunct((char *) ptr, (char *) ptr-1, (char *) ptr+1))) || (is_ignore_word = IsIgnore((char *&) ptr)))) {
          current_word_precedes_ignore_word |= is_ignore_word;
          current_word_precedes_punct |= is_punct; 
          ptr++;
        }

        if (ptr && '\0' != *ptr) {
          next_word_start = ptr;
          num_words++;
          if (current_word_precedes_ignore_word) {
            sentence_start = next_word_start;
#ifdef KE_DEBUG
            if (KE_DEBUG > 5) {
              cout << "sentence start: " << sentence_start << endl;
            }
#endif
          }

          if (current_word_precedes_punct) {
            sentence_start = next_word_start;
#ifdef KE_DEBUG
            if (KE_DEBUG > 5) {
              cout << "sentence start: " << sentence_start << endl;
            }
#endif
            if (':' == current_word_delimiter || '>' == current_word_delimiter || '-' == current_word_delimiter || '(' == current_word_delimiter) {
              if (num_normal_words == 0) {
                *keywords_buffer = '\0';
                keywords_len = 0;
                caps_entity_start = NULL;
                caps_entity_end = NULL;
                stopwords_entity_start = NULL;
                stopwords_entity_end = NULL;
                stopwords_keyphrase_start = NULL;
                stopwords_keyphrase_end = NULL;
              }
            } else {
              for (pch = current_word_end + 1; (pch != next_word_start); pch++) {
                if (':' == *pch || '>' == *pch || '-' == *pch || '(' == *pch) {
                  if (num_normal_words == 0) {
                    *keywords_buffer = '\0';
                    keywords_len = 0;
                    caps_entity_start = NULL;
                    caps_entity_end = NULL;
                    stopwords_entity_start = NULL;
                    stopwords_entity_end = NULL;
                    stopwords_keyphrase_start = NULL;
                    stopwords_keyphrase_end = NULL;
                  }
                }
              }
            }
          }

          // after finding the start of next word, probe shud be at the same place as ptr
          probe = ptr;

          if (isupper(*next_word_start)) {
            next_word_caps = true;
            num_caps_words++;
            next_word_all_caps = true;
            next_word_starts_num = false;
          } else {
            next_word_caps = false;
            next_word_all_caps = false;
            if (isdigit(*next_word_start)) {
              next_word_starts_num = true;
              num_numeric_words++;
            } else {
              next_word_starts_num = false;
            }
          }
        } else {
          // placing the probe before '/0' so that loop will make it probe++
          // loop will terminate in the next cycle
          probe = ptr-1;
        }
      } // check for current word delimiter 

    } else {
      /*
      if (!strcmp((char *) probe, "&#")) {
        probe+=2;
        while (' ' != *probe && '\0' != *probe && (isdigit(*probe) || ';' == *probe))
          probe++;
        if ('\0' == *probe)
          break;
        current_word_precedes_ignore_word = true;
      }
      */
      // TODO (balaji) - mixed case logic seems twisted
      if (isupper(*probe)) {
        if (!next_word_all_caps && !ispunct(*probe)) {
          //if ((probe-1) == ptr)
            //second_letter_has_caps = true;
          //else
          next_word_has_mixed_case = true;
        }
      } else {
        if (next_word_caps)
          next_word_has_mixed_case = false;
        next_word_all_caps = false;
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
            cout << endl << "original query: " << std::string((char *) buffer) << endl << endl;
          }
#endif
          return -1;
        }
      //}
    }
  }

  if (num_mixed_words > 2) {
#ifdef KE_DEBUG
    if (KE_DEBUG > 1) {
      cout << "non-english tweet. ignoring keywords and keyphrases" << endl;
    }
#endif
    *keywords_buffer = '\0';
    keywords_len = 0;
#ifdef KEYPHRASE_ENABLED
    *keyphrases_buffer = '\0';
    keyphrases_len = 0;
#endif
  }

#ifdef KE_DEBUG
  if (KE_DEBUG > 5) {
    cout << endl << "\norginal query: " << std::string((char *) buffer) << endl;
    cout << "num words: " << num_words << endl;
    cout << "num caps words: " << num_caps_words << endl;
    cout << "num stop words: " << num_stop_words << endl;
    cout << "num dict words: " << num_dict_words << endl;
    cout << "num numeric words: " << num_numeric_words << endl;
    cout << "num normal words: " << num_normal_words << endl;
  }
#endif

  if ((num_normal_words == 0) && (num_dict_words != 0 || num_words > 5)) {
    *keywords_buffer = '\0';
    keywords_len = 0;
  }

  // safe status
  if (text_has_unsafe_words)
    strcpy(safe_status_buffer, "unsafe");
  else
    strcpy(safe_status_buffer, "safe");

  bool detect_lang = false;
  if (script_count == 0 && english_count > 10) {
    script = "en";
    detect_lang = true;
  } else if (script_count > 0 && (script_count < 11 || script_count < english_count)) {
    script = "uu";
    detect_lang = true;
  }

  strcpy(script_buffer, script.c_str());

#ifdef LANG_DETECT
  // language detection
  if (detect_lang) {
    std::string lang;
    if (m_language_detector->DetectLanguage(words_set, lang, true) < 0) {
      std::cout << "ERROR: language detection failed\n";
    } else {
      strcpy(buffer1, lang.c_str());
    }
    if (m_language_detector->Classify(std::string((char *) buffer), buffer_len, lang) < 0) {
      std::cout << "ERROR: language detection failed\n";
    } else {
      strcpy(buffer2, lang.c_str());
    }
/*
    if (lang.compare(0,2,"en") != 0) {
#ifdef KEYPHRASE_ENABLED
#ifdef KE_DEBUG
      std::cout << "Non-english: no keyphrases selected\n";
#endif
      *keyphrases_buffer = '\0';
      keyphrases_len = 0;
      keyphrases_count = 0;
#endif
    }
*/
  }
#endif

#ifdef KE_DEBUG
  if (KE_DEBUG > 1) {
    std::cout << "\nbuffer1: " << buffer1 << std::endl;
    std::cout << "buffer2: " << buffer2 << std::endl;
    std::cout << "buffer3: " << buffer3 << std::endl;
    std::cout << "buffer4: " << buffer4 << std::endl;
  }
#endif
#ifdef KE_DEBUG
  if (KE_DEBUG > 2) {
    cout << "returning from keywords extract. keywords: " << keywords_count \
         << " keyphrases: " << keyphrases_count << std::endl;
  }
#endif

  return keywords_count + keyphrases_count + hashtags_count;
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

} // namespace inagist_trends

