#include "ngrams_generator.h"
#include <iostream>
#include <fstream>
#include <cstring>
#include "script_detector_utils.h"
#include "string_utils.h"
#include "utf8.h"

#ifdef DEBUG
#if DEBUG>0
#define NG_DEBUG DEBUG
#endif
#endif
//#define NG_DEBUG 1

#define NGRAM_LENGTH 5
// this is used in a conditional, hence the macro
#define NGRAM_DIFF NGRAM_LENGTH-1

#define MAX_WORD_LEN 256

namespace inagist_classifiers {

NgramsGenerator::NgramsGenerator() {
#ifdef NG_DEBUG
  m_debug_level = NG_DEBUG;
  std::cout << "NG_DEBUG (default): " << m_debug_level << std::endl;
#else
  m_debug_level = 0;
#endif
}

NgramsGenerator::~NgramsGenerator() {
}

int NgramsGenerator::SetDebugLevel(unsigned int debug_level) {
  m_debug_level = debug_level;
  return 0;
}

// return value,
// -1 means error
// 0 means end of string, handle ngram as required
// 1 means end of string, but bigram and ngram (word) are present
// 2 means both bigram and trigram are present 
int NgramsGenerator::PositionPointer(unsigned char*& prev, unsigned char*& current, unsigned char*& next) {

  if (!next) {
#ifdef NG_DEBUG
    std::cout << "ERROR: next ptr cannot be null\n";
#endif
    return -1;
  }

  if (prev == next) {
    if (*prev == '\0')
      return 0;
    current = prev + 1;
    if (*current != '\0')
      next = current + 1;
    else
      return 0;
  } else {
    unsigned char* ptr = next + 1;
    if (ispunct(*next)) {
      if (*ptr != '\0')
        prev = ptr;
      else
        return 0;
      current = prev + 1;
      if (*current != '\0')
        next = current + 1;
      else
        return 0;
    } else {
      prev = current;
      current = next;
      next = ptr;
    }
  }

  if (*current == '\0') {
    return 0;
  } else if (*prev == ' ' || ispunct(*prev) || *current == ' ' || ispunct(*current)) {
    return 1;
  } else if (*next == ' ' || ispunct(*next) || *next == '\0') {
    return 2;
  } else {
    return 3;
  }

#ifdef NG_DEBUG
  std::cout << "ERROR: control should not come here\n";
#endif
  return -1;
}

// returns the number of bigrams + trigrams + ngrams (words)
int NgramsGenerator::GetNgrams(const unsigned char* text,
                               const unsigned int& text_len,
                               std::map<std::string, int>& features_map) {

  if (!text) {
#ifdef NG_DEBUG
    std::cout << "ERROR: invalid string\n";
#endif
    return -1;
  }

  //char* end_text = (char*) text + text_len;

  unsigned char* prev = NULL;
  unsigned char* current = NULL;
  unsigned char* next = prev = current = (unsigned char *) text;

  int count = 0;
  int ret_value = 0;
  std::string bigram;
  std::string trigram;
  std::map<std::string, int>::iterator map_iter;
  while (next && *next != '\0') {
    if ((ret_value = PositionPointer(prev, current, next)) < 0) {
      std::cout << "Error: invalid pointer position\n";
      return count;
    }

    switch (ret_value) {
      case 3:
        trigram = std::string((char *) prev, 3);
        if ((map_iter = features_map.find(trigram)) != features_map.end())
          (*map_iter).second += 1;
        else
          features_map.insert(std::pair<std::string, int>(trigram, 1));
        count++;
        // fall through
      case 2:
        bigram = std::string((char *) prev, 2);
        if ((map_iter = features_map.find(bigram)) != features_map.end())
          (*map_iter).second += 1;
        else
          features_map.insert(std::pair<std::string, int>(bigram, 1));
        count++;
        break;
      case 1:
        break;
      case 0:
        return count;
    }
  }

  return count;
}

// returns the number of bigrams + trigrams + ngrams (words)
// this handles the ignore words, utf codepoints present in tweets
int NgramsGenerator::GetNgramsFromTweet(const std::string& tweet,
                                        std::map<std::string, int>& features_map,
                                        bool ignore_case) {

  if (tweet.length() < 1) {
#ifdef NG_DEBUG
    std::cout << "ERROR: empty string. no ngrams." << std::endl;
#endif
    return -1;
  }

  if (ignore_case) {
    inagist_utils::ToLower(tweet.c_str(), (char *) m_buffer);
  } else {
    strcpy((char *) m_buffer, tweet.c_str());
  }

  unsigned char *ptr = NULL;
  unsigned char *probe = NULL;
  unsigned char current_word_delimiter;

  unsigned int current_word_len = 0;

  unsigned char *current_word_start = NULL;
  unsigned char *current_word_end = NULL;
  unsigned char *next_word_start = NULL;
  bool is_ignore_word = false;
  bool is_punct = false;
  bool current_word_starts_num = false;
  bool current_word_all_caps = true;
  int num_words = 0;
  std::string ngram;

  // script detection
  unsigned char *end = (unsigned char*) strchr((char *) m_buffer, '\0');
  std::string script = "uu";
  int code_point = 0;
  std::string script_temp;
  int script_count = 0;
  int english_count = 0;

  // the whole thing starts here
  ptr = m_buffer;

#ifdef NG_DEBUG
  if (m_debug_level > 1) {
    std::cout << std::endl << "original query: " << m_buffer << std::endl;
  }
#endif

  // go to the first word, ignoring handles and punctuations
  unsigned char *prev_temp = NULL;
  while (ptr && '\0' != *ptr && (' ' == *ptr || '#' == *ptr || isdigit(*ptr) || (ispunct(*ptr) && inagist_utils::IsPunct((char *) ptr, (char *) prev_temp, (char *) ptr+1)) || inagist_utils::IsIgnore((char *&) ptr))) {
    prev_temp = ptr;
    ptr++;
  }

  if (!ptr || '\0' == *ptr) {
#ifdef NG_DEBUG
    std::cout << "either the input is empty or has ignore words only" << std::endl;
#endif
    return 0;
  }

  current_word_start = ptr;
  num_words++;
  bool word_has_all_latin = true;
  bool word_starts_caps = true;

  if (isdigit(*ptr)) {
    current_word_starts_num = true; 
    word_has_all_latin = false;
    word_starts_caps = false;
  } else {
    current_word_starts_num = false;
    if (isupper(*ptr)) {
      word_starts_caps = true;
    } else {
      word_starts_caps = false;
    }
  }

  // now we need to achieve the following
  // probe = ptr + 1;
  probe = ptr;
  try {
    code_point = utf8::next(probe, end);
  } catch (...) {
#ifdef NG_DEBUG
    std::cout << "EXCEPTION: utf8 returned exception" << std::endl;
#endif
    return -1;
  }

  while (ptr && probe && *ptr != '\n' && *ptr != '\0') {
    // this loop works between second letter to end punctuation for each word
    is_punct = false;
    if (' ' == *probe || '\0' == *probe || '\'' == *probe || isdigit(*probe) || (ispunct(*probe) && (is_punct = inagist_utils::IsPunct((char *) probe, (char *) probe-1, (char *) probe+1)))) {

      current_word_delimiter = *probe;
      current_word_end = probe;
      *probe = '\0';
      current_word_len = current_word_end - current_word_start;
#ifdef NG_DEBUG
      if (m_debug_level > 2) {
        std::cout << "current word: " << current_word_start << " (";
        if (current_word_all_caps) std::cout << "all_caps, ";
        if (word_starts_caps) std::cout << "starts_caps, ";
        if (word_has_all_latin) std::cout << "all_latin, ";
        std::cout << ") " << std::endl;
      }
#endif

      // find ngrams
      if (word_has_all_latin && !current_word_all_caps && !word_starts_caps) {
        /*
        ngram.assign((char *) current_word_start, current_word_len);
        if (features_map.find(ngram) != features_map.end()) {
          features_map[ngram] += 1;
        } else {
          features_map[ngram] = 1;
        }
        */
        if (GetNgramsFromWord((const unsigned char*) current_word_start, current_word_len, features_map) < 0) {
#ifdef NG_DEBUG
          std::cout << "ERROR: could not get ngrams for word " << current_word_start << std::endl;
#endif
          return -1;
        }
#ifdef NG_DEBUG
      } else {
        if (m_debug_level > 2) {
          if (!word_has_all_latin) {
            std::cout << current_word_start << " is not all latin. ignored.\n";
          }
          if (current_word_all_caps) {
            std::cout << current_word_start << " has all caps. ignored.\n";
          } else if (word_starts_caps) {
            std::cout << current_word_start << " starts with caps. ignored.\n";
          }
        }
#endif
      }

      // exit conditions
      if ('\0' == current_word_delimiter) {
        *current_word_end = current_word_delimiter;
        break;
      }

      // if current word is not the known last word, briefly probe to see if next word exists
      if ('\0' != current_word_delimiter) {
        ptr = probe + 1;
        if (!ptr) {
#ifdef NG_DEBUG
          std::cout << "ERROR: Fatal Exception trying to access unallocated memory space\n";
#endif
          return -1;
        }

        // find the next position of ptr
        // IsIgnore will literally ignore the word by changing the cursor to next word end
        is_ignore_word = false;
        is_punct = false;
        while ('\0' != *ptr && (' ' == *ptr || '#' == *ptr || isdigit(*ptr) || (ispunct(*ptr) && (is_punct = inagist_utils::IsPunct((char *) ptr, (char *) ptr-1, (char *) ptr+1))) || (is_ignore_word = inagist_utils::IsIgnore((char *&) ptr)))) {
          ptr++;
        }

        if (ptr && '\0' != *ptr) {
          next_word_start = ptr;
          num_words++;
          // initialising for the next word
          word_has_all_latin = true;
          if (isupper(*ptr)) {
            word_starts_caps = true;
            current_word_all_caps = true;
          } else {
            word_starts_caps = false;
            current_word_all_caps = false;
          }
          // after finding the start of next word, probe shud be at the same place as ptr
          probe = ptr;
        } else {
          // placing the probe before '/0' so that loop will make it probe++
          // loop will terminate in the next cycle
          probe = ptr-1;
        }
      } // check for current word delimiter 

      *current_word_end = current_word_delimiter;
      current_word_start = next_word_start;
    } else {
      if (!strcmp((char *) probe, "&#")) {
        while (' ' != *probe && '\0' != *probe)
          probe++;
        if ('\0' == *probe)
          break;
      }
    }

    // a mere cog in a loop wheel, but a giant killer if commented
    /*
    if (script_count > 9 || english_count > 20) {
      probe++;
    } else {
    */
      try {
        code_point = utf8::next(probe, end);
        if (code_point > 0x7F) {
          if (inagist_classifiers::DetectScript(code_point, script_temp) > 0) {
            if (script_temp != "en") {
              word_has_all_latin = false;
              if (script_temp != script) {
                script_count = 0;
                script = script_temp;
              } else {
                script_count++;
                if (script_count > 9) {
                  break;
                }
              }
            }
          } else {
            word_has_all_latin = false;
          }
        } else {
          if (code_point > 0x40 && code_point < 0x7B) {
            english_count++;
            if (code_point > 0x60) {
              current_word_all_caps = false;
            }
          }
          else
            word_has_all_latin = false;
        }
      } catch (...) {
#ifdef NG_DEBUG
        std::cout << "Exception: " << code_point << " " << probe << std::endl;
#endif
        features_map.clear();
        return -1;
      }
    //}
  }

#ifdef NG_DEBUG
  if (m_debug_level > 2) {
    std::cout << "num words: " << num_words << std::endl;
    std::cout << "features map size: " << features_map.size() << std::endl;
  }
#endif

  return features_map.size();
}

int NgramsGenerator::GetNgramsFromFile(const std::string& input_file_name,
                                       std::map<std::string, int>& features_map) {

  std::ifstream ifs(input_file_name.c_str());
  if (!ifs) {
#ifdef NG_DEBUG
    std::cout << "ERROR: could not open file " << input_file_name << std::endl;
#endif
    return -1;
  }

  std::string line;
  int num_lines = 0;
  while (getline(ifs, line)) {
    num_lines++;
    if (GetNgrams((unsigned char*) line.c_str(), line.length(), features_map) <= 0) {
    }
  }
  ifs.close();

  return features_map.size();
}

int NgramsGenerator::GetAllNgrams(const std::string& tweet,
                                  std::map<std::string, int>& features_map) {

  if (tweet.length() < 1) {
    std::cout << "ERROR: empty string. no ngrams.\n";
    return -1;
  }

  unsigned char* ptr = m_buffer;
  /*
  if (tweet.at(0) != ' ') {
    *m_buffer = ' ';
    ptr++;
  }
  */
  strcpy((char *) m_buffer, tweet.c_str());

  unsigned char* end = m_buffer + tweet.length() - 1;
  unsigned char* stop = ptr;
  unsigned char* pch = NULL;

  while (ptr && *ptr != '\0' && stop <= end) {
    if ((pch = (unsigned char*) strstr((char *) ptr, "http:")) != NULL ||
        (pch = (unsigned char*) strstr((char *) ptr, "@")) != NULL) {
      if (pch > ptr) {
        stop = pch - 1;
        if (GetAllNgrams(ptr, stop, features_map) < 0) {
          std::cout << "ERROR: could not find ngrams\n";
          return -1;
        }
        ptr = pch;
      }
      while (ptr && *ptr != ' ' && *ptr != '\0') {
        ptr++;
      }
    } else {
      break;
    }
  }

  if (ptr && ptr < end) {
    if (GetAllNgrams(ptr, end, features_map) < 0) {
      std::cout << "ERROR: could not find ngrams\n";
      return -1;
    }
  }

#ifdef NG_DEBUG
  if (m_debug_level) {
    std::cout << "Num ngrams: " << features_map.size() << std::endl;
  }
#endif

  return features_map.size();
}

int NgramsGenerator::GetAllNgrams(unsigned char* start,
                                  unsigned char* stop,
                                  std::map<std::string, int>& features_map) {

  unsigned char* ptr = start;
  unsigned char* pch = NULL;
  unsigned int diff = 0;

  std::string ngram;
  while (ptr && ptr < stop) {
    // replace usual punctuations with terminating null space
    pch = ptr+1;
    diff = pch - ptr;
    while (pch <= stop) {
      if (diff > 1 && diff <= NGRAM_DIFF) {
        ngram.assign((char *) ptr, (diff + 1));
#ifdef NG_DEBUG
        std::cout << ngram << std::endl;
#endif
        if (features_map.find(ngram) != features_map.end()) {
          features_map[ngram] += 1;
        } else {
          features_map[ngram] = 1;
        }
      }
      pch++;
      diff = pch - ptr;
    }
    ptr++;
  }

  return 0;
}

int NgramsGenerator::GetNgramsFromWords(std::set<std::string>& words_set,
                                  std::map<std::string, int>& features_map,
                                  bool ignore_case) {

  std::set<std::string>::iterator set_iter;
  if (ignore_case) {
    unsigned char word[MAX_WORD_LEN];
    word[MAX_WORD_LEN] = '\0';
    for (set_iter = words_set.begin(); set_iter != words_set.end(); set_iter++) {
      inagist_utils::ToLower(set_iter->c_str(), (char *) word);
      GetNgramsFromWord(word, set_iter->length(), features_map);
    }
  } else {
    for (set_iter = words_set.begin(); set_iter != words_set.end(); set_iter++) {
      GetNgramsFromWord((unsigned char*) set_iter->c_str(), set_iter->length(), features_map);
    }
  }

  return features_map.size();
}


int NgramsGenerator::GetNgramsFromWord(const unsigned char* word_str,
                                       unsigned int word_len,
                                       std::map<std::string, int>& features_map) {

  if (!word_str || word_len < 1) {
    std::cerr << "ERROR: invalid word input. could not get ngrams\n";
    return -1;
  }

  std::string ngram;
  int count = 0;

  ngram = " ";
  ngram += std::string((char *) word_str);
  ngram += " ";
  if (features_map.find(ngram) != features_map.end()) {
    features_map[ngram] += 1;
  } else {
    features_map[ngram] = 1;
  }
  count++;

  if (word_len <= NGRAM_LENGTH)
    return 1;

  unsigned char* ptr = NULL;
  unsigned char* pch = NULL;
  unsigned char* stop = NULL;
  unsigned int diff = 0;
  unsigned int len = 0;

  unsigned char buffer[1024];
  memset(buffer, '\0', 1024);

  len = word_len;
  ptr = buffer;
  *ptr = ' ';  
  ptr++;
  memcpy((char *) ptr, (char *) word_str, word_len);
  ptr += len;
  *ptr = ' ';
  len += 2;
  ptr = buffer;
  stop = ptr + len - 1;
  while (ptr && ptr < stop) {
    // replace usual punctuations with terminating null space
    pch = ptr+1;
    diff = pch - ptr;
    while (pch <= stop) {
      if (diff > 1 && diff <= NGRAM_DIFF) {
        ngram.assign((char *) ptr, (diff + 1));
#ifdef NG_DEBUG
        std::cout << ngram << std::endl;
#endif
        if (features_map.find(ngram) != features_map.end()) {
          features_map[ngram] += 1;
        } else {
          features_map[ngram] = 1;
        }
        count++;
      }
      pch++;
      diff = pch - ptr;
    }
    ptr++;
  }

  return count;
}

} // namespace inagist classifiers
