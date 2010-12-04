#include "ngrams_generator.h"
#include <iostream>
#include <fstream>
#include <cstring>
#include "script_detector.h"
#include "string_utils.h"
#include "utf8.h"

namespace inagist_classifiers {

NgramsGenerator::NgramsGenerator() {
}

NgramsGenerator::~NgramsGenerator() {
}

// return value,
// -1 means error
// 0 means end of string, handle ngram as required
// 1 means end of string, but bigram and ngram (word) are present
// 2 means both bigram and trigram are present 
int NgramsGenerator::PositionPointer(char*& prev, char*& current, char*& next) {

  if (!next)
    return -1;

  if (prev == next) {
    if (*prev == '\0')
      return 0;
    current = prev + 1;
    if (*current != '\0')
      next = current + 1;
    else
      return 0;
  } else {
    char* ptr = next + 1;
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

  // control should not come here
  return -1;
}

// returns the number of bigrams + trigrams + ngrams (words)
int NgramsGenerator::GetNgrams(const char* text,
                               const unsigned int& text_len,
                               std::map<std::string, int>& features_map) {

  if (!text) {
    std::cout << "ERROR: Empty string\n";
    return -1;
  }

  //char* end_text = (char*) text + text_len;

  char* prev = NULL;
  char* current = NULL;
  char* next = prev = current = (char *) text;

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
        trigram = std::string(prev, 3);
        if ((map_iter = features_map.find(trigram)) != features_map.end())
          (*map_iter).second += 1;
        else
          features_map.insert(std::pair<std::string, int>(trigram, 1));
        count++;
        // fall through
      case 2:
        bigram = std::string(prev, 2);
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
                               std::map<std::string, int>& features_map) {

  if (tweet.length() < 1)
    return -1;

  strcpy(m_buffer, tweet.c_str());

  char *ptr = NULL;
  char *probe = NULL;
  char current_word_delimiter;

  unsigned int current_word_len = 0;

  char *current_word_start = NULL;
  char *current_word_end = NULL;
  char *next_word_start = NULL;
  bool is_ignore_word = false;
  bool is_punct = false;
  bool current_word_starts_num = false;
  int num_words = 0;

  inagist_utils::StringUtils utils;
  // script detection
  char *end = strchr(m_buffer, '\0');
  std::string script = "uu";
  int code_point = 0;
  std::string script_temp;
  int script_count = 0;
  int english_count = 0;

  // the whole thing starts here
  ptr = m_buffer;

#ifdef DEBUG
  std::cout << std::endl << "original query: " << m_buffer << std::endl;
#endif

  // go to the first word, ignoring handles and punctuations
  char *prev_temp = NULL;
  while (ptr && '\0' != *ptr && (' ' == *ptr || '#' == *ptr || (ispunct(*ptr) && utils.IsPunct(ptr, prev_temp, ptr+1)) || utils.IsIgnore(ptr))) {
    prev_temp = ptr;
    ptr++;
  }

  if (!ptr || '\0' == *ptr) {
#ifdef DEBUG
    std::cout << "either the input is empty or has ignore words only" << std::endl;
    return 0;
#endif
  }

  current_word_start = ptr;
  num_words++;
  bool word_has_all_latin = true;

  if (isdigit(*ptr)) {
    current_word_starts_num = true; 
  } else {
    current_word_starts_num = false;
  }

  // initialize script detecter. this clears its internal hash
  inagist_classifiers::ScriptDetector sd;
  sd.Init();
  probe = ptr + 1;

  while (ptr && probe && *ptr != '\n' && *ptr != '\0') {
    // this loop works between second letter to end punctuation for each word
    is_punct = false;
    if (' ' == *probe || '\0' == *probe || '\'' == *probe || (ispunct(*probe) && (is_punct = utils.IsPunct(probe, probe-1, probe+1)))) {

      current_word_delimiter = *probe;
      current_word_end = probe;
      *probe = '\0';
      current_word_len = current_word_end - current_word_start;
#ifdef DEBUG
      std::cout << "current word: " << current_word_start << std::endl;
#endif

      // find ngrams
      if (word_has_all_latin) {
        if (GetNgrams((const char*) current_word_start, current_word_len, features_map) < 0) {
          std::cerr << "ERROR: could not get ngrams for word " << current_word_start << std::endl;
          return -1;
        }
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
          std::cerr << "ERROR: Fatal Exception trying to access unallocated memory space\n";
          return -1;
        }

        // find the next position of ptr
        // IsIgnore will literally ignore the word by changing the cursor to next word end
        is_ignore_word = false;
        is_punct = false;
        while ('\0' != *ptr && (' ' == *ptr || '#' == *ptr || (ispunct(*ptr) && (is_punct = utils.IsPunct(ptr, ptr-1, ptr+1))) || (is_ignore_word = utils.IsIgnore(ptr)))) {
          ptr++;
        }

        if (ptr && '\0' != *ptr) {
          next_word_start = ptr;
          num_words++;
          word_has_all_latin = true;
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
      if (!strcmp(probe, "&#")) {
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
          if (sd.DetectScript(code_point, script_temp) > 0) {
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
          if (code_point > 0x40 && code_point < 0x7B)
            english_count++;
        }
      } catch (...) {
#ifdef DEBUG
        std::cout << "Exception: " << code_point << " " << probe << std::endl;
#endif
        probe++;
      }
    //}
  }

#ifdef DEBUG
  std::cout << "num words: " << num_words << std::endl;
#endif

  // deinitialize script detector
  sd.Clear();

  return features_map.size();
}

int NgramsGenerator::GetNgramsFromFile(const std::string& input_file_name,
                                       std::map<std::string, int>& features_map) {

  std::ifstream ifs(input_file_name.c_str());
  if (!ifs) {
    std::cout << "ERROR: could not open file " << input_file_name << std::endl;
    return -1;
  }

  std::string line;
  int num_lines = 0;
  while (getline(ifs, line)) {
    num_lines++;
    if (GetNgrams(line.c_str(), line.length(), features_map) <= 0) {
    }
  }
  ifs.close();

  return features_map.size();
}

} // namespace inagist classifiers
