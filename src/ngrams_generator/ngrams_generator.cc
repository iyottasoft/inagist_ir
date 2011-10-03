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
#else
  m_debug_level = 0;
#endif // NG_DEBUG
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
#endif // NG_DEBUG
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
  std::cerr << "ERROR: control should not come here\n";
#endif // NG_DEBUG
  return -1;
}

// returns the number of bigrams + trigrams + ngrams (words)
int NgramsGenerator::GetNgrams(const unsigned char* text,
                               const unsigned int& text_len,
                               Corpus& corpus) {

  if (!text) {
#ifdef NG_DEBUG
    std::cerr << "ERROR: invalid string\n";
#endif // NG_DEBUG
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
  Corpus::iterator map_iter;
  while (next && *next != '\0') {
    if ((ret_value = PositionPointer(prev, current, next)) < 0) {
      std::cout << "Error: invalid pointer position\n";
      return count;
    }

    switch (ret_value) {
      case 3:
        trigram = std::string((char *) prev, 3);
        if ((map_iter = corpus.find(trigram)) != corpus.end())
          (*map_iter).second += 1;
        else
          corpus.insert(std::pair<std::string, int>(trigram, 1));
        count++;
        // fall through
      case 2:
        bigram = std::string((char *) prev, 2);
        if ((map_iter = corpus.find(bigram)) != corpus.end())
          (*map_iter).second += 1;
        else
          corpus.insert(std::pair<std::string, int>(bigram, 1));
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
int NgramsGenerator::GetNgrams(const unsigned char* text,
                               const unsigned int& text_len,
                               std::set<std::string>& ngrams_set) {

  if (!text) {
#ifdef NG_DEBUG
    std::cerr << "ERROR: invalid string\n";
#endif // NG_DEBUG
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
  while (next && *next != '\0') {
    if ((ret_value = PositionPointer(prev, current, next)) < 0) {
      std::cout << "Error: invalid pointer position\n";
      return count;
    }

    switch (ret_value) {
      case 3:
        trigram = std::string((char *) prev, 3);
        ngrams_set.insert(trigram);
        // fall through
      case 2:
        bigram = std::string((char *) prev, 2);
        ngrams_set.insert(bigram);
        break;
      case 1:
        break;
      case 0:
        return count;
    }
  }

  return count;
}

int NgramsGenerator::GetNgramsFromFile(const std::string& input_file_name,
                                       Corpus& corpus) {

  std::ifstream ifs(input_file_name.c_str());
  if (!ifs) {
#ifdef NG_DEBUG
    std::cerr << "ERROR: could not open file " << input_file_name << std::endl;
#endif // NG_DEBUG
    return -1;
  }

  std::string line;
  int num_lines = 0;
  while (getline(ifs, line)) {
    num_lines++;
    if (GetNgrams((unsigned char*) line.c_str(), line.length(), corpus) <= 0) {
    }
  }
  ifs.close();

  return corpus.size();
}

int NgramsGenerator::GetAllNgrams(const std::string& tweet,
                                  Corpus& corpus) {

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
        if (GetAllNgrams(ptr, stop, corpus) < 0) {
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
    if (GetAllNgrams(ptr, end, corpus) < 0) {
      std::cout << "ERROR: could not find ngrams\n";
      return -1;
    }
  }

#ifdef NG_DEBUG
  if (m_debug_level > 1) {
    std::cout << "Num ngrams: " << corpus.size() << std::endl;
  }
#endif // NG_DEBUG

  return corpus.size();
}

int NgramsGenerator::GetAllNgrams(unsigned char* start,
                                  unsigned char* stop,
                                  Corpus& corpus) {

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
        if (m_debug_level > 3) {
          std::cout << ngram << std::endl;
        }
#endif // NG_DEBUG
        if (corpus.find(ngram) != corpus.end()) {
          corpus[ngram] += 1;
        } else {
          corpus[ngram] = 1;
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
                                  Corpus& corpus,
                                  bool ignore_case) {

  std::set<std::string>::iterator set_iter;
  if (ignore_case) {
    unsigned char word[MAX_WORD_LEN];
    word[MAX_WORD_LEN] = '\0';
    for (set_iter = words_set.begin(); set_iter != words_set.end(); set_iter++) {
      inagist_utils::ToLower(set_iter->c_str(), (char *) word);
      GetNgramsFromWord(word, set_iter->length(), corpus);
    }
  } else {
    for (set_iter = words_set.begin(); set_iter != words_set.end(); set_iter++) {
      GetNgramsFromWord((unsigned char*) set_iter->c_str(), set_iter->length(), corpus);
    }
  }

  return corpus.size();
}

int NgramsGenerator::GetNgramsFromWords(const unsigned char* text_word_list,
                                        const unsigned int& list_len,
                                        const unsigned int& word_count, 
                                        Corpus& corpus,
                                        bool ignore_case) {

  if (!text_word_list || list_len <= 0 || word_count <= 0) {
    return -1;
  }

  int num_ngrams = 0;
  int ret_val = 0;
  unsigned char* start = (unsigned char*) text_word_list;
  unsigned char* end = (unsigned char*) strchr((char *) start, '|');
  unsigned int word_len = 0;
  while (start && end && *start != '\0') {
    word_len = end - start; 
    if ((ret_val = GetNgramsFromWord(start, word_len, corpus)) < 0) {
      std::cerr << "ERROR: could not get ngrams" << std::endl;
      return ret_val;
    } else {
      num_ngrams += ret_val;
    }
    start = end + 1;
    end = (unsigned char*) strchr((char *) start, '|');
  }
  start = NULL;
  end = NULL;

  return num_ngrams;
}

int NgramsGenerator::GetNgramsFromWord(const unsigned char* word_str,
                                       unsigned int word_len,
                                       Corpus& corpus) {

  if (!word_str || word_len < 1) {
    std::cerr << "ERROR: invalid word input. could not get ngrams\n";
    return -1;
  }

  std::string ngram;
  int count = 0;

  ngram = " ";
  ngram += std::string((char *) word_str, word_len);
  ngram += " ";

  if (word_len <= NGRAM_LENGTH) {

    // this inserts the whole word. earlier this was done by default. now only if word length less than min ngram_length
    if (corpus.find(ngram) != corpus.end()) {
      corpus[ngram] += 1;
    } else {
      corpus[ngram] = 1;
    }
    count++;

    return 1;
  }

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
        if (m_debug_level > 3) {
          std::cout << ngram << std::endl;
        }
#endif // NG_DEBUG
        if (corpus.find(ngram) != corpus.end()) {
          corpus[ngram] += 1;
        } else {
          corpus[ngram] = 1;
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
