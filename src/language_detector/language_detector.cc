#include "language_detector.h"
#include "keywords_extract.h"

#include <iostream>

namespace inagist_classifiers {

LanguageDetector::LanguageDetector() {
}

LanguageDetector::~LanguageDetector() {
}

int LanguageDetector::Init(const std::string& training_data_dir) {
  return 0;
}

// return value,
// -1 means error
// 0 means end of string, handle ngram as required
// 1 means end of string, but bigram present and ngram are present
// 2 means both bigram and trigram are present 
int LanguageDetector::PositionPointer(char*& prev, char*& current, char*& next) {

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
    if (*next == ' ') {
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
  } else if (*prev == ' ' || *current == ' ') {
    return 1;
  } else if ((*next == ' ') || (*next == '\0')) {
    return 2;
  } else {
    return 3;
  }

  // control should not come here
  return -1;
}

// returns the number of bigrams + trigrams + ngrams (words)
int LanguageDetector::GetNgrams(const char* text, const unsigned int& text_len, std::map<std::string, int>& features_map) {

  if (!text)
    return -1;

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

int LanguageDetector::DetectLanguage(const std::string& text, const unsigned int& text_len, std::string& lang) {
  std::map<std::string, int> features_map;
  int num_ngrams = 0;
  if ((num_ngrams = GetNgrams(text.c_str(), text_len, features_map)) < 0) {
    std::cout << "Error: could not find ngrams" << std::endl;
    return -1;
  }

  if (num_ngrams == 0) {
    std::cout << "no ngrams found\n";
    return 0;
  }

  std::map<std::string, int>::iterator map_iter;
  for (map_iter = features_map.begin(); map_iter != features_map.end(); map_iter++) {
    std::cout << (*map_iter).first << " " << (*map_iter).second << std::endl;
  }

  return num_ngrams;
}

int LanguageDetector::Clear() {
  return 0;
}

} // namespace inagist_classifiers

