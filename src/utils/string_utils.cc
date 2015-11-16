#include "string_utils.h"
#include <cstdlib>
#include <cstring>
#include <cctype>
#include <cstdio>
#include <iostream>
#include "utf8.h"
#include "script_detector_utils.h"

#ifdef DEBUG
#if DEBUG>0
#define UTILS_DEBUG DEBUG
#endif
#endif
//#define UTILS_DEBUG 0

#define MAX_BUFFER_LEN 1024

extern int DetectScript(int code_point, std::string &script);

namespace inagist_utils {
  // unless otherwise specified functions return 0 or NULL or false as default
  // return values less than 0 are likely error codes

//StringUtils::StringUtils() {
//}

//StringUtils::~StringUtils() {
//}

// this function isn't unicode safe
// TODO (balaji) for ascii, we can ofcourse use an array lookup to speed up
bool IsPunct(char*& ptr, char* prev, char* next, int* punct_intent, int* punct_senti) {

  if (!ptr || *ptr == ' ' || *ptr == '\0')
    return true;

  if (!ispunct(*ptr))
    return false;

  switch (*ptr) {
    case ',':
      if (prev && next)
        if (isdigit(*prev) && isdigit(*next))
          return false;
      break;
    case '.':
      if (prev && next)
        if (isdigit(*prev) && isdigit(*next))
          return false;
      if (next && *next != '\0') {
        if (*next == ' ')
          return true;
        if (!strcmp(ptr, ".com") || !strcmp(ptr, ".org") || !strcmp(ptr, ".ly"))
          return false; // not handling .come on or .organization etc
      }
      break;
    case '\'':
      if (!prev || IsPunct(prev)) {
        return true;
      }
      if (!next || IsPunct(next)) {
        return true;
      }
      /*
      if (strncmp(ptr, "'s ", 3) == 0) {
        // its callers responsibility to initialize this to false
        //word_has_apostrophe = true;
        ptr += 2;
        return true;
      } else {
        return false;
      }
      */
      /*
      if (!strncmp(ptr, "'t", 2) || !strncmp(ptr, "'ve", 2) ||
          !strncmp(ptr, "'ll", 2) || !strncmp(ptr, "'re", 2) || !strncmp(ptr, "'m", 2) ||
          !strncmp(ptr, "'em", 3))
       return false;
      */
      return false;
      break;
    case '@':
      if (prev && !IsPunct(prev))
        return true;
      return IsPunct(next);
      break;
    case '#':
      if (!next || (*next == '\0'))
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
    case ';':
      // fall thru
    case ':':
      if (next && *next != '\0' && punct_senti) {
        switch (*next) {
          case ')':
          case 'P':
          case 'D':
            (*punct_senti)++;
            break;
          case '(':
            (*punct_senti)--;
            break;
          case '-':
            if ((next+1) && (*(next+1) != '\0')) {
              if (*(next+1) == '(') {
                (*punct_senti)--;
              } else if (*(next+1) == ')') {
                (*punct_senti)++;
              }
            }
          default:
            break;
        }
      }
      if (prev && next)
        if (isdigit(*prev) && isdigit(*next))
          return false;
      break;
    case '!':
      if (punct_senti)
        (*punct_senti)++;
      break;
    case '?':
      if (punct_intent)
        (*punct_intent) = 1;
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

//bool StringUtils::IsIgnore(char *&ptr) {
bool IsIgnore(char *&ptr) {
#ifdef UTILS_DEBUG
  char* word = ptr;
#endif
  if (!ptr || '\0' == *ptr)
    return false;
  if ('@' == *ptr || !strncmp(ptr, "&#", 2) || !strncmp(ptr, "http://", 7) || !strncmp(ptr, "www.", 4)) {
    while (' ' != *(ptr+1) && '\0' != *(ptr+1)) {
      ptr++;
    }
#ifdef UTILS_DEBUG
    if (UTILS_DEBUG > 3) {
      char temp = *(ptr+1);
      *(ptr+1) = '\0'; 
      std::cout << "Ignore word: " << word << std::endl;
      *(ptr+1) = temp;
    }
#endif
    return true;
  }
  return false;
}

// TODO (balaji)
// this is a rudimentary test version, will write a detailed one later
//int StringUtils::Tokenize(const std::string& text, std::set<std::string>& tokens) {
int Tokenize(const std::string& text, std::set<std::string>& tokens) {

  unsigned int len = text.length();
  if (len < 1) {
    std::cout << "ERROR: empty string. no tokens\n";
    return -1;
  }

  std::string::size_type loc = 0;
  std::string::size_type prev = -1;
  std::string word;
  unsigned int size = 0;
  while ((loc = text.find(" ", prev+1)) != std::string::npos) {
    if ((size = loc-prev) > 1) {
      word.assign(text, prev+1, size-1); 
      tokens.insert(word);
    }
    prev = loc;
  }
  if (std::string::npos == loc) {
    if ((size = len-prev) > 1) {
      word.assign(text, prev+1, size); 
      tokens.insert(word);
    }
  }

  return tokens.size();

}

int ToLower(const char* input, char* output) {

  char* ptr = (char *) input;
  char* optr = output;
  while (ptr && *ptr != '\0') {
    if (*ptr > 64 && *ptr < 91) {
      *optr = *ptr + 32; 
    } else {
      *optr = *ptr;
    }
    ptr++;
    optr++;
  }
  *optr = '\0';

  return 0;
}

int PipeListToMap(unsigned char* buffer,
                  std::map<std::string, double>& map,
                  double value) { // value - default set to 1

  if (!buffer) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  unsigned char* start = NULL; 
  unsigned char* end = NULL; 
  unsigned int word_len = 0;
  unsigned int count = 0;

  start = buffer;
  end = (unsigned char*) strchr((char*) start, '|');
  word_len = 0;
  std::string word;
  while (start && end && *start != '\0') {
    word_len = end - start; 
    if (word_len < 1) {
      std::cerr << "ERROR: invalid buffer input. cannot convert PipeListToMap\n";
      break;
    }
    word.clear();
    word.assign((char *) start, word_len);
    if (map.find(word) != map.end()) {
      map[word] += value;
    } else {
      map[word] = value;
    }
    count++;
    start = end + 1;
    end = (unsigned char*) strchr((char*) start, '|');
  }
  start = NULL;
  end = NULL;

  return count; 
}

// copy elements of a set to a buffer, each element separated by a '|'
int MapToPipeList(std::map<std::string, double>& map,
                  unsigned char* buffer, unsigned int buffer_len,
                  unsigned int& list_len, unsigned int& list_count) {

  if (!buffer) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  if (map.empty()) {
    return 0;
  }

  list_len = 0;
  list_count = 0;
  std::map<std::string, double>::iterator map_iter;
  unsigned char* ptr = buffer;
  std::string element;
  int temp_len = 0;
  for (map_iter = map.begin(); map_iter != map.end(); map_iter++) {
    element.assign((*map_iter).first);
    element += ",";
    if ((list_len + element.length() + 21) >= buffer_len) { // WTH?
      break;
    }
    strcpy((char *) ptr, element.c_str()); 
    ptr += element.length();
    temp_len = sprintf((char *) ptr, "%f", (*map_iter).second);
    ptr += temp_len;
    strcpy((char *) ptr, "|");
    ptr += 1;
    list_count++;
    list_len += element.length();
    list_len += temp_len;
    list_len += 1;
  }

  return list_count;
}

int PipeListToStringMap(unsigned char* buffer, const unsigned int buffer_len,
                        unsigned int& list_len, unsigned int& list_count,
                        std::map<std::string, std::string>& map) {
  // this won't work. abandoning this. will come back later
  if (!buffer) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  if (list_len <= 0 || list_count <=0) {
    return 0;
  }

  unsigned char* start = NULL;
  unsigned char* end = NULL; 
  unsigned int word_len = 0;
  unsigned int count = 0;

  start = buffer;
  end = (unsigned char*) strchr((char*) start, '|');
  word_len = 0;
  std::string word;
  while (start && end && *start != '\0') {
    word_len = end - start; 
    word.clear();
    word.assign((char *) start, word_len);
    if (map.find(word) != map.end()) {
      map[word] += 1;
    } else {
      map[word] = 1;
    }
    count++;
    start = end + 1;
    end = (unsigned char*) strchr((char*) start, '|');
  }
  start = NULL;
  end = NULL;

  return count;
}

// copy elements of a set to a buffer, each element separated by a '|'
int StringMapToPipeList(std::map<std::string, std::string> map,
                  unsigned char* buffer, const unsigned int& buffer_len,
                  unsigned int& list_len, unsigned int& list_count) {

  if (!buffer) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  if (map.empty()) {
    return 0;
  }

  list_len = 0;
  list_count = 0;
  std::map<std::string, std::string>::iterator map_iter;
  unsigned char* ptr = buffer;
  std::string element;
  for (map_iter = map.begin(); map_iter != map.end(); map_iter++) {
    element.assign((*map_iter).first);
    element += ":";
    element += (*map_iter).second;
    if ((list_len + element.length() + 1) >= buffer_len) {
      break;
    }
    strcpy((char *) ptr, element.c_str()); 
    ptr += element.length();
    strcpy((char *) ptr, "|");
    ptr += 1;
    list_count++;
    list_len += element.length();
    list_len += 1;
  }

  return list_count;
}

int PipeListToSet(unsigned char* buffer, std::set<std::string>& set) {

  if (!buffer) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  unsigned char* start = NULL; 
  unsigned char* end = NULL; 
  unsigned int word_len = 0;
  unsigned int count = 0;

  start = buffer;
  end = (unsigned char*) strchr((char*) start, '|');
  word_len = 0;
  std::string word;
  while (start && end && *start != '\0') {
    word_len = end - start; 
    word.clear();
    word.assign((char *) start, word_len);
    set.insert(word);
    count++;
    start = end + 1;
    end = (unsigned char*) strchr((char*) start, '|');
  }
  start = NULL;
  end = NULL;

  return count; 
}

// copy elements of a set to a buffer, each element separated by a '|'
int SetToPipeList(std::set<std::string>& set,
                  unsigned char* buffer, unsigned int buffer_len,
                  unsigned int& list_len, unsigned int& list_count) {

  if (!buffer) {
    std::cerr << "ERROR: invalid input\n";
    return -1;
  }

  if (set.empty()) {
    return 0;
  }

  list_len = 0;
  list_count = 0;
  std::set<std::string>::iterator set_iter;
  unsigned char* ptr = buffer;
  std::string element;
  for (set_iter = set.begin(); set_iter != set.end(); set_iter++) {
    element.assign(*set_iter);
    if ((list_len + element.length() + 1) >= buffer_len) {
      break;
    }
    strcpy((char *) ptr, element.c_str()); 
    ptr += element.length();
    strcpy((char *) ptr, "|");
    ptr += 1;
    list_count++;
    list_len += element.length();
    list_len += 1;
  }

  return list_count;
}

} // namespace inagist_utils
