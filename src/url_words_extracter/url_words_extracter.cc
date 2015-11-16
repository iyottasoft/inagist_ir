#include "url_words_extracter.h"
#include <cstring>
#include "script_detector_utils.h"
#include "string_utils.h"

#ifdef I18N_ENABLED
#include "utf8.h"
#endif // I18N_ENABLED

#define MAX_DEBUG_BUFFER_LEN 1024
//#define I18N_ENABLED 0

// IMPORTANT NOTE - this code is copied from keytuples_extracter.cc
// the looping over the words in the url is somewhat roundabout because of this copying.
// but keeping it that way for the time being, beco
// 1. we may have to merge this functionality in keytuples_extracter for efficiency
// 2. there might be some use in knowing the prev and next word while analyzing the current word.

namespace inagist_trends {
  // unless otherwise specified functions return 0 or NULL or false as default
  // return values less than 0 are likely error codes

URLwordsExtracter::URLwordsExtracter() {
  m_debug_level = 0;
#ifdef GM_DEBUG
  if (GM_DEBUG > 0) {
    m_debug_level = GM_DEBUG;
  }
#endif // GM_DEBUG
}

URLwordsExtracter::~URLwordsExtracter() {
  if (Clear() < 0)
    std::cerr << "ERROR: Clear() failed\n";
}

int URLwordsExtracter::SetDebugLevel(unsigned int& debug_level) {
  debug_level = debug_level;
  return 0;
}

// every input parameter can be set to NULL!
//
// stopwords, dictionary, unsafe dictionary file paths, if not given will
// just mean that those dictionaries will not be populated
// 
//
int URLwordsExtracter::Init(const char *stopwords_file,
    const char *dictionary_file,
    const char *unsafe_dictionary_file) {
  // load dictionaries
  if (stopwords_file) {
#ifdef GM_DEBUG
    if (m_debug_level > 3) {
      std::cout << "Info: loading stopwords file - " << stopwords_file << std::endl;
    }
#endif // GM_DEBUG
    int ret = m_stopwords_dictionary.Load(stopwords_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load stopwords file " \
                << stopwords_file << " into dictionary set\n";
      return -1;
    }
  }

  if (dictionary_file) {
#ifdef GM_DEBUG
    if (m_debug_level > 3) {
      std::cout << "Info: loading dictionary file - " << dictionary_file << std::endl;
    }
#endif // GM_DEBUG
    int ret = m_dictionary.Load(dictionary_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load dictionary file " \
                << dictionary_file << " into dictionary set\n";
      return -1;
    }
  }

  if (unsafe_dictionary_file) {
#ifdef GM_DEBUG
    if (m_debug_level > 3) {
      std::cout << "Info: loading unsafe dictionary file - " << unsafe_dictionary_file << std::endl;
    }
#endif // GM_DEBUG
    int ret = m_unsafe_dictionary.Load(unsafe_dictionary_file);
    if (ret < 0) {
      std::cerr << "ERROR: could not load unsafe file " \
                << unsafe_dictionary_file << " into dictionary set\n";
      return -1;
    }
  }

  return 0;
}

int URLwordsExtracter::Clear() {

  m_stopwords_dictionary.Clear();
  m_dictionary.Clear();
  m_unsafe_dictionary.Clear();

  return 0;
}

int URLwordsExtracter::LoadClassifierDictionary(const char* classifier_dictionary_file) {
  int ret = m_dictionary.Load(classifier_dictionary_file);
  if (ret < 0) {
    std::cerr << "ERROR: could not load classifier dictionary file: " \
              << classifier_dictionary_file;
    return -1;
  }
  return 0;
}

// this is an helper function used to make a|b|c| kind of strings given a, b, c in consecutive calls
inline void URLwordsExtracter::Insert(unsigned char* buffer, unsigned int& current_len,
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

int URLwordsExtracter::GetURLwords(unsigned char* text_buffer, const unsigned int& text_buffer_len,
                         const unsigned int& text_len,
                         unsigned char* url_words_buffer,
                         const unsigned int& url_words_buffer_len,
                         unsigned int& url_words_len,
                         unsigned int& url_words_count
                        ) {
  return GetURLwords(text_buffer, text_buffer_len, text_len,
                     m_dictionary,
                     m_stopwords_dictionary,
                     m_unsafe_dictionary,
                     url_words_buffer, url_words_buffer_len,
                     url_words_len, url_words_count,
                     m_debug_level
                    );
}

int URLwordsExtracter::GetURLwords(unsigned char* url_buffer, const unsigned int& url_buffer_len,
                        const unsigned int& url_len,
                        inagist_utils::DictionarySet& m_dictionary,
                        inagist_utils::DictionarySet& m_stopwords_dictionary,
                        inagist_utils::DictionarySet& m_unsafe_dictionary,
                        unsigned char* url_words_buffer,
                        const unsigned int& url_words_buffer_len,
                        unsigned int& url_words_len,
                        unsigned int& url_words_count,
                        unsigned int debug_level
                       ) {

  if (!url_buffer || url_buffer_len < 1 || url_len < 1) {
    std::cout << "ERROR: invalid buffer(s) at input\n";
    return -1;
  }

  // these pointers are used throughout the function to move over the string
  unsigned char *ptr = NULL;
  unsigned char *end = NULL;

  int ret_val = 0;

  if (!url_words_buffer) {
#ifdef GM_DEBUG
    std::cerr << "ERROR: invalid url_words buffer\n";
#endif // GM_DEBUG
    return -1;
  }
  *url_words_buffer = '\0';
  url_words_len = 0;
  url_words_count = 0;

  unsigned char *probe = NULL;
  unsigned char current_word_delimiter;
  unsigned char prev_word_delimiter;
  unsigned char next_word_delimiter;

  unsigned int current_word_len = 0;
  unsigned int next_word_len = 0;
  int num_words = 0;
  int num_stop_words = 0;
  int num_dict_words = 0;

  unsigned char *current_word_start = NULL;
  unsigned char *current_word_end = NULL;
  std::string current_word;
  unsigned char *prev_word_start = NULL;
  unsigned char *prev_word_end = NULL;
  std::string prev_word;
  unsigned char *next_word_start = NULL;
  unsigned char *next_word_end = NULL;
  std::string next_word;

  // TODO (balaji) use bit map and masks to reduce comparisons
  bool current_word_not_alpha = false;
  bool prev_word_not_alpha = false;
  bool next_word_not_alpha = false;

  bool current_word_stop = false;
  bool current_word_dict = false;
  bool prev_word_stop = false;
  bool prev_word_dict = false;
  bool next_word_stop = false;
  bool next_word_dict = false;

  bool url_has_unsafe_words = false;

  // the whole thing starts here!
  if ((ptr = (unsigned char*) strstr((char*) url_buffer, "http")) == NULL &&
      (ptr = (unsigned char*) strstr((char*) url_buffer, "ftp")) == NULL) {
#ifdef GM_DEBUG
    if (debug_level > 2) {
      std::cout << "INFO: no url found\n";
    }
#endif // GM_DEBUG
    return 0;
  }
  end = ptr + strlen((char*)ptr);

#ifdef GM_DEBUG
  if (debug_level > 5)
    std::cout << std::endl << "INFO: original query: " << std::string((char *) url_buffer) << std::endl << std::endl;
#endif // GM_DEBUG

  if (!ptr || '\0' == *ptr) {
#ifdef GM_DEBUG
    if (debug_level > 2) {
      std::cout << "INFO: either the input is empty or has ignore words only" << std::endl;
    }
#endif // GM_DEBUG
    return 0;
  }

  // go past http header
  int len = 0;
  if ((len = (7 + strncmp((char*) ptr, "http://", 7))) == 7 ||
      (len = (8 + strncmp((char*) ptr, "https://", 8))) == 8 ||
      (len = (6 + strncmp((char*) ptr, "ftp://", 6))) == 6
     ) {
    ptr += len;
  } else {
#ifdef GM_DEBUG
    if (debug_level > 2) {
      std::cout << "INFO: this is not a URL" << std::endl;
    }
#endif // GM_DEBUG
    return 0;
  }

  // ignore the domain name
  while (ptr && '\0' != *ptr && '/' != *ptr) {
#ifdef I18N_ENABLED
    try {
      utf8::next(ptr, end);
    } catch (...) {
#ifdef GM_DEBUG
      if (debug_level > 1) {
        std::cout << "EXCEPTION 1: utf8 returned exception" << std::endl;
        std::cout << std::endl << "original query: " << std::string((char *) url_buffer) << std::endl << std::endl;
      }
#endif // GM_DEBUG
      return -1;
    }
#else // I18N_ENABLED
    ptr++;
#endif // I18N_ENABLED
  }

  // go past the "/" after the domain name
  while (ptr && '/' == *ptr) {
    ptr++;
  }

  if (!ptr || '\0' == *ptr ||
      ' ' == *ptr || '?' == *ptr || '&' == *ptr || '.' == *ptr) {
    return 0;
  }

  current_word_start = ptr;

  if (!isalpha(*ptr)) {
    current_word_not_alpha = true;
  } else if (isupper(*ptr)) {
    *ptr += 32;
  }

  // now lets find the end of the current word
  while ((ptr && '\0' != *ptr) && // is the string still valid
         ('/' != *ptr && '-' != *ptr && '_' != *ptr) &&  // have we found the end of the word?
         (' ' != *ptr && '?' != *ptr && '&' != *ptr && '.' != *ptr)) { // have we found letters beyond which we need not go

    // while reading the current word, lets learn few things about this word
    if (!isalpha(*ptr)) {
      current_word_not_alpha = true;
    } else if (isupper(*ptr)) {
      *ptr += 32;
    }

#ifdef I18N_ENABLED
    try {
      utf8::next(ptr, end);
    } catch (...) {
#ifdef GM_DEBUG
      if (debug_level > 1) {
        std::cout << "EXCEPTION 2: utf8 returned exception" << std::endl;
        std::cout << std::endl << "original query: " << std::string((char *) url_buffer) << std::endl << std::endl;
      }
#endif // GM_DEBUG
      return -1;
    }
#else // I18N_ENABLED
    ptr++;
#endif // I18N_ENABLED
  } // while loop


  if ((!ptr || '\0' == *ptr) ||
      (' ' == *ptr || '?' == *ptr || '&' == *ptr || '.' == *ptr)) {
#ifdef GM_DEBUG
    if (debug_level > 2) {
      std::cout << "INFO: either the input is empty or there is only one word" << std::endl;
    }
#endif // GM_DEBUG
    return 0;
  }

  current_word_end = ptr;
  current_word_delimiter = *ptr;
  current_word_len = current_word_end - current_word_start;
  current_word.assign((char *) current_word_start, current_word_len);
  num_words++;

  if (!current_word_not_alpha) {
    // stop words
    if (m_stopwords_dictionary.Find(current_word) == 1) {
      current_word_stop = true;
      num_stop_words++;
#ifdef GM_DEBUG
      if (debug_level > 5) {
        std::cout << "current word: " << current_word << " :stopword" << std::endl;
      }
#endif // GM_DEBUG
    } else {
      current_word_stop = false;
    }

    // dictionary words
    if (m_dictionary.Find(current_word) == 1) {
      current_word_dict = true;
      num_dict_words++;
#ifdef GM_DEBUG
      if (debug_level > 5) {
        std::cout << "current word: " << current_word << " :dictionary word" << std::endl;
      }
#endif // GM_DEBUG
    } else {
      current_word_dict = false;
    }
  
    if (m_unsafe_dictionary.Find(current_word) == 1) {
      url_has_unsafe_words = true;
    }
  } // current_word_alpha?

  // go to the next word
#ifdef I18N_ENABLED
  try {
    utf8::next(ptr, end);
  } catch (...) {
#ifdef GM_DEBUG
    if (debug_level > 1) {
      std::cout << "EXCEPTION 3: utf8 returned exception" << std::endl;
      std::cout << std::endl << "original query: " << std::string((char *) url_buffer) << std::endl << std::endl;
    }
#endif // GM_DEBUG
    return -1;
  }
#else // I18N_ENABLED
  ptr++;
#endif // I18N_ENABLED

  while ((ptr && '\0' != *ptr) &&
         (' ' != *ptr && '?' != *ptr && '&' != *ptr && '.' != *ptr) && // show stoppers
         ('/' == *ptr || '_' == *ptr || '-' == *ptr)) { // there might be "//" instead of just "/", for example
    ptr++;
  }

  if ((!ptr || '\0' == *ptr) ||
      (' ' == *ptr || '?' == *ptr || '&' == *ptr || '.' == *ptr)) {
    next_word_start = NULL;
#ifdef GM_DEBUG
    if (debug_level > 2) {
      std::cout << "INFO: only one word found\n";
    }
#endif // GM_DEBUG
    return 0;
  }

  next_word_start = ptr;
  num_words++;
  // remember - this is start of word
  if (!isalpha(*next_word_start)) {
    next_word_not_alpha = true;
  } else if (isupper(*next_word_start)) {
    *next_word_start += 32;
  }

  // now we need to achieve the following
  // probe = ptr + 1;
#ifdef I18N_ENABLED
  probe = ptr;
  try {
    if (probe < end) {
      utf8::next(probe, end);
    } else {
#ifdef GM_DEBUG
      if (debug_level > 0) {
        std::cout << "ERROR: invalid pointers?" << std::endl;
      }
#endif // GM_DEBUG
      return 0;
    }
  } catch (...) {
#ifdef GM_DEBUG
    if (debug_level > 1) {
      std::cout << "EXCEPTION 4: utf8 returned exception" << std::endl;
      std::cout << std::endl << "original query: " << std::string((char *) url_buffer) << std::endl << std::endl;
    }
#endif // GM_DEBUG
    return -1;
  }
#else // I18N_ENABLED
  probe = ptr + 1;
#endif // I18N_ENABLED

  // this loop works between second letter to end punctuation for each word
  while (ptr && probe/* && *ptr != '\n' && *ptr != '\0'*/) {

    // end of word?
    if (('\0' == *probe || '?' == *probe || '&' == *probe || ' ' == *probe || '.' == *probe) ||
        ('/' == *probe || '-' == *probe || '_' == *probe)) {

      // word boundary
      if (next_word_start) {
        next_word_delimiter = *probe;
        next_word_end = probe;
        next_word_len = next_word_end - next_word_start;
        next_word.assign((char *) next_word_start, next_word_len);
      }

#ifdef GM_DEBUG
      if (debug_level > 5) {
        std::cout << std::endl;
        std::cout << "prev word: " << prev_word << std::endl;
        std::cout << "current word: " << current_word << std::endl;
        std::cout << "next word: " << next_word << std::endl;
        std::cout << std::endl;
      }
#endif // GM_DEBUG

      if (next_word_start) {
        if (!next_word_not_alpha) {
        // stop words
        if (m_stopwords_dictionary.Find(next_word) == 1) {
          next_word_stop = true;
          num_stop_words++;
#ifdef GM_DEBUG
          if (debug_level > 5) {
            std::cout << "next word: " << next_word << " :stopword" << std::endl;
          }
#endif // GM_DEBUG
        } else {
          next_word_stop = false;
        }

        // dictionary words
        if (m_dictionary.Find(next_word) == 1) {
          next_word_dict = true;
          num_dict_words++;
#ifdef GM_DEBUG
          if (debug_level > 5) {
            std::cout << "next word: " << next_word << " :dictionary word" << std::endl;
          }
#endif // GM_DEBUG
        } else {
          next_word_dict = false;
        }

        if (m_unsafe_dictionary.Find(next_word) == 1) {
          url_has_unsafe_words = true;
        }

        } // current_word_alpha
      } // next_word_start

      if (!current_word_stop &&
          !current_word_dict &&
          !current_word_not_alpha &&
          (current_word_len > 2 || current_word_delimiter != '/')) {
        if ((url_words_len + current_word_len + 1) < url_words_buffer_len) {
          Insert(url_words_buffer, url_words_len,
                 current_word_start, current_word_len,
                 url_words_count); 
        }
      }

#ifdef GM_DEBUG
      if (debug_level > 5) {
        std::cout << std::endl;
      }
#endif // GM_DEBUG

      // exit conditions
      if ('\0' == current_word_delimiter ||
          ' ' == current_word_delimiter ||
          '?' == current_word_delimiter ||
          '&' == current_word_delimiter ||
          '.' == current_word_delimiter ||
          !next_word_start || '\0' == *next_word_start) {
        break;
      }

      // ****************************** beginning of next cycle ******************************* //

      prev_word_end = current_word_end;
      prev_word_start = current_word_start;

      prev_word_stop = current_word_stop;
      prev_word_dict = current_word_dict;
      prev_word_delimiter = current_word_delimiter;
      prev_word_not_alpha = current_word_not_alpha;
      prev_word = current_word;

      current_word_end = next_word_end;
      current_word_start = next_word_start;

      current_word_stop = next_word_stop;
      current_word_dict = next_word_dict;
      current_word_delimiter = next_word_delimiter;
      current_word_not_alpha = next_word_not_alpha;
      current_word_len = next_word_len;
      current_word = next_word;

      next_word_start = NULL;
      next_word_end = NULL;
      next_word_stop = false;
      next_word_dict = false;
      next_word_not_alpha = false;
      next_word_delimiter = '\0';
      next_word.clear();

      // BE CAREFUL ABOUT WHAT IS NEXT WORD OR CURRENT WORD NOW

      // if current word is not the known last word, briefly probe to see if next word exists
      if ('\0' != current_word_delimiter ||
          ' ' != current_word_delimiter ||
          '?' != current_word_delimiter ||
          '&' != current_word_delimiter ||
          '.' != current_word_delimiter) {

        ptr = probe + 1;
        if (!ptr) {
          std::cerr << "ERROR: Fatal Exception trying to access unallocated memory space\n";
          return -1;
        }

        // find the next position of ptr

        // go past "//" etc
        while ((ptr && '\0' != *ptr) && ('/' == *ptr || '-' == *ptr || '_' == *ptr)) {
          ptr++;
        }

        // is there a next word?
        if (ptr && '\0' != *ptr &&
            (' ' != *ptr && '?' != *ptr && '&' != *ptr && '.' != *ptr)) {
          next_word_start = ptr;

          // after finding the start of next word, probe shud be at the same place as ptr
          probe = ptr;

          if (!isalpha(*next_word_start)) {
            next_word_not_alpha = true;
          } else if (isupper(*next_word_start)) {
            *next_word_start += 32;
          }
        } else {
          // placing the probe before '\0' so that loop will make it probe++
          // loop will terminate in the next cycle
          probe = ptr-1;
        }
      } // check for current word delimiter 

    } else {

      if (!isalpha(*probe)) {
        next_word_not_alpha = true;
      } else if (isupper(*probe)) {
        *probe += 32;
      }

    }

    // a mere cog in a loop wheel, but a giant killer if commented
    if (probe && *probe != '\0') {
#ifdef I18N_ENABLED
        try {
          utf8::next(probe, end);
        } catch (...) {
#ifdef GM_DEBUG
          if (debug_level > 0) {
            std::cout << "Exception 5: " << " " << probe << std::endl;
            std::cout << std::endl << "original query: " << std::string((char *) url_buffer) << std::endl << std::endl;
          }
#endif // GM_DEBUG
          return -1;
        }
#else // I18N_ENABLED
        probe++;
#endif // I18N_ENABLED
    }
  }
  probe = NULL;
  ptr = NULL;
  end = NULL;

#ifdef GM_DEBUG
  if (debug_level > 5) {
    std::cout << std::endl << "\norginal query: " << std::string((char *) url_buffer) << std::endl;
    std::cout << "num words: " << num_words << std::endl;
    std::cout << "num stop words: " << num_stop_words << std::endl;
    std::cout << "num dict words: " << num_dict_words << std::endl;
  }
#endif // GM_DEBUG

  ret_val += url_words_count;

#ifdef GM_DEBUG
  if (debug_level > 2) {
    std::cout << "input: " << url_buffer << std::endl;
    std::cout << "url_words (" << url_words_count << "): " << url_words_buffer << std::endl;
  }
#endif // GM_DEBUG

  return ret_val;

}

} // namespace inagist_trends

