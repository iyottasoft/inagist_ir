#include "stemmer.h"
#include <iostream>
#include <cstring>
#include <cstdlib>
#include <cctype>
#include "porter_stemmer.h"
#include "script_detector.h"
#include "utf8.h"

//#define DEBUG 1

namespace inagist_search {

Stemmer::Stemmer() {
  porter_stemmer::init_stemmer();
  memset(m_buffer, '\0', MAX_STEM_TEXT_LEN); 
}

Stemmer::~Stemmer() {
  porter_stemmer::free_stemmer();
  memset(m_buffer, '\0', MAX_STEM_TEXT_LEN); 
}


// TODO (balaji) - stemmer dictionary file is currently not used.
int Stemmer::Init(const char *stopwords_file,
                  const char *dictionary_file,
                  const char *stemmer_dictionary_file) {

  if (!stopwords_file || !dictionary_file || !stemmer_dictionary_file) {
    std::cout << "ERROR: invalid dictionary file(s)\n";
    return -1;
  }

  if (m_exclude_dictionary.Load(stopwords_file) < 0) {
    std::cout << "ERROR: could not load stopwords file " << stopwords_file << std::endl;
    return -1;
  }

  if (m_exclude_dictionary.Load(dictionary_file) < 0) {
    std::cout << "ERROR: could not load dictionary file " << dictionary_file << std::endl;
    return -1;
  }

  if (m_include_dictionary.Load(stemmer_dictionary_file) < 0) {
    std::cout << "ERROR: could not load stemmer dictionary file " << stemmer_dictionary_file << std::endl;
    return -1;
  }

  return 0;
}

unsigned int Stemmer::Stem(const std::string& text,
                           const unsigned int& output_buffer_len,
                           char*& pipe_delimited_output) {

  std::set<std::string> stems;
  if (Stem(text, stems) < 0) {
    std::cout << "ERROR: stemming failed\n";
    return -1;
  }

  std::set<std::string>::iterator stems_iter;
  unsigned int total_len = 0;
  unsigned int len = 0;
  char* ptr = pipe_delimited_output;
  for (stems_iter = stems.begin(); stems_iter != stems.end(); stems_iter++) {
    len = (*stems_iter).length();
    total_len += (len + 1); // 1 for the pipe
    if (total_len < output_buffer_len) {
      strcpy(ptr, (*stems_iter).c_str());
      ptr += len;
      strcpy(ptr, "|");
      ptr++;
    } else {
#ifdef DEBUG
      std::cout << "ERROR: Not enuf space in the keywords buffer\n";
#endif
      *pipe_delimited_output = '\0';
      stems.clear();
      return -1;
    }
  }
  return total_len;
}

int Stemmer::Stem(const std::string& text, std::set<std::string>& stems) {

  if (text.length() < 1)
    return -1;

  strcpy(m_buffer, text.c_str());

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

  // script detection
  char *end = strchr(m_buffer, '\0');
  std::string script = "uu";
  int code_point = 0;
  std::string script_temp;
  int script_count = 0;
  int english_count = 0;
  
  unsigned int stemmed_word_len = 0;

  // the whole thing starts here
  ptr = m_buffer;

#ifdef DEBUG
  std::cout << std::endl << "original query: " << m_buffer << std::endl;
#endif

  // go to the first word, ignoring handles and punctuations
  char *prev = NULL;
  while (ptr && '\0' != *ptr && (' ' == *ptr || (ispunct(*ptr) && m_utils.IsPunct(ptr, prev, ptr+1)) || m_utils.IsIgnore(ptr))) {
    prev = ptr;
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
    if (' ' == *probe || '\0' == *probe || (ispunct(*probe) && (is_punct = m_utils.IsPunct(probe, probe-1, probe+1)))) {

      current_word_delimiter = *probe;
      current_word_end = probe;
      *probe = '\0';
      current_word_len = current_word_end - current_word_start;
#ifdef DEBUG
      std::cout << "current word: " << current_word_start << std::endl;
#endif

      // exclude stopwords and dictionary words - no need to stem them
      if (m_exclude_dictionary.Find(current_word_start) == 0) {
        stemmed_word_len = porter_stemmer::stem(current_word_start, current_word_len, m_stemmed_word);
        if (strcmp(current_word_start, m_stemmed_word) != 0) {
          if (m_include_dictionary.Find(m_stemmed_word) == 1) {
            // if the stemmed word is in the stemmer dictionary, insert it
            stems.insert(std::string(m_stemmed_word));
          } else {
            // so what do we do if the stemmed word is not in dictionary?
            // lets first check if the original word was in dictionary. if so don't bother anymore
            if (m_include_dictionary.Find(current_word_start) == 0) {
              // so, original word was not a dictionary word
              // has the plural 's' been removed?
              if ((current_word_len == (stemmed_word_len + 1)) &&
                  ('s' == *(current_word_start + stemmed_word_len))) {
                stems.insert(std::string(m_stemmed_word));
              // if not, lets consider "es" endings
              } else if ((current_word_len == (stemmed_word_len + 2) &&
                  (strcmp(current_word_start + stemmed_word_len, "es") == 0) &&
                  ('e' != *(current_word_start + stemmed_word_len)))) {
                stems.insert(std::string(m_stemmed_word));
              }
            }
          }
        }
      } else {
#ifdef DEBUG
        std::cout << current_word_start << " is a dictionaty word\n";
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
          std::cerr << "ERROR: Fatal Exception trying to access unallocated memory space\n";
          exit(-1);
        }

        // find the next position of ptr
        // IsIgnore will literally ignore the word by changing the cursor to next word end
        is_ignore_word = false;
        is_punct = false;
        while ('\0' != *ptr && (' ' == *ptr || (ispunct(*ptr) && (is_punct = m_utils.IsPunct(ptr, ptr-1, ptr+1))) || (is_ignore_word = m_utils.IsIgnore(ptr)))) {
          ptr++;
        }

        if (ptr && '\0' != *ptr) {
          next_word_start = ptr;
          num_words++;
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
    if (script_count > 9 || english_count > 20) {
      probe++;
    } else {
      try {
        code_point = utf8::next(probe, end);
        if (code_point > 0x7F) {
          if (sd.DetectScript(code_point, script_temp) > 0) {
            if (script_temp != "en") {
              if (script_temp != script) {
                script_count = 0;
                script = script_temp;
              } else {
                script_count++;
                if (script_count > 9) {
#ifdef DEBUG
                  std::cout << "this is a non-english tweet. don't bother stemming\n";
#endif
                  break;
                }
              }
            }
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
    }
  }

#ifdef DEBUG
  std::cout << "num words: " << num_words << std::endl;
#endif

  // deinitialize script detector
  sd.Clear();

  return stems.size();
}

}
