#include "keywords_extract.h"
#include <cstring>

//#define DEBUG 0

namespace inagist_trends {
  // unless otherwise specified functions return 0 or NULL or false as default
  // return values less than 0 are likely error codes

using std::cout;
using std::endl;
using std::string;

KeywordsExtract::KeywordsExtract() {
}

KeywordsExtract::~KeywordsExtract() {
  if (DeInit() < 0)
    std::cerr << "ERROR: DeInit() failed\n";
}

// every input parameter is optional!
//
// stopwords, dictionary and stemmer dictionary file paths, if not given will
// just mean that those dictionaries will not be populated
// 
// if input_file or output_file are given, they will be initialized
// the above are two are typically used by a test program which will
// subsequently call GetKeywords() and PrintKeywords()
//
int KeywordsExtract::Init(const char *stopwords_file,
    const char *dictionary_file,
    const char *stemmer_dictionary_file,
    const char *input_file,
    const char *output_file) {

  // load dictionaries
  if (stopwords_file) {
    int ret = LoadDictionary(stopwords_file, m_stopwords_dictionary);
    if (ret < 0) {
      std::cerr << "ERROR: could not load stopwords file into dictionary\n";
      return -1;
    }
    //PrintDictionary(m_stopwords_dictionary);
  }

  if (dictionary_file) {
    int ret = LoadDictionary(dictionary_file, m_dictionary);
    if (ret < 0) {
      std::cerr << "ERROR: could not load dictionary file into dictionary\n";
      return -1;
    }
    //PrintDictionary(m_dictionary);
  }

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

int KeywordsExtract::DeInit() {
  //std::cout << "closing input stream\n";
  if (m_tweet_stream && m_tweet_stream.is_open())
    m_tweet_stream.close();

  //std::cout << "clearing dictionaries\n";
  m_dictionary.clear();
  m_stopwords_dictionary.clear();

  //std::cout << "closing output stream\n";
  if (m_out_stream && m_out_stream.is_open())
    m_out_stream.close();

  //std::cout << "deinit done\n";
  return 0;
}

// this function expects the dictionary words in the following format:
//
// one word or phrase per line
// a single newline character at the end of the line
// lower case expected in most cases
// upper case or mixed case will be inserted as is
// no unnecessary blankspace anywhere. word phrases separated by single spaces
// no empty lines

// the caller MUST ensure that the above conditions are met
// this function checks nothing of the above. just inserts whatever is given
//
int KeywordsExtract::LoadDictionary(const char* file, string_hash_set &dictionary) {
  if (NULL == file) {
    std::cerr << "ERROR: invalid dictionary file\n";
    return -1;
  }

  std::ifstream ifs(file);
  if (!ifs) {
    std::cerr << "ERROR: error opening dictionary file\n";
    return -1;
  }

  std::string str;
  while (getline(ifs, str)) {
    dictionary.insert(str.c_str());
  }

  ifs.close();

  return 0;
}

int KeywordsExtract::DictionaryLookup(char *word) {
  if (m_stopwords_dictionary.find(word) != m_stopwords_dictionary.end())
    cout << word << "stopword" << endl;
  if (m_dictionary.find(word) != m_dictionary.end())
    cout << word << "dictionary word" << endl;
  return 0;
}

int KeywordsExtract::PrintDictionary(string_hash_set dictionary) {
  string_hash_set::const_iterator iter;
  for (iter = dictionary.begin(); iter != dictionary.end(); iter++)
    std::cout << *iter << std::endl;
  return 0;
}

void KeywordsExtract::PrintKeywords(std::set<std::string> &keywords_set) {
  std::set<std::string>::iterator iter;

  for (iter = keywords_set.begin(); iter != keywords_set.end(); iter++)
//  std::cout << *iter << " ";
//std::cout << std::endl;
    std::cout << *iter << std::endl;
}

// this function isn't unicode safe
// TODO (balaji) for ascii, we can ofcourse use an array lookup to speed up
bool KeywordsExtract::IsPunct(char *ptr, char *prev, char *next) {
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
      if (prev)
        if (*prev == ' ' || !IsPunct(prev))
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
    default:
      break;
  }

  return true;
}

// passing as ptr-to-ptr instead of ref-to-ptr to get it be compiled using gcc
bool KeywordsExtract::IsIgnore(char **ptr) {
  if (!(*ptr) || '\0' == **ptr)
    return false;
  if ('@' == **ptr || !strncmp(*ptr, "&#", 2) || !strncmp(*ptr, "http://", 7) || !strncmp(*ptr, "www.", 4)) {
    printf("%s is ignore word\n", *ptr);
    while (' ' != *(*ptr+1) && '\0' != *(*ptr+1)) {
      *ptr += 1;
    }
    return true;
  }
  return false;
}

int KeywordsExtract::GetKeywords(char *str, std::set<std::string> &keywords_set) {
  if (!str)
    return -1;

  char *ptr = NULL;
  char *probe = NULL;;
  char current_word_delimiter;
  char prev_word_delimiter;

  //unsigned in_len = 0;
  //unsigned out_len = 0;
  unsigned int current_word_len = 0;
  int score = 0;

  char *current_word_start = NULL;
  char *current_word_end = NULL;
  char *prev_word_start = NULL;
  char *prev_word_end = NULL;
  char *next_word_start = NULL;

  char *caps_entity_start = NULL;
  char *caps_entity_end = NULL;
  char *stopwords_entity_start = NULL;
  char *stopwords_entity_end = NULL;

  // TODO (balaji) use bit map and masks to reduce comparisons
  //bool sentence_start = true;
  bool current_word_caps = false;
  bool current_word_all_caps = false;
  bool current_word_has_mixed_case = false;
  bool prev_word_caps = false;
  bool prev_word_all_caps = false;
  bool prev_word_has_mixed_case = false;
  bool next_word_caps = false;
  bool next_word_all_caps = false;
  bool invisible_word_before_next = false;

  bool current_word_stop = false;
  bool current_word_dict = false;
  bool prev_word_stop = false;
  bool prev_word_dict = false;

  //bool second_letter_has_caps = false;

  // misc
  char *pch = NULL;
  char ch;

  // go to the first word, ignoring handles and punctuations
  ptr = str;
  while (ptr && '\0' != *ptr && (' ' == *ptr || IsPunct(ptr) || IsIgnore(&ptr))) {
    ptr++;
  }

  if (!ptr || '\0' == *ptr)
    return 0;

  if (isupper(*ptr)) {
    current_word_caps = true;
    current_word_all_caps = true;
  }

  current_word_start = ptr;
  probe = ptr + 1;

#ifdef DEBUG
  cout << endl;
#endif

  while (ptr && probe && ptr != '\0') {
    if (' ' == *probe || '\0' == *probe || IsPunct(probe, probe-1, probe+1)) {

      // TODO (balaji) sanity checks. remove these after stress tests
      if (NULL != stopwords_entity_end)
        std::cout << "ERROR: stopswords entity end is not null. did you not write it before?" << std::endl;
      if (NULL != caps_entity_end)
        std::cout << "ERROR: caps entity end is not null. did you not write it before?" << std::endl;
      // word boundary
      score = 0;

      current_word_delimiter = *probe;
      current_word_end = probe;
      *probe = '\0';

#ifdef DEBUG
      cout << current_word_start;
#endif

      current_word_len = current_word_end - current_word_start;
      if (current_word_len < 2)
        score-=5;

      if ('#' == *current_word_start) {
        score++;
      }

      if (current_word_all_caps) {
        score--;
        if (current_word_len > 1 && current_word_len < 6) {
          score++;
#ifdef DEBUG
          cout << " :all caps";
#endif
        } else {
#ifdef DEBUG
          cout << " :all caps but bad length";
#endif
        }
      } else if (current_word_has_mixed_case) {
        score++;
#ifdef DEBUG
        cout << " :mixed case";
#endif
      } else if (current_word_caps) {
        score++;
#ifdef DEBUG
        cout << " :starts with caps";
#endif
      }

      // stop words
      if ((m_stopwords_dictionary.find(ptr) != m_stopwords_dictionary.end())) {
        if (!strncmp(ptr, "of", 2)) {
          if (prev_word_start == NULL || !prev_word_caps) {
            current_word_stop = true;
            score--;
#ifdef DEBUG
            cout << " :stopword";
#endif
          }
        } else {
        current_word_stop = true;
        score--;
#ifdef DEBUG
        cout << " :stopword";
#endif
        }
      } else {
        current_word_stop = false;
      }

      // dictionary words
      if (current_word_caps)
        *ptr = tolower(*ptr);
      if ((m_dictionary.find(ptr) != m_dictionary.end())) {
        current_word_dict = true;
        score--;
#ifdef DEBUG
        cout << " :dictionary word";
#endif
      } else {
        current_word_dict = false;
      }
      if (current_word_caps)
        *ptr = toupper(*ptr);

      if (score > 0) {
        if ((pch = strstr(ptr, "\'s"))) {
          ch = *pch;
          *pch = '\0';
          keywords_set.insert(string(ptr));
          *pch = ch;
        } else {
          keywords_set.insert(string(ptr));
        }
      }

      if (prev_word_end)
        *prev_word_end = prev_word_delimiter;

      // if current word is not the known last word, briefly probe to see if next word exists
      if ('\0' != current_word_delimiter) {
        ptr = probe + 1;
        if (!ptr) {
          std::cerr << "ERROR: Fatal Exception trying to access unallocated memory space\n";
          exit(-1);
        }

        // find the next position of ptr
        // IsIgnore will literally ignore the word by chaging the cursor to next word end
        // &= is to handle rare case where multiple continous ignore words reset invisible_word_before_next
        while ('\0' != *ptr && (' ' == *ptr || IsPunct(ptr, ptr-1, ptr+1) || ((invisible_word_before_next &= IsIgnore(&ptr))))) {
          ptr++;
        }

        next_word_start = ptr;

        // after finding the start of next word, probe shud be at the same place as ptr
        probe = ptr;

        if (isupper(*next_word_start)) {
          next_word_caps = true;
          next_word_all_caps = true;
        } else {
          next_word_caps = false;
          next_word_all_caps = false;
        }
      } else {
        next_word_start = NULL;
      } 

      if (NULL == caps_entity_start) {
        if ('\0' != current_word_delimiter &&
            current_word_len > 1 &&
            current_word_caps &&
            !current_word_stop &&
            !current_word_dict &&
            '\0' != *next_word_start &&
            !invisible_word_before_next) { 
          if (' ' == current_word_delimiter &&
              ((current_word_end + 1) == next_word_start)) {
            caps_entity_start = current_word_start;
          }
        }
        caps_entity_end = NULL;
      } else {
        if (current_word_stop ||
            !current_word_caps ||
            current_word_dict ||
            current_word_len < 2) {
          if (caps_entity_start != prev_word_start) {
            caps_entity_end = prev_word_end;
          }
          else {
            caps_entity_start = NULL;
            caps_entity_end = NULL;
          }
        } else {
          if (' ' != current_word_delimiter ||
              '\0' == *next_word_start ||
              invisible_word_before_next ||
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

      if (NULL == stopwords_entity_start) {
        if ('\0' != current_word_delimiter &&
            !current_word_stop &&
            !current_word_dict &&
            '#' != *current_word_start &&
            '\0' != *next_word_start &&
            !invisible_word_before_next &&
            current_word_len > 1) {
          if (' ' == current_word_delimiter &&
              ((current_word_end + 1) == next_word_start)) {
            stopwords_entity_start = current_word_start;
          }
        }
        stopwords_entity_end = NULL;
      } else {
        if (current_word_stop || current_word_dict || '#' == *current_word_start || current_word_len < 2) {
          if (stopwords_entity_start != prev_word_start) {
            stopwords_entity_end = prev_word_end;
          }
          else {
            stopwords_entity_start = NULL;
            stopwords_entity_end = NULL;
          }
        } else {
          if (' ' != current_word_delimiter ||
              '\0' == *next_word_start ||
              invisible_word_before_next ||
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
      }

      // write entities
      if (NULL != stopwords_entity_start && NULL != stopwords_entity_end) {
        if (stopwords_entity_start < stopwords_entity_end) {
#ifdef DEBUG
          cout << endl << string(stopwords_entity_start, (stopwords_entity_end - stopwords_entity_start)) << " :entity by stopword";
#endif
          keywords_set.insert(string(stopwords_entity_start, (stopwords_entity_end - stopwords_entity_start)));
        } else {
          cout << "ERROR: stopwords entity markers are wrong\n";
        }
        stopwords_entity_start = NULL;
        stopwords_entity_end = NULL;
      }

      if (NULL != caps_entity_start && NULL != caps_entity_end) {
        if (caps_entity_start < caps_entity_end) {
#ifdef DEBUG
          cout << endl << string(caps_entity_start, (caps_entity_end - caps_entity_start)) << " :entity by caps";
#endif
          keywords_set.insert(string(caps_entity_start, (caps_entity_end - caps_entity_start)));
        } else {
          cout << "ERROR: caps entity markers are wrong\n";
        }
        caps_entity_start = NULL;
        caps_entity_end = NULL;
      }

#ifdef DEBUG
      cout << endl;
#endif

      // exit conditions
      if ('\0' == current_word_delimiter || '\0' == *next_word_start) {
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

      prev_word_delimiter = current_word_delimiter;

      current_word_start = next_word_start;
      current_word_caps = next_word_caps;
      current_word_all_caps = next_word_all_caps;
      current_word_has_mixed_case = false;

      invisible_word_before_next = false;
      next_word_start = NULL;
      next_word_caps = false;
      next_word_all_caps = false;

    } else {
      if (!strcmp(probe, "&#")) {
        while (' ' != *probe && '\0' != *probe)
          probe++;
        if ('\0' == *probe)
          break;
      }
      if (isupper(*probe)) {
        if (!current_word_all_caps) {
          //if ((probe-1) == ptr)
            //second_letter_has_caps = true;
          //else
            current_word_has_mixed_case = true;
        }
      } else {
        if (current_word_caps)
          current_word_has_mixed_case = false;
        current_word_all_caps = false;
      }
    }

    // a mere cog in a loop wheel, but a giant killer if commented
    probe++;
  }
#ifdef DEBUG
  cout << endl << "\norginal query: " << std::string(str) << endl;
#endif
  return 0;
}

} // namespace inagist_trends
