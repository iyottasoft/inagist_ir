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
    case '_':
      return false;
    default:
      break;
  }

  return true;
}

bool KeywordsExtract::IsIgnore(char *&ptr) {
  if (!ptr || '\0' == *ptr)
    return false;
  if ('@' == *ptr || !strncmp(ptr, "&#", 2) || !strncmp(ptr, "http://", 7) || !strncmp(ptr, "www.", 4)) {
#ifdef DEBUG
    printf("%s is ignore word\n", ptr);
#endif
    while (' ' != *(ptr+1) && '\0' != *(ptr+1)) {
      ptr++;
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
  char next_word_delimiter;

  //unsigned in_len = 0;
  //unsigned out_len = 0;
  unsigned int current_word_len = 0;
  unsigned int next_word_len = 0;
#ifdef DEBUG
  int score = 0;
#endif
  int num_mixed_words = 0;
  int num_caps_words = 0;
  int num_words = 0;
  int num_stop_words = 0;
  int num_dict_words = 0;
  int num_numeric_words = 0;
  int num_normal_words = 0; // not caps or stop or dict or numeric

  char *current_word_start = NULL;
  char *current_word_end = NULL;
  char *prev_word_start = NULL;
  char *prev_word_end = NULL;
  char *next_word_start = NULL;
  char *next_word_end = NULL;

  char *caps_entity_start = NULL;
  char *caps_entity_end = NULL;
  char *stopwords_entity_start = NULL;
  char *stopwords_entity_end = NULL;
  char *sentence_start = NULL;

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
  char *pch = NULL;
  char ch;

  std::set<std::string> keyphrases_set;

  // the whole thing starts here
  ptr = str;

#ifdef DEBUG
  cout << endl << "orginal query: " << std::string(str) << endl << endl;
#endif

  // go to the first word, ignoring handles and punctuations
  char *prev = NULL;
  while (ptr && '\0' != *ptr && (' ' == *ptr || (ispunct(*ptr) && IsPunct(ptr, prev, ptr+1)) || IsIgnore(ptr))) {
    prev = ptr;
    ptr++;
  }

  if (!ptr || '\0' == *ptr) {
#ifdef DEBUG
    cout << "either the input is empty or has ignore words only" << endl;
    return 0;
#endif
  }

  current_word_start = ptr;
  sentence_start = ptr;
#ifdef DEBUG
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
  ptr++;
  while (ptr && ' ' != *ptr && '\0' != *ptr && !(is_punct = IsPunct(ptr, ptr-1, ptr+1))) {
    if (!strcmp(ptr, "&#")) {
      while (' ' != *ptr && '\0' != *ptr)
        ptr++;
      if ('\0' == *ptr)
        break;
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
    ptr++;
  }

  if (!ptr || '\0' == *ptr) {
#ifdef DEBUG
    cout << "either the input has only one word or the other words are ignore words" << endl;
    return 0;
#endif
  }
  current_word_end = ptr;
  current_word_delimiter = *ptr;
  current_word_len = current_word_end - current_word_start;
  *ptr = '\0';
  current_word_precedes_punct = is_punct;
  num_words++;

  // stop words
  if ((m_stopwords_dictionary.find(current_word_start) != m_stopwords_dictionary.end())) {
    current_word_stop = true;
    num_stop_words++;
#ifdef DEBUG
    //cout << "current word: " << current_word_start << " :stopword" << endl;
#endif
  } else {
    current_word_stop = false;
  }

  // dictionary words
  if ((m_dictionary.find(current_word_start) != m_dictionary.end())) {
    current_word_dict = true;
    num_dict_words++;
#ifdef DEBUG
    //cout << "current word: " << current_word_start << " :dictionary word" << endl;
#endif
  } else {
    current_word_dict = false;
  }

  // go to the next word, ignoring punctuation and ignore words.
  // however passing over ignorewords must be recorded
  ptr++;
  is_ignore_word = false;
  is_punct = false;
  while ('\0' != *ptr &&
         (' ' == *ptr || (ispunct(*ptr) && (is_punct = IsPunct(ptr, ptr-1, ptr+1))) || (is_ignore_word = IsIgnore(ptr)))) {
    current_word_precedes_ignore_word |= is_ignore_word;
    current_word_precedes_punct |= is_punct;
    ptr++;
  }

  if (ptr && '\0' != *ptr) {
    next_word_start = ptr;
    num_words++;
    if (current_word_precedes_ignore_word || current_word_precedes_punct) {
      sentence_start = next_word_start;
#ifdef DEBUG
      cout << "sentence start: " << sentence_start << endl;
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
  } else {
    next_word_start = NULL;
  }
  probe = ptr + 1;

  while (ptr && probe && ptr != '\0') {
    // this loop works between second letter to end punctuation for each word
    is_punct = false;
    if (' ' == *probe || '\0' == *probe || (ispunct(*probe) && (is_punct = IsPunct(probe, probe-1, probe+1)))) {

#ifdef DEBUG
      if (NULL != stopwords_entity_end)
        cout << "ERROR: stopswords entity end is not null. did you not write it before?" << endl;
      if (NULL != caps_entity_end)
        cout << "ERROR: caps entity end is not null. did you not write it before?" << endl;
#endif

      // word boundary
#ifdef DEBUG
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

#ifdef DEBUG
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
#endif

#ifdef DEBUG
      if ((current_word_len < 2) && !isdigit(*current_word_start))
        score-=5;

      if ('#' == *current_word_start) {
        score++;
      }
#endif

#ifdef DEBUG
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
#endif

      // stop words
      if (next_word_start) {
        if ((m_stopwords_dictionary.find(next_word_start) != m_stopwords_dictionary.end())) {
          next_word_stop = true;
          num_stop_words++;
#ifdef DEBUG
          score--;
          cout << "next word: " << next_word_start << " :stopword" << endl;
#endif
        } else {
          next_word_stop = false;
        }

        // dictionary words
        if ((m_dictionary.find(next_word_start) != m_dictionary.end())) {
          next_word_dict = true;
          num_dict_words++;
#ifdef DEBUG
          score--;
          cout << "next word: " << next_word_start << " :dictionary word" << endl;
#endif
        } else {
          next_word_dict = false;
        }
      }

      if (prev_word_end)
        *prev_word_end = prev_word_delimiter;

      if (!current_word_stop && !current_word_dict && !current_word_caps &&
          !current_word_starts_num && !current_word_has_mixed_case &&
          (current_word_len > 1) && '#' != *current_word_start) {
#ifdef DEBUG
        cout << current_word_start << ": normal word" << endl;
#endif
        num_normal_words++;
      }
      if (current_word_has_mixed_case)
        num_mixed_words++;

      if (NULL == stopwords_entity_start) {
        if (current_word_stop) {
          // X of Y case
          if (strcmp(current_word_start, "of") == 0 && NULL != next_word_start && NULL != prev_word_start) {
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
        } else if (NULL != prev_word_start && current_word_starts_num &&
                   (' ' == current_word_delimiter || '\0' == current_word_delimiter)) {
          // handling numbers that occur with cap entities
          if (prev_word_caps && !prev_word_stop && !prev_word_dict &&
              !prev_word_precedes_ignore_word &&
              !prev_word_precedes_punct) {

            if (caps_entity_start && caps_entity_start < prev_word_start)
              stopwords_entity_start = caps_entity_start;
            else
              stopwords_entity_start = prev_word_start;

            if (!next_word_start || !next_word_caps || next_word_stop || next_word_dict ||
                current_word_precedes_ignore_word || current_word_precedes_punct) {
                stopwords_entity_end = current_word_end;
            }
          }
        } else if (!caps_entity_start && prev_word_start && next_word_start) {
          // Experimental location detection - TODO (balaji) use regex if this experiment succeeds
          if (current_word_caps &&
              strcmp(prev_word_start, "in") == 0 && ',' == current_word_delimiter &&
              next_word_caps && !current_word_dict &&
              !next_word_stop && !next_word_dict && !current_word_stop
             ) {
            stopwords_entity_start = current_word_start;
            stopwords_entity_end = current_word_end;
          } else if (next_word_caps &&
                     ((strcmp(prev_word_start, "place") == 0 && strcmp(current_word_start, "called") == 0 &&
                       !next_word_stop && (',' == next_word_delimiter || '.' == next_word_delimiter || '\0' == next_word_delimiter)) ||
                      (strcmp(prev_word_start, "town") == 0 &&
                       (strcmp(current_word_start, "of") == 0 || strcmp(current_word_start, "called") == 0) &&
                       !next_word_stop && (',' == next_word_delimiter || '.' == next_word_delimiter || '\0' == next_word_delimiter)))
                    ) {
            stopwords_entity_start = next_word_start;
            stopwords_entity_end = next_word_end;
          }
        } else if (caps_entity_start &&
                   next_word_start && next_word_caps && !next_word_stop && !next_word_dict) {
          // Experimental sports event detection - TODO (balaji) use regex if this experiment succeeds
          if ((strcmp(current_word_start, "vs") == 0) ||
              (strcmp(current_word_start, "v") == 0) ||
              (strcmp(current_word_start, "beat ") == 0) ||
              (strcmp(current_word_start, "defeat ") == 0) ||
              (strcmp(current_word_start, "beats ") == 0) ||
              (strcmp(current_word_start, "defeats ") == 0)) {
            stopwords_entity_start = caps_entity_start;
          }
        }
      } else {
#ifdef DEBUG
        cout << "stopword entity candidate: " << stopwords_entity_start << endl;
#endif
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

      /*
      if (NULL == stopwords_keyphrase_start) {
        if ('\0' != current_word_delimiter &&
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
      */

      if (NULL == caps_entity_start) {
        caps_entity_end = NULL;
        if ((current_word_len > 1 && current_word_caps && !current_word_stop && !current_word_dict) && 
            (!prev_word_start || prev_word_precedes_ignore_word || prev_word_precedes_punct || !prev_word_caps)) {

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

          } /*else if (prev_word_stop &&
              ('\0' == current_word_delimiter ||
              NULL == next_word_start ||
              current_word_precedes_ignore_word ||
              current_word_precedes_punct)) { 

              caps_entity_start = current_word_start;
              caps_entity_end = current_word_end;
          }*/
        }
      } else {
#ifdef DEBUG
        cout << "caps entity candidate: " << caps_entity_start << endl;
#endif
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

      // write entities
      if (NULL != stopwords_entity_start && NULL != stopwords_entity_end) {
        if (stopwords_entity_start < stopwords_entity_end) {
#ifdef DEBUG
          cout << endl << string(stopwords_entity_start, (stopwords_entity_end - stopwords_entity_start)) << " :entity by stopword";
#endif
          if (strncmp(stopwords_entity_end-2, "\'s", 2) == 0) {
            ch = *(stopwords_entity_end-2);
            *(stopwords_entity_end-2) = '\0';
            keywords_set.insert(string(stopwords_entity_start, ((stopwords_entity_end-2) - stopwords_entity_start)));
            *(stopwords_entity_end-2) = ch;
          }
          else if ((pch = strstr(stopwords_entity_start, "\'s")) && (pch < stopwords_entity_end)) {
            ch = *pch;
            *pch = '\0';
            // but don't insert the X in X's if X is a single word!
            if (strstr(stopwords_entity_start, " "))
              keywords_set.insert(string(stopwords_entity_start, (pch - stopwords_entity_start)));
            *pch = ch;
            keywords_set.insert(string(stopwords_entity_start, (stopwords_entity_end - stopwords_entity_start)));
          } else {
           keywords_set.insert(string(stopwords_entity_start, (stopwords_entity_end - stopwords_entity_start)));
          }
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
          if (strncmp(caps_entity_end-2, "\'s", 2) == 0) {
            ch = *(caps_entity_end-2);
            *(caps_entity_end-2) = '\0';
            keywords_set.insert(string(caps_entity_start, ((caps_entity_end-2) - caps_entity_start)));
            *(caps_entity_end-2) = ch;
          }
          else if ((pch = strstr(caps_entity_start, "\'s")) && (pch < caps_entity_end)) {
            ch = *pch;
            *pch = '\0';
            // but don't insert the X in X's if X is a single word!
            if (strstr(caps_entity_start, " "))
              keywords_set.insert(string(caps_entity_start, (pch - caps_entity_start)));
            *pch = ch;
            keywords_set.insert(string(caps_entity_start, (caps_entity_end - caps_entity_start)));
          }
          else { 
            keywords_set.insert(string(caps_entity_start, (caps_entity_end - caps_entity_start)));
          }
        } else {
          cout << "ERROR: caps entity markers are wrong\n";
        }
        caps_entity_start = NULL;
        caps_entity_end = NULL;
      }

#ifdef DEBUG
      //cout << endl;
#endif

      // exit conditions
      if ('\0' == current_word_delimiter || !next_word_start || '\0' == *next_word_start) {
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

      // BE CAREFUL ABOUT WHAT IS NEXT WORD OR CURRENT WORD NOW

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
        while ('\0' != *ptr && (' ' == *ptr || (ispunct(*ptr) && (is_punct = IsPunct(ptr, ptr-1, ptr+1))) || (is_ignore_word = IsIgnore(ptr)))) {
          current_word_precedes_ignore_word |= is_ignore_word;
          current_word_precedes_punct |= is_punct; 
          ptr++;
        }

        if (ptr && '\0' != *ptr) {
          next_word_start = ptr;
          num_words++;
          if (current_word_precedes_ignore_word) {
            sentence_start = next_word_start;
#ifdef DEBUG
            cout << "sentence start: " << sentence_start << endl;
#endif
          }

          if (current_word_precedes_punct) {
            sentence_start = next_word_start;
#ifdef DEBUG
            cout << "sentence start: " << sentence_start << endl;
#endif
            if (':' == current_word_delimiter) {
              if (num_normal_words == 0) {
                keywords_set.clear();
                caps_entity_start = NULL;
                stopwords_entity_start = NULL;
              }
            } else {
              for (pch = current_word_end + 1; (pch != next_word_start); pch++) {
                if (':' == *pch) {
                  if (num_normal_words == 0) {
                    keywords_set.clear();
                    caps_entity_start = NULL;
                    stopwords_entity_start = NULL;
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
      if (!strcmp(probe, "&#")) {
        while (' ' != *probe && '\0' != *probe)
          probe++;
        if ('\0' == *probe)
          break;
        current_word_precedes_ignore_word = true;
      }
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
    probe++;
  }

  if (num_mixed_words > 2) {
#ifdef DEBUG
    cout << "non-english tweet. ignoring." << endl;
#endif
    keywords_set.clear();
  }

#ifdef DEBUG
  cout << endl << "\norginal query: " << std::string(str) << endl;
  cout << "num words: " << num_words << endl;
  cout << "num caps words: " << num_caps_words << endl;
  cout << "num stop words: " << num_stop_words << endl;
  cout << "num dict words: " << num_dict_words << endl;
  cout << "num numeric words: " << num_numeric_words << endl;
  cout << "num normal words: " << num_normal_words << endl;
#endif
  std::set<std::string>::iterator iter;
  if ((num_normal_words == 0) && (num_dict_words != 0 || num_words > 5))
    keywords_set.clear();

  //for (iter = keyphrases_set.begin(); iter != keyphrases_set.end(); iter++) {
    //keywords_set.insert(*iter);
  //}

  return 0;
}

} // namespace inagist_trends
