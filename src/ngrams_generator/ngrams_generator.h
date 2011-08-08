#ifndef _INAGIST_CLASSIFIERS_NGRAMS_GENERATOR_H_
#define _INAGIST_CLASSIFIERS_NGRAMS_GENERATOR_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <map>
#include <set>
#include "corpus_manager.h"

namespace inagist_classifiers {

#define MAX_STEM_TEXT_LEN 1024

class NgramsGenerator {

 public:
  NgramsGenerator();
  ~NgramsGenerator();
  int GetNgrams(const unsigned char* text,
                const unsigned int& text_len,
                Corpus& corpus);
  int GetNgramsFromTweet(const std::string& tweet,
                         Corpus& corpus,
                         bool ignore_case=false);
  int GetAllNgrams(const std::string& tweet,
                   Corpus& corpus);
  int GetAllNgrams(unsigned char* start,
                   unsigned char* stop,
                   Corpus& corpus);
  int GetNgramsFromFile(const std::string& input_file_name,
                        Corpus& corpus);
  int GetNgramsFromWords(std::set<std::string>& words_set,
                         Corpus& corpus,
                         bool ignore_case=false);
  int GetNgramsFromWords(const unsigned char* text_word_list,
                         const unsigned int& list_len,
                         const unsigned int& word_count, 
                         Corpus& corpus,
                         bool ignore_case=false);
  int GetNgramsFromWord(const unsigned char* word_str,
                        unsigned int word_len,
                        Corpus& corpus);
  int SetDebugLevel(unsigned int debug_level);

 private:
  unsigned char m_buffer[MAX_STEM_TEXT_LEN];
  int PositionPointer(unsigned char*& prev, unsigned char*& current, unsigned char*& next);

  unsigned int m_debug_level;

  DISALLOW_COPY_AND_ASSIGN(NgramsGenerator); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_NGRAMS_GENERATOR_H_
