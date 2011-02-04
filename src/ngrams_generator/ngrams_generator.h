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

namespace inagist_classifiers {

#define MAX_STEM_TEXT_LEN 1024

class NgramsGenerator {

 public:
  NgramsGenerator();
  ~NgramsGenerator();
  int GetNgrams(const unsigned char* text,
                const unsigned int& text_len,
                std::map<std::string, int>& features_map);
  int GetNgramsFromTweet(const std::string& tweet,
                         std::map<std::string, int>& features_map);
  int GetAllNgrams(const std::string& tweet,
                   std::map<std::string, int>& features_map);
  int GetAllNgrams(unsigned char* start,
                   unsigned char* stop,
                   std::map<std::string, int>& features_map);
  int GetNgramsFromFile(const std::string& input_file_name,
                        std::map<std::string, int>& features_map);
  int GetNgramsFromWords(std::set<std::string>& words_set,
                         std::map<std::string, int>& features_map);
  int GetNgramsFromWord(const unsigned char* word_str,
                        unsigned int word_len,
                        std::map<std::string, int>& features_map);

 private:
  unsigned char m_buffer[MAX_STEM_TEXT_LEN];
  int PositionPointer(unsigned char*& prev, unsigned char*& current, unsigned char*& next);
  DISALLOW_COPY_AND_ASSIGN(NgramsGenerator); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_NGRAMS_GENERATOR_H_
