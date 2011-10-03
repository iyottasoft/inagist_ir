#ifndef _INAGIST_UTILS_STRING_UTILS_H_
#define _INAGIST_UTILS_STRING_UTILS_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <map>
#include <set>

namespace inagist_utils {

//class StringUtils {
 //public:
  //StringUtils();
  //~StringUtils();
  bool IsPunct(char*& ptr, char* prev=NULL, char* next=NULL, int* punct_intent=NULL, int* punct_senti=NULL);
  bool IsIgnore(char *&ptr);
  int Tokenize(const std::string& text, std::set<std::string>& tokens);
  int ToLower(const char* input, char* output);
  int PipeListToMap(unsigned char* buffer, std::map<std::string, double>& map);
  int MapToPipeList(std::map<std::string, double>& map,
                    unsigned char* buffer, unsigned int buffer_len,
                    unsigned int& list_len, unsigned int& list_count);
  int StringMapToPipeList(std::map<std::string, std::string> map,
                    unsigned char* buffer, const unsigned int& buffer_len,
                    unsigned int& list_len, unsigned int& list_count);
  int PipeListToStringMap(unsigned char* buffer, const unsigned int buffer_len,
                        unsigned int& list_len, unsigned int& list_count,
                        std::map<std::string, std::string>& map);
  int PipeListToSet(unsigned char* buffer, std::set<std::string>& set);
  int SetToPipeList(std::set<std::string>& set,
                    unsigned char* buffer, unsigned int buffer_len,
                    unsigned int& list_len, unsigned int& list_count);

 //private:
  //DISALLOW_COPY_AND_ASSIGN(StringUtils);
//};

} // namespace inagist_utils

#endif // _INAGIST_UTILS_STRING_UTILS_H_
