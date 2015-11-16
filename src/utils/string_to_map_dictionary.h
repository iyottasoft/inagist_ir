#ifndef _INAGIST_UTILS_CLASSIFIER_DICTIONARY_MAP_H_
#define _INAGIST_UTILS_CLASSIFIER_DICTIONARY_MAP_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

//#include <ext/hash_set>
#include <string>
#include <map>

namespace inagist_utils {

class StringToMapDictionary {
 public:
  StringToMapDictionary();
  ~StringToMapDictionary();
  int Load(const char* dictionary_file_name);
  int Find(const unsigned char *key, std::map<std::string, double>& value);
  int Print();
  int Clear();

 private:
  //string_hash_set m_dictionary_map;
  std::map<std::string, std::map<std::string, double> > m_dictionary_map;
  std::map<std::string, std::map<std::string, double> >::iterator m_dict_map_iter;

  DISALLOW_COPY_AND_ASSIGN(StringToMapDictionary); 
};

} // namespace inagist_utils

#endif // _INAGIST_UTILS_CLASSIFIER_DICTIONARY_MAP_H_
