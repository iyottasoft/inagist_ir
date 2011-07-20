#ifndef _INAGIST_UTILS_INT_DICTIONARY_MAP_H_
#define _INAGIST_UTILS_INT_DICTIONARY_MAP_H_

#include <string>
#include <map>

namespace inagist_utils {

class IntDictionaryMap {
 public:
  IntDictionaryMap();
  ~IntDictionaryMap();
  int Load(const char* dictionary_file_name);
  int Find(const unsigned char *key, int& value);
  int FindPart(const unsigned char* key, int& value);
  int Print();
  int Clear();

 private:
  std::map<std::string, int> m_dictionary_map;
  std::map<std::string, int>::iterator m_dict_map_iter;

};

} // namespace inagist_utils

#endif // _INAGIST_UTILS_DICTIONARY_MAP_H_
