#ifndef _INAGIST_UTILS_DOUBLE_DICTIONARY_MAP_H_
#define _INAGIST_UTILS_DOUBLE_DICTIONARY_MAP_H_

#include <string>
#include <map>

namespace inagist_utils {

class DoubleDictionaryMap {
 public:
  DoubleDictionaryMap();
  ~DoubleDictionaryMap();
  int Load(const char* dictionary_file_name);
  int Find(const unsigned char *key, double& value);
  int Find(std::string& key, double& value);
  int FindPart(const unsigned char* key, double& value);
  int Print();
  int Clear();

 private:
  std::map<std::string, double> m_dictionary_map;
  std::map<std::string, double>::iterator m_dict_map_iter;

};

} // namespace inagist_utils

#endif // _INAGIST_DOUBLE_DICTIONARY_MAP_H_
