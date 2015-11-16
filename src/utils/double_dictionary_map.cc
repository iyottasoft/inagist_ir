#include "double_dictionary_map.h"
#include <iostream>
#include <fstream>
#include <cstring>

//#define UTILS_DEBUG 0

namespace inagist_utils {

DoubleDictionaryMap::DoubleDictionaryMap() {
}

DoubleDictionaryMap::~DoubleDictionaryMap() {
  if (!m_dictionary_map.empty()) {
    m_dictionary_map.clear();
  }
}

int DoubleDictionaryMap::Clear() {
  if (!m_dictionary_map.empty()) {
    m_dictionary_map.clear();
  }
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
int DoubleDictionaryMap::Load(const char* file) {
  if (NULL == file) {
    std::cerr << "ERROR: invalid dictionary map file\n";
    return -1;
  }

  std::ifstream ifs(file);
  if (!ifs) {
    std::cerr << "ERROR: error opening dictionary map file " << file << std::endl;
    return -1;
  }

  std::string line;
  std::string key;
  std::string value;
  double value_double = 0;
  std::string::size_type loc;
  while (getline(ifs, line)) {
    loc = line.find("=", 0);
    if (loc == std::string::npos) {
      std::cout << "ERROR: invalid dictinary map file entry:\n" << line << std::endl;
      break;
    }
    key.assign(line.c_str(), loc);
    value.assign(line.c_str(), loc+1, (line.length()-loc-1));
    sscanf(value.c_str(), "%lf", &value_double);
    m_dictionary_map[key] = value_double;
  }

  ifs.close();

  return 0;
}

int DoubleDictionaryMap::Find(const unsigned char *key, double& value) {
  if (!key) {
    std::cout << "ERROR: invalid input for dictionary map lookup\n";
    return -1;
  }
  if ((m_dict_map_iter = m_dictionary_map.find(std::string((char *) key))) != m_dictionary_map.end()) {
    value = m_dict_map_iter->second;
    return 1;
  }
  return 0;
}

int DoubleDictionaryMap::Find(std::string& key, double& value) {
  if (key.empty()) {
#ifdef UTILS_DEBUG
    std::cerr << "ERROR: invalid input for dictionary map lookup\n";
#endif // UTILS_DEBUG
    return -1;
  }
  if ((m_dict_map_iter = m_dictionary_map.find(key)) != m_dictionary_map.end()) {
    value = m_dict_map_iter->second;
    return 1;
  }
  return 0;
}

int DoubleDictionaryMap::FindPart(const unsigned char* key, double& value) {

  int ret_value = 0;
  if ((ret_value = Find(key, value)) != 0)
    return ret_value;

  char* pch = (char*) key;
  char temp;
  while (*pch != '\0' && (pch = strstr(pch+1, " ")) != NULL) {
    temp = *pch;
    *pch = '\0';
    if ((ret_value = Find(key, value)) != 0) {
      *pch = temp;
      return ret_value;
    }
    *pch = temp;
  }
  return 0;
}

int DoubleDictionaryMap::Print() {
  std::map<std::string, double>::iterator iter;
  for (iter = m_dictionary_map.begin(); iter != m_dictionary_map.end(); iter++)
    std::cout << iter->first << " = " << iter->second << std::endl;

  return 0;
}

} // namespace inagist_utils
