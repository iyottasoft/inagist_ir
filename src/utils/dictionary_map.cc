#include "dictionary_map.h"
#include <iostream>
#include <fstream>
#include <cstring>

namespace inagist_utils {

DictionaryMap::DictionaryMap() {
}

DictionaryMap::~DictionaryMap() {
  if (!m_dictionary_map.empty()) {
    m_dictionary_map.clear();
  }
}

int DictionaryMap::Clear() {
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
int DictionaryMap::Load(const char* file) {
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
  std::string::size_type loc;
  while (getline(ifs, line)) {
    loc = line.find("=", 0);
    if (loc == std::string::npos) {
      std::cout << "ERROR: invalid dictinary map file entry:\n" << line << std::endl;
      break;
    }
    key.assign(line.c_str(), loc);
    value.assign(line.c_str(), loc+1, (line.length()-loc-1));
    m_dictionary_map[key] = value;
  }

  ifs.close();

  return 0;
}

int DictionaryMap::Find(const unsigned char *key, std::string& value) {
  if (!key) {
    std::cout << "ERROR: invalid input for dictionary map lookup\n";
    return -1;
  }
  if ((m_dict_map_iter = m_dictionary_map.find(std::string((char *) key))) != m_dictionary_map.end()) {
    value.assign(m_dict_map_iter->second);
    return 1;
  }
  return 0;
}

int DictionaryMap::FindPart(const unsigned char* key, std::string& value) {

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

int DictionaryMap::Print() {
  std::map<std::string, std::string>::iterator iter;
  for (iter = m_dictionary_map.begin(); iter != m_dictionary_map.end(); iter++)
    std::cout << iter->first << " = " << iter->second << std::endl;

  return 0;
}

} // namespace inagist_utils
