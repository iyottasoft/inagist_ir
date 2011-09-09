#include "string_to_map_dictionary.h"
#include <iostream>
#include <fstream>
#include <cstring>
#include <cstdlib>

namespace inagist_utils {

StringToMapDictionary::StringToMapDictionary() {
}

StringToMapDictionary::~StringToMapDictionary() {
  Clear();
}

int StringToMapDictionary::Clear() {
  if (!m_dictionary_map.empty()) {
    for (m_dict_map_iter = m_dictionary_map.begin();
         m_dict_map_iter != m_dictionary_map.end();
         m_dict_map_iter++) {
      if (!m_dict_map_iter->second.empty()) {
        m_dict_map_iter->second.clear();
      }
    }
    m_dictionary_map.clear();
  }
  return 0;
}

// this function expects the dictionary words in the following format:
//
// one word or phrase per line followed by = and then pipe separated list of class_name:double value
// end of the line is a pipe
// a single newline character at the end of the line
// lower case expected in most cases
// upper case or mixed case will be inserted as is
// no unnecessary blankspace anywhere. word phrases separated by single spaces
// no empty lines

// the caller MUST ensure that the above conditions are met
// this function checks nothing of the above. just inserts whatever is given
//
int StringToMapDictionary::Load(const char* file) {
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
  std::string::size_type prev_loc;
  std::string::size_type divider;
  std::string class_name;
  std::string freq_str;
  std::map<std::string, double> class_map;
  bool error_flag = false;
  while (getline(ifs, line)) {
    loc = line.find("=", 0);
    if (loc == std::string::npos) {
      std::cout << "ERROR: invalid dictinary map file entry:\n" << line << std::endl;
      break;
    }
    key.assign(line.c_str(), loc);
    value.assign(line.c_str(), loc+1, (line.length()-loc-1));
    prev_loc = loc;
    while ((loc = line.find("|", prev_loc + 1)) != std::string::npos) {
      divider = line.find(":", prev_loc);
      if (divider == std::string::npos) {
        std::cerr << "ERROR: invalid value for key: " << key << " in file entry:\n" << line << std::endl;
        error_flag = true;
        break;
      }
      class_name.assign(line.c_str(), prev_loc+1, divider-(prev_loc+1));
      freq_str.assign(line.c_str(), divider+1, loc-(divider+1));
      class_map[class_name] = atof(freq_str.c_str());
      prev_loc = loc;
    }
    if (error_flag) {
      class_map.clear();
      break;
    }
    m_dictionary_map.insert(std::pair<std::string, std::map<std::string, double> >(key, class_map));
    class_map.clear();
  }

  ifs.close();

  return 0;
}

int StringToMapDictionary::Find(const unsigned char *key, std::map<std::string, double>& value) {
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

int StringToMapDictionary::Print() {
  std::map<std::string, std::map<std::string, double> >::iterator map_iter1;
  std::map<std::string, double>::iterator map_iter2;
  for (map_iter1 = m_dictionary_map.begin(); map_iter1 != m_dictionary_map.end(); map_iter1++) {
    std::cout << map_iter1->first << " = " << std::endl;
    for (map_iter2 = map_iter1->second.begin(); map_iter2 != map_iter1->second.end(); map_iter2++) {
    }
  }

  return 0;
}

} // namespace inagist_utils
