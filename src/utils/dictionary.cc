#include "dictionary.h"
#include <iostream>
#include <fstream>

namespace inagist_utils {

Dictionary::Dictionary() {
}

Dictionary::~Dictionary() {
  m_dictionary.clear();
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
int Dictionary::Load(const char* file) {
  if (NULL == file) {
    std::cerr << "ERROR: invalid dictionary file\n";
    return -1;
  }

  std::ifstream ifs(file);
  if (!ifs) {
    std::cerr << "ERROR: error opening dictionary file " << file << std::endl;
    return -1;
  }

  std::string str;
  while (getline(ifs, str)) {
    //m_dictionary.insert(str.c_str());
    m_dictionary.insert(str);
  }

  ifs.close();

  return 0;
}

int Dictionary::Find(const char *word) {
  if (!word) {
    std::cout << "ERROR: fatal error - memory corruption\n";
    return -1;
  }
  if (m_dictionary.find(std::string(word)) != m_dictionary.end())
    return 1;
  return 0;
}

int Dictionary::Print() {
  //string_hash_set::const_iterator iter;
  std::set<std::string>::iterator iter;
  for (iter = m_dictionary.begin(); iter != m_dictionary.end(); iter++)
    std::cout << *iter << std::endl;
  return 0;
}

} // namespace inagist_utils
