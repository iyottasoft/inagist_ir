#include "keywords_manager.h"
#include <fstream>
#include <cmath>

namespace inagist_trends {

KeywordsManager::KeywordsManager() {
};

KeywordsManager::~KeywordsManager() {
  if (DeInit() < 0)
    std::cout << "ERROR: couldn't deinitialize KeywordsManager\n";
  m_entity_freq_map.clear();
  m_entity_idf_map.clear();
};

int KeywordsManager::Init(const char *keywords_repo_directory) {
  // load the idf buckets
  return 0;
}

int KeywordsManager::DeInit() {
  // dump the idf buckets back to files
  return 0;
}

int KeywordsManager::PopulateFreqMap(std::set<std::string> &keywords_set) {
  std::set<std::string>::iterator set_iter;

  for (set_iter = keywords_set.begin(); set_iter != keywords_set.end(); set_iter++)
    ++m_entity_freq_map[*set_iter];

  return 0;
}

void KeywordsManager::PrintFreqMap() {
  _entity_freq_map_iter map_iter;
  std::ofstream ofs("./data/keywords_set.txt", std::ofstream::out);
  for (map_iter = m_entity_freq_map.begin(); map_iter != m_entity_freq_map.end(); map_iter++) {
    std::cout << map_iter->second << "\t" << map_iter->first << std::endl;
    ofs << map_iter->second << " = " << map_iter->first << std::endl;
  }
  ofs.close();
}

int KeywordsManager::CalculateIDF(unsigned int num_docs) {
  _entity_freq_map_iter map_iter;

  for (map_iter = m_entity_freq_map.begin(); map_iter != m_entity_freq_map.end(); map_iter++)
    m_entity_idf_map[map_iter->first] = log(num_docs/map_iter->second);

  return 0;
}

// populates output file with word, freq and idf
int KeywordsManager::CalculateIDF(unsigned int num_docs, const char *file_name) {
  if (NULL == file_name)
    return -1;

  std::ofstream ofs(file_name, std::ofstream::out);

  _entity_freq_map_iter map_iter;
  for (map_iter = m_entity_freq_map.begin(); map_iter != m_entity_freq_map.end(); map_iter++) {
    m_entity_idf_map[map_iter->first] = log(num_docs/map_iter->second);
    ofs << map_iter->first << " = " << m_entity_idf_map[map_iter->first] << "," << map_iter->second << std::endl;
  }
  ofs.close();

  return 0;
}

void KeywordsManager::PrintEntityIDFs() {
  _entity_idf_map_iter map_iter;
  std::ofstream ofs("./data/keyword_idf_pairs.txt", std::ofstream::out);
  for (map_iter = m_entity_idf_map.begin(); map_iter != m_entity_idf_map.end(); map_iter++) {
    std::cout << map_iter->second << "\t" << map_iter->first << std::endl;
    ofs << map_iter->second << " = " << map_iter->first << ", " << m_entity_freq_map[map_iter->first] << std::endl;
  }
  ofs.close();
}

void KeywordsManager::PrintIdfBuckets() {
}

}
