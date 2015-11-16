#include "named_entities_manager.h"
#include <fstream>
#include <cmath>

namespace inagist_trends {

KeywordsManager::KeywordsManager() {
};

KeywordsManager::~KeywordsManager() {
  if (DeInit() < 0)
    std::cout << "ERROR: couldn't deinitialize KeywordsManager\n";
};

int KeywordsManager::Clear() {
  m_entity_freq_map.clear();
  m_entity_idf_map.clear();
  return 0;
}

int KeywordsManager::Init(const char *named_entities_repo_directory) {
  Clear();
  // load the idf buckets
  return 0;
}

int KeywordsManager::DeInit() {
  Clear();
  // dump the idf buckets back to files
  return 0;
}

int KeywordsManager::PopulateFreqMap(std::set<std::string> &named_entities_set) {
  std::set<std::string>::iterator set_iter;

  for (set_iter = named_entities_set.begin(); set_iter != named_entities_set.end(); set_iter++) {
    if (m_entity_freq_map.find(*set_iter) != m_entity_freq_map.end())
      ++m_entity_freq_map[*set_iter];
    else
      m_entity_freq_map[*set_iter] = 1;
  }

  return 0;
}

void KeywordsManager::PrintFreqMap() {
  _entity_freq_map_iter map_iter;
  for (map_iter = m_entity_freq_map.begin(); map_iter != m_entity_freq_map.end(); map_iter++) {
    std::cout << map_iter->second << "\t" << map_iter->first << std::endl;
  }
}

int KeywordsManager::CalculateIDF(unsigned int num_docs) {
  _entity_freq_map_iter map_iter;

  for (map_iter = m_entity_freq_map.begin(); map_iter != m_entity_freq_map.end(); map_iter++)
    m_entity_idf_map[map_iter->first] = log(num_docs/map_iter->second);

  return 0;
}

int KeywordsManager::CalulateIDF(unsigned int num_docs, std::ofstream& ofs) {
  _entity_freq_map_iter map_iter;
  for (map_iter = m_entity_freq_map.begin(); map_iter != m_entity_freq_map.end(); map_iter++) {
    m_entity_idf_map[map_iter->first] = log(num_docs/map_iter->second);
    ofs << map_iter->first << " = " << m_entity_idf_map[map_iter->first] << "," << map_iter->second << std::endl;
    ofs.flush();
  }
 return 0;
}

// populates output file with word, freq and idf
int KeywordsManager::CalculateIDF(unsigned int num_docs, const char* file_name) {
  if (NULL == file_name)
    return -1;

  int ret_value = 0;
  std::ofstream ofs(file_name, std::ios::app);
  if (!ofs) {
    std::cout << "Error: could not open file " << file_name << std::endl;
    return -1;
  }

  _entity_freq_map_iter map_iter;
  for (map_iter = m_entity_freq_map.begin(); map_iter != m_entity_freq_map.end(); map_iter++) {
    m_entity_idf_map[map_iter->first] = log(num_docs/map_iter->second);
    ofs << map_iter->first << " = " << m_entity_idf_map[map_iter->first] << "," << map_iter->second << std::endl;
  }

  ofs.close();

  return ret_value;
}

void KeywordsManager::PrintEntityIDFs() {
  _entity_idf_map_iter map_iter;
  for (map_iter = m_entity_idf_map.begin(); map_iter != m_entity_idf_map.end(); map_iter++) {
    std::cout << map_iter->second << "\t" << map_iter->first << std::endl;
  }
}

void KeywordsManager::PrintIdfBuckets() {
}

}
