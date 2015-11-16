#ifndef _INAGIST_TRENDS_NAMED_ENTITIES_MANAGER_H_
#define _INAGIST_TRENDS_NAMED_ENTITIES_MANAGER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <map>
#include <string>
#include <iostream>
#include <fstream>
#include <set>
#include <vector>
#include <list>

namespace inagist_trends {

typedef std::map<std::string, std::size_t> _entity_freq_map;
typedef _entity_freq_map::iterator _entity_freq_map_iter;

typedef std::map<std::string, double> _entity_idf_map;
typedef _entity_idf_map::iterator _entity_idf_map_iter;

typedef struct {
  std::string word;
  std::vector<std::list<double> > queues;
  std::vector<double> queue_totals;
} WordIdfBucket;

class KeywordsManager {
 public:
  KeywordsManager();
  ~KeywordsManager();
  int Init(const char* named_entities_repo_directory);
  int DeInit();
  int Clear();

  int PopulateFreqMap(std::set<std::string>& named_entities_set);
  void PrintFreqMap();
  int CalculateIDF(unsigned int num_docs);
  int CalulateIDF(unsigned int num_docs, std::ofstream& ofs);
  int CalculateIDF(unsigned int num_docs, const char* file_name);
  void PrintEntityIDFs();
  void PrintIdfBuckets();

 private:
  inagist_trends::_entity_freq_map m_entity_freq_map;
  inagist_trends::_entity_idf_map m_entity_idf_map;
};

}

#endif // _INAGIST_TRENDS_NAMED_ENTITIES_MANAGER_H_
