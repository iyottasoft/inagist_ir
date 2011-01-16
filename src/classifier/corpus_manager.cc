#include "corpus_manager.h"
#include <iostream>
#include <fstream>
#include <cstdlib>

namespace inagist_classifiers {

CorpusManager::CorpusManager() {
}

CorpusManager::~CorpusManager() {
  Clear();
}

int CorpusManager::WriteCorpusToFile(Corpus& corpus, const std::string& file_name) {

  std::ofstream ofs(file_name.c_str());
  if (!ofs) {
    std::cout << "ERROR: could not open file " << file_name << std::endl;
    return -1;
  }

  CorpusIter corpus_iter;
  for (corpus_iter = corpus.begin(); corpus_iter != corpus.end(); corpus_iter++) {
    ofs << (*corpus_iter).first << "=" << (*corpus_iter).second << std::endl;
  }
  ofs.close();

  return 0;
}

int CorpusManager::InitRead(const std::string& corpus_file_name) {

  if (LoadCorpus(corpus_file_name, m_corpus) < 0) {
    std::cout << "ERROR: could not load corpus file " << corpus_file_name << std::endl;
    return -1;
  } else {
    return m_corpus.size();
  }
}

int CorpusManager::LookUp(const std::string& entry) {

  CorpusIter corpus_iter;
  if ((corpus_iter = m_corpus.find(entry)) != m_corpus.end())
    return (*corpus_iter).second;
  else
    return 0;
}

int CorpusManager::Clear() {

  m_corpus.clear();
  CorpusMapIter corpus_map_iter;
  for (corpus_map_iter = m_corpus_map.begin(); corpus_map_iter != m_corpus_map.end(); corpus_map_iter++) {
    (corpus_map_iter->second).clear();
  }
  m_corpus_map.clear();

  return 0;
}

int CorpusManager::LoadCorpus(const std::string corpus_file_name, Corpus& corpus) {

  std::ifstream ifs(corpus_file_name.c_str());
  if (!ifs) {
    std::cout << "ERROR: could not open corpus file " << corpus_file_name << std::endl;
    return -1;
  }

  std::string entry; 
  std::string line;
  std::string freq_str;
  std::string::size_type loc = 0;
  int freq = 0;
  int num_docs = 0;
  while (getline(ifs, line)) {
    num_docs++;
    if ((loc = line.find("=", 0)) != std::string::npos) {
      entry = std::string(line, 0, loc);
      freq_str = std::string(line, loc+1, line.length() - loc); 
      freq = atoi(freq_str.c_str());
      corpus[entry] = freq; 
    } else {
      std::cout << "ERROR: malformed corpus entry on line number " << num_docs \
                << " in " << corpus_file_name << std::endl;
      ifs.close();
      return -1;
    }
  }
  ifs.close();

  return 0;
}

int CorpusManager::LoadCorpusMap(std::map<std::string, std::string> corpus_class_files_map) {

  std::map<std::string, std::string>::iterator cfile_iter;
  Corpus corpus;
  for (cfile_iter = corpus_class_files_map.begin(); cfile_iter != corpus_class_files_map.end(); cfile_iter++) {
    if (LoadCorpus((*cfile_iter).second, corpus) < 0) {
      std::cout << "ERROR: could not load corpus from file " << (*cfile_iter).second << std::endl;
    } else {
      m_corpus_map.insert(std::pair<std::string, Corpus> ((*cfile_iter).first, corpus));
    }
  }

  return 0;
}

int CorpusManager::LoadCorpusMap(const std::string config_file_name) {

  std::ifstream ifs(config_file_name.c_str());
  if (!ifs) {
    std::cout << "ERROR: could not open corpus file " << config_file_name << std::endl;
    return -1;
  }

  std::string line;
  std::string class_name; 
  std::string file_name;
  std::string::size_type loc = 0;
  int num_docs = 0;
  Corpus corpus;
  while (getline(ifs, line)) {
    if ((loc = line.find("=", 0)) != std::string::npos) {
      class_name = std::string(line, 0, loc);
      file_name = std::string(line, loc+1, line.length() - loc); 
      if (LoadCorpus(file_name, corpus) < 0) {
        std::cout << "ERROR: could not load corpus from file " << file_name << std::endl;
      } else {
        m_corpus_map.insert(std::pair<std::string, Corpus> (class_name, corpus));
      }
    } else {
      std::cout << "ERROR: malformed config entry on line number " << num_docs+1 \
                << " in " << file_name << std::endl;
      ifs.close();
      return -1;
    }
    num_docs++;
  }
  ifs.close();

  return 0;
}

}
