#include "corpus_manager.h"
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <sys/stat.h>

#ifdef DEBUG
#if DEBUG>0
#define CORPUS_MANAGER_DEBUG DEBUG
#endif
#endif
//#define CORPUS_MANAGER_DEBUG 5

namespace inagist_classifiers {

CorpusManager::CorpusManager() {
}

CorpusManager::~CorpusManager() {
  Clear();
}

int CorpusManager::PrintCorpus(Corpus& corpus) {

  CorpusIter corpus_iter;
  for (corpus_iter = corpus.begin(); corpus_iter != corpus.end(); corpus_iter++) {
    std::cout << (*corpus_iter).first << "=" << (*corpus_iter).second << std::endl;
  }

  return 0;
}

int CorpusManager::PrintCorpusMap(CorpusMap& corpus_map) {
  CorpusMapIter corpus_map_iter;
  for (corpus_map_iter = corpus_map.begin(); corpus_map_iter != corpus_map.end(); corpus_map_iter++) {
    std::cout << corpus_map_iter->first << " corpus of size " << corpus_map_iter->second.size() << std::endl;
  }
  return 0;
}

int CorpusManager::UpdateCorpusFile(Corpus& corpus, const std::string& file_name) {

  struct stat stat_struct;
  if (stat(file_name.c_str(), &stat_struct) != 0) {
    std::cout << "WARNING: output file " << file_name << " not found. writing a new one\n";
    return WriteCorpusToFile(corpus, file_name);
  }

  Corpus temp_corpus;
  if (LoadCorpus(file_name, temp_corpus) < 0) {
    std::cerr << "ERROR: could not load corpus from file: " << file_name << std::endl;
    return -1;
  }

  CorpusIter corpus_iter;
  for (corpus_iter = corpus.begin(); corpus_iter != corpus.end(); corpus_iter++) {
    if (temp_corpus.find((*corpus_iter).first) != temp_corpus.end()) {
      temp_corpus[(*corpus_iter).first] += (*corpus_iter).second;
    } else {
      temp_corpus[(*corpus_iter).first] = (*corpus_iter).second;
    }
  }

  std::ofstream ofs(file_name.c_str());
  if (!ofs) {
    std::cout << "ERROR: could not open file " << file_name << std::endl;
    return -1;
  }

  for (corpus_iter = temp_corpus.begin(); corpus_iter != temp_corpus.end(); corpus_iter++) {
    ofs << (*corpus_iter).first << "=" << (*corpus_iter).second << std::endl;
  }
  ofs.close();

  return 0;
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
    std::cout << "ERROR: could not load corpus file for read" << corpus_file_name << std::endl;
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
  if (!m_corpus_map.empty()) {
    for (corpus_map_iter = m_corpus_map.begin(); corpus_map_iter != m_corpus_map.end(); corpus_map_iter++) {
      if (!corpus_map_iter->second.empty()) {
        (corpus_map_iter->second).clear();
      }
    }
    m_corpus_map.clear();
  }

  if (!m_classes_freq_map.empty()) {
    m_classes_freq_map.clear();
  }

  return 0;
}

int CorpusManager::LoadCorpus(const std::string corpus_file_name, Corpus& corpus) {

  if (corpus_file_name.length() < 1) {
    std::cerr << "ERROR: invalid file name for corpus\n";
    return -1;
  }

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
      std::cout << "ERROR: malformed entry: \"" << line << "\"" << std::endl;
      ifs.close();
      return -1;
    }
  }
  ifs.close();

  return 0;
}

// this is a map of class name and corresponding file names.
// for example for language detection an entry will look like
// <en, /path/to/file/name/file.txt>

int CorpusManager::LoadCorpusMap(std::map<std::string, std::string> corpus_class_files_map) {

  std::map<std::string, std::string>::iterator map_iter;
  Corpus corpus;
  std::string class_name;
  std::string corpus_file;
  for (map_iter = corpus_class_files_map.begin();
       map_iter != corpus_class_files_map.end();
       map_iter++) {
    class_name = map_iter->first;
    corpus_file = map_iter->second;
#ifdef CORPUS_MANAGER_DEBUG
    if (CORPUS_MANAGER_DEBUG > 4) {
      std::cout << class_name << " = " << corpus_file << std::endl;
    }
#endif
    if (class_name.empty() || corpus_file.empty()) {
      std::cerr << "ERROR: invalid corpus information.\n";
      return -1;
    }
    if (LoadCorpus(corpus_file, corpus) < 0) {
      std::cout << "ERROR: could not load corpus from file " << corpus_file << std::endl;
      return -1;
    } else {
      m_corpus_map.insert(std::pair<std::string, Corpus> (class_name, corpus));
#ifdef CORPUS_MANAGER_DEBUG
      if (CORPUS_MANAGER_DEBUG > 4) {
        std::cout << "corpus of size " << corpus.size() << " loaded for " << class_name << std::endl;
      }
#endif
    }
    corpus.clear();
  }

  return 0;
}

int CorpusManager::LoadCorpusMap(const std::string config_file_name) {

#ifdef CORPUS_MANAGER_DEBUG
  if (CORPUS_MANAGER_DEBUG > 3) {
    std::cout << "INFO: loading corpus map using config file:" << config_file_name << std::endl;
  }
#endif

  if (config_file_name.length() < 1) {
    std::cerr << "ERROR: invalid file name for corpus map config\n";
    return -1;
  }

  std::ifstream ifs(config_file_name.c_str());
  if (!ifs) {
    std::cout << "ERROR: could not open corpus file " << config_file_name << std::endl;
    return -1;
  }

  std::string line;
  std::string class_name; 
  std::string corpus_file_name;
  std::string::size_type loc = 0;
  int num_docs = 0;
  Corpus corpus;
  while (getline(ifs, line)) {
    if ((loc = line.find("=", 0)) != std::string::npos) {
      class_name = std::string(line, 0, loc);
      corpus_file_name = std::string(line, loc+1, line.length() - loc); 
      if (LoadCorpus(corpus_file_name, corpus) < 0) {
        std::cout << "ERROR: could not load corpus from file " << corpus_file_name << std::endl;
        return -1;
      } else {
        m_corpus_map.insert(std::pair<std::string, Corpus> (class_name, corpus));
      }
      corpus.clear();
    } else {
      std::cout << "ERROR: malformed config entry on line number " << num_docs+1 \
                << " in " << config_file_name << std::endl;
      std::cout << "ERROR: malformed entry: \"" << line << "\"" << std::endl;
      ifs.close();
      return -1;
    }
    num_docs++;
  }
  ifs.close();

  return 0;
}

}
