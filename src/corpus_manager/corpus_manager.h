#ifndef _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_
#define _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_

#include <string>
#include <map>

namespace inagist_classifiers {

#define MAX_CORPUS_NUMBER 64

typedef std::map<std::string, double> Corpus;
typedef std::map<std::string, double>::iterator CorpusIter;
typedef std::map<std::string, Corpus> CorpusMap;
typedef std::map<std::string, Corpus>::iterator CorpusMapIter;
typedef std::map<std::string, std::string> CorpusMapMeta;
typedef std::map<std::string, std::string>::iterator CorpusMapMetaIter;

class CorpusManager {
 public:
  CorpusManager();
  ~CorpusManager();
  int InitRead(const std::string& corpus_file_name);
  static int LoadCorpus(const std::string corpus_file_name, Corpus& corpus);
  int LoadCorpusMap(const std::string config_file_name);
  int LoadCorpusMap(CorpusMapMeta& corpus_map_meta_data);
  static int LoadCorpusMap(CorpusMapMeta& corpus_map_meta_data, CorpusMap& corpus_map);
  static int UpdateCorpusMap(CorpusMap& corpus_map, std::string& input_class_name, Corpus& input_corpus);
  int LookUp(const std::string& entry);
  static int PrintCorpus(Corpus& corpus);
  static int PrintCorpusMap(CorpusMap& corpus_map);
  static int WriteCorpusToFile(Corpus& corpus, const std::string& file_name);
  static int UpdateCorpusFile(Corpus& corpus, const std::string& file_name);
  static int WriteCorpusMap(CorpusMap& corpus_map, CorpusMapMeta& corpus_map_meta_data);
  int Clear();

  CorpusMap m_corpus_map;
  Corpus m_classes_freq_map;
 private:
  Corpus m_corpus;

};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_
