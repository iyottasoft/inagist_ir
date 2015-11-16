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
  static int LoadCorpus(const std::string corpus_file_name, Corpus& corpus, double default_value=0);
  static int LookUp(Corpus& corpus, const std::string& entry);
  static int WriteCorpusToFile(Corpus& corpus, const std::string& file_name);
  static int UpdateCorpusFile(Corpus& corpus, const std::string& file_name);
  static int PrintCorpus(Corpus& corpus);
  static int ClearCorpus(Corpus& corpus);

  static int LoadCorpusMap(const std::string config_file_name, CorpusMap& corpus_map);
  static int LoadCorpusMap(CorpusMapMeta& corpus_map_meta_data, CorpusMap& corpus_map);
  static int WriteCorpusMap(CorpusMap& corpus_map, CorpusMapMeta& corpus_map_meta_data);
  static int UpdateCorpusMap(CorpusMap& corpus_map, std::string& input_class_name, Corpus& input_corpus);
  static int PrintCorpusMap(CorpusMap& corpus_map);
  static int ClearCorpusMap(CorpusMap& corpus_map);

 private:

};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_
