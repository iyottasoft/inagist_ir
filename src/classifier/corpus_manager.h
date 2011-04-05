#ifndef _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_
#define _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_

#include <string>
#include <map>

namespace inagist_classifiers {

#define MAX_CORPUS_NUMBER 20

typedef std::map<std::string, int> Corpus;
typedef std::map<std::string, int>::iterator CorpusIter;
typedef std::map<std::string, Corpus> CorpusMap;
typedef std::map<std::string, Corpus>::iterator CorpusMapIter;

class CorpusManager {
 public:
  CorpusManager();
  ~CorpusManager();
  int InitRead(const std::string& corpus_file_name);
  static int LoadCorpus(const std::string corpus_file_name, Corpus& corpus);
  int LoadCorpusMap(const std::string config_file_name);
  int LoadCorpusMap(std::map<std::string, std::string> corpus_class_file_map);
  int LookUp(const std::string& entry);
  static int PrintCorpus(Corpus& corpus);
  static int WriteCorpusToFile(Corpus& corpus, const std::string& file_name);
  static int UpdateCorpusFile(Corpus& corpus, const std::string& file_name);
  int Clear();

  CorpusMap m_corpus_map;
  Corpus m_classes_freq_map;
 private:
  Corpus m_corpus;

};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_
