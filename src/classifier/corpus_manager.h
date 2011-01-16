#ifndef _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_
#define _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

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
  int LoadCorpus(const std::string corpus_file_name, Corpus& corpus);
  int LoadCorpusMap(const std::string config_file_name);
  int LoadCorpusMap(std::map<std::string, std::string> corpus_class_file_map);
  int LookUp(const std::string& entry);
  int WriteCorpusToFile(Corpus& corpus,
                     const std::string& file_name);
  int Clear();

  CorpusMap m_corpus_map;
 private:
  Corpus m_corpus;

  DISALLOW_COPY_AND_ASSIGN(CorpusManager); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_CORPUS_MANAGER_H_
