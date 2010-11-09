#ifndef _INAGIST_CLASSIFIERS_SENTIMENT_ANALYSER_H_
#define _INAGIST_CLASSIFIERS_SENTIMENT_ANALYSER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <map>
#include <string>
#include <set>
#include <fstream>

namespace inagist_classifiers {

class SentimentAnalyser {
 public:
  SentimentAnalyser();
  ~SentimentAnalyser();
  int Init(const std::string& evidence_map_file);
  int AnalyseSentiment(std::set<std::string>& features);
  int Clear();

 private:
  std::map<std::string, int> m_evidence_map;
  std::ofstream m_evidence_map_file_stream;

  DISALLOW_COPY_AND_ASSIGN(SentimentAnalyser); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_SENTIMENT_ANALYSER_H_
