#ifndef _INAGIST_CLASSIFIERS_LANGUAGE_DETECTOR_H_
#define _INAGIST_CLASSIFIERS_LANGUAGE_DETECTOR_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <map>
#include <string>

namespace inagist_classifiers {

class LanguageDetector {
 public:
  LanguageDetector();
  ~LanguageDetector();
  int Init(const std::string& training_data_dir);
  int Clear();
  int DetectLanguage(const std::string& text, const unsigned int& text_len, std::string& lang);
  int GetNgrams(const char* text, const unsigned int& text_len, std::map<std::string, int>& features_map);
  int PositionPointer(char*& prev, char*& current, char*& next);

 private:
  std::map<std::string, int> m_global_features_map;

  DISALLOW_COPY_AND_ASSIGN(LanguageDetector); 
};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_LANGUAGE_DETECTOR_H_
