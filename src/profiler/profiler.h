#ifndef _INAGIST_DASHBOARD_PROFILER_H_
#define _INAGIST_DASHBOARD_PROFILER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include "corpus_manager.h"
#include "text_classifier.h"

namespace inagist_dashboard {

class Profiler {
 public:
  Profiler();
  ~Profiler();
  int Init(const std::string &keytuples_extracter_config,
           const std::string &text_classifier_config,
           const std::string &language_detection_config);
  int Profile(const std::string& twitter_handle,
              std::string& dominant_class,
              const std::string& profile_name);
  int GenerateProfile(const std::string& twitter_handle,
                      inagist_classifiers::Corpus& corpus);
  int ClassifyProfile(inagist_classifiers::Corpus& corpus,
                      std::string& dominant_class);
  int WriteProfile(inagist_classifiers::Corpus& corpus,
                   const std::string& profile_name);

 private:
  inagist_classifiers::TextClassifier m_text_classifier;

  DISALLOW_COPY_AND_ASSIGN(Profiler); 
};

} // inagist_dashboard

#endif // _INAGIST_DASHBOARD_PROFILER_H_
