#ifndef _INAGIST_DASHBOARD_PROFILER_H_
#define _INAGIST_DASHBOARD_PROFILER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <set>
#include "gist_maker.h"

namespace inagist_dashboard {

class Profiler {

 public:

  Profiler();
  ~Profiler();

  int Init(const char* keytuples_extracter_config,
           const char* language_detection_config,
           const char* text_classification_config,
           const char* sentiment_analyser_config=NULL);

  int Profile(const std::string& twitter_handle,
              std::set<std::string>& languages,
              std::set<std::string>& text_classes,
              std::set<std::string>& sub_classes,
              std::string& sentiment,
              const std::string& profile_name);

  int Profile(const char* twitter_handle, unsigned int twitter_handle_len,
              char* languages, const unsigned int languages_buffer_len,
              unsigned int& languages_len, unsigned int& languages_count,
              char* text_classes_buffer, const unsigned int text_classes_buffer_len,
              unsigned int& text_classes_len, unsigned int& text_classes_count,
              char* sub_classes_buffer, const unsigned int sub_classes_buffer_len,
              unsigned int& sub_classes_len, unsigned int& sub_classes_count,
              char* sentiment_buffer, const unsigned int sentiment_buffer_len,
              const char* profile_name);

  int ProfileFromFile(const char* docs_file_name, unsigned int docs_file_name_len,
            char* languages_buffer, const unsigned int languages_buffer_len,
            unsigned int& languages_len, unsigned int& languages_count,
            char* text_classes_buffer, const unsigned int text_classes_buffer_len,
            unsigned int& text_classes_len, unsigned int& text_classes_count,
            char* sub_classes_buffer, const unsigned int sub_classes_buffer_len,
            unsigned int& sub_classes_len, unsigned int& sub_classes_count,
            char* sentiment_buffer, const unsigned int sentiment_buffer_len,
            const char* profile_name);

  int GetGist(std::set<std::string>& tweets,
            char* languages_buffer, const unsigned int languages_buffer_len,
            unsigned int& languages_len, unsigned int& languages_count,
            char* text_classes_buffer, const unsigned int text_classes_buffer_len,
            unsigned int& text_classes_len, unsigned int& text_classes_count,
            char* sub_classes_buffer, const unsigned int sub_classes_buffer_len,
            unsigned int& sub_classes_len, unsigned int& sub_classes_count,
            char* sentiment_buffer, const unsigned int sentiment_buffer_len,
            inagist_classifiers::Corpus& corpus, unsigned int& corpus_size);

 private:
  inagist::GistMaker m_gist_maker;

  DISALLOW_COPY_AND_ASSIGN(Profiler); 
};

} // inagist_dashboard

#endif // _INAGIST_DASHBOARD_PROFILER_H_
