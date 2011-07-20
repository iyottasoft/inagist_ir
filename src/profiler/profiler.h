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
           const char* self_text_classification_config);

  int SetDebugLevel(unsigned int debug_level);

  int Profile(const std::string& twitter_handle,
              std::set<std::string>& locations,
              std::set<std::string>& self_languages,
              std::set<std::string>& self_text_classes,
              std::set<std::string>& self_sub_classes,
              std::map<std::string, std::string>& self_text_class_contributors_map,
              std::set<std::string>& others_languages,
              std::set<std::string>& others_text_classes,
              std::set<std::string>& others_sub_classes,
              std::map<std::string, std::string>& others_text_class_contributors_map,
              std::string& intent,
              std::string& sentiment,
              std::set<std::string>& recommendations,
              const std::string& profile_name);

  int Profile(const char* twitter_handle, unsigned int twitter_handle_len,
              unsigned char* locations_buffer, const unsigned int locations_buffer_len,
              unsigned int& locations_len, unsigned int& locations_count,
              char* self_languages_buffer, const unsigned int self_languages_buffer_len,
              unsigned int& self_languages_len, unsigned int& self_languages_count,
              char* self_text_classes_buffer, const unsigned int self_text_classes_buffer_len,
              unsigned int& self_text_classes_len, unsigned int& self_text_classes_count,
              char* self_sub_classes_buffer, const unsigned int self_sub_classes_buffer_len,
              unsigned int& self_sub_classes_len, unsigned int& self_sub_classes_count,
              unsigned char* self_text_class_contributors_buffer,
              const unsigned int self_text_class_contributors_buffer_len,
              unsigned int& self_text_class_contributors_len,
              unsigned int& self_text_class_contributors_count,
              char* others_languages_buffer, const unsigned int others_languages_buffer_len,
              unsigned int& others_languages_len, unsigned int& others_languages_count,
              char* others_text_classes_buffer, const unsigned int others_text_classes_buffer_len,
              unsigned int& others_text_classes_len, unsigned int& others_text_classes_count,
              char* others_sub_classes_buffer, const unsigned int others_sub_classes_buffer_len,
              unsigned int& others_sub_classes_len, unsigned int& others_sub_classes_count,
              unsigned char* others_text_class_contributors_buffer,
              const unsigned int others_text_class_contributors_buffer_len,
              unsigned int& others_text_class_contributors_len,
              unsigned int& others_text_class_contributors_count,
              char* intent_buffer, const unsigned int intent_buffer_len,
              char* sentiment_buffer, const unsigned int sentiment_buffer_len,
              unsigned char* recommendations_buffer, const unsigned int recommendations_buffer_len,
              unsigned int& recommendations_len, unsigned int& recommendations_count,
              const char* profile_name);

  int ProfileFromFile(const char* docs_file_name, unsigned int docs_file_name_len,
        unsigned char* locations_buffer, const unsigned int locations_buffer_len,
        unsigned int& locations_len, unsigned int& locations_count,
        char* self_languages_buffer, const unsigned int self_languages_buffer_len,
        unsigned int& self_languages_len, unsigned int& self_languages_count,
        char* self_text_classes_buffer, const unsigned int self_text_classes_buffer_len,
        unsigned int& self_text_classes_len, unsigned int& self_text_classes_count,
        char* self_sub_classes_buffer, const unsigned int self_sub_classes_buffer_len,
        unsigned int& self_sub_classes_len, unsigned int& self_sub_classes_count,
        unsigned char* self_text_class_contributors_buffer,
        const unsigned int self_text_class_contributors_buffer_len,
        unsigned int& self_text_class_contributors_len,
        unsigned int& self_text_class_contributors_count,
        char* intent_buffer, const unsigned int intent_buffer_len,
        char* sentiment_buffer, const unsigned int sentiment_buffer_len,
        unsigned char* recommendations_buffer, const unsigned int recommendations_buffer_len,
        unsigned int& recommendations_len, unsigned int& recommendations_count,
        const char* profile_name);

int GetGist(std::set<std::string>& tweets,
            char* lang_class_buffer, const unsigned int lang_class_buffer_len,
            unsigned int& lang_class_len, unsigned int& lang_class_count,
            char* text_class_buffer, const unsigned int text_class_buffer_len,
            unsigned int& text_class_len, unsigned int& text_class_count,
            char* top_text_classes_buffer, const unsigned int sub_classes_buffer_len,
            unsigned int& top_text_classes_len, unsigned int& top_text_classes_count,
            unsigned char* text_class_contributors_buffer,
            const unsigned int text_class_contributors_buffer_len,
            unsigned int& text_class_contributors_len,
            unsigned int& text_class_contributors_count,
            char* intent_buffer, const unsigned int intent_buffer_len,
            char* sentiment_buffer, const unsigned int sentiment_buffer_len,
            inagist_classifiers::Corpus& corpus, unsigned int& corpus_size);

 private:
  inagist::GistMaker m_gist_maker;
  unsigned int m_debug_level;

  DISALLOW_COPY_AND_ASSIGN(Profiler); 
};

} // inagist_dashboard

#endif // _INAGIST_DASHBOARD_PROFILER_H_
