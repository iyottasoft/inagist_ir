#ifndef _INAGIST_DASHBOARD_PROFILER_H_
#define _INAGIST_DASHBOARD_PROFILER_H_
/*
#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif
*/

#include <string>
#include <set>
#include "gist_maker.h"

#define MAX_CLASS_LIST_LEN 320

#ifndef MAX_BUFFER_LEN
#define MAX_BUFFER_LEN 1024
#endif // MAX_BUFFER_LEN

namespace inagist_dashboard {

typedef struct _profile {

  unsigned int id;
  std::string profanity_status;
  std::map<std::string, double> lang_classes_map;
  std::map<std::string, double> scripts_map;
  std::map<std::string, double> locations_map;
  std::map<std::string, double> text_classes_map;
  std::map<std::string, double> features_map;
  std::map<std::string, double>::iterator map_iter;
  std::multimap<std::string, std::string> features_to_classes_map;
  std::multimap<std::string, std::string>::iterator mmap_iter;
  std::pair<std::multimap<std::string, std::string>::iterator,
            std::multimap<std::string, std::string>::iterator> range;
  void Clear() {
    if (!lang_classes_map.empty()) {
      lang_classes_map.clear();
    }
    if (!text_classes_map.empty()) {
      text_classes_map.clear();
    }
    if (!features_map.empty()) {
      features_map.clear();
    }
    if (!features_to_classes_map.empty()) {
      features_to_classes_map.clear();
    }
  }
/*
  friend bool operator<(Profile const& p1, Profile const& p2) {
    return (p1.id < p2.id);
  }
*/
} Profile;

typedef struct _gist {
 public:
  char profanity_status_buffer[10];
  unsigned int profanity_status_buffer_len;

  char scripts_buffer[10];
  unsigned int scripts_buffer_len;

  unsigned char named_entities_buffer[MAX_BUFFER_LEN];
  unsigned int named_entities_buffer_len;
  unsigned int named_entities_len;
  unsigned int named_entities_count;

  unsigned char keywords_buffer[MAX_BUFFER_LEN];
  unsigned int keywords_buffer_len;
  unsigned int keywords_len;
  unsigned int keywords_count;

  unsigned char keyphrases_buffer[MAX_BUFFER_LEN];
  unsigned int keyphrases_buffer_len;
  unsigned int keyphrases_len;
  unsigned int keyphrases_count;

#ifdef LANG_ENABLED
  char* lang_classes_buffer[MAX_CLASS_LIST_LEN];
  unsigned int lang_classes_buffer_len;
  unsigned int lang_classes_len;
  unsigned int lang_classes_count;
#endif // LANG_ENABLED

#ifdef TEXT_CLASSIFICATION_ENABLED
  char* text_classes_buffer[MAX_CLASS_LIST_LEN];
  unsigned int text_classes_buffer_len;
  unsigned int text_classes_len;
  unsigned int text_classes_count;
#endif // TEXT_CLASSIFICATION_ENABLED

#ifdef LOCATION_ENABLED
  char* location_classes_buffer[MAX_CLASS_LIST_LEN];
  unsigned int location_classes_buffer_len;
  unsigned int location_classes_len;
  unsigned int location_classes_count;
#endif // LOCATION_ENABLED

  int intent_valence;
  int sentiment_valence;

  _gist() {
    profanity_status_buffer_len = 10;
    scripts_buffer_len = 10;
    named_entities_buffer_len = MAX_BUFFER_LEN;
    keywords_buffer_len = MAX_BUFFER_LEN;
    keyphrases_buffer_len = MAX_BUFFER_LEN;
#ifdef LANG_ENABLED
    lang_classes_buffer_len = MAX_CLASS_LIST_LEN;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    text_classes_buffer_len = MAX_CLASS_LIST_LEN;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
    location_classes_buffer_len = MAX_CLASS_LIST_LEN;
#endif // LOCATION_ENABLED
  }

  void Clear() {
    profanity_status_buffer[0] = '\0';
    scripts_buffer[0] = '\0';
    named_entities_buffer[0] = '\0';
    named_entities_len = 0;
    named_entities_count = 0;
    keywords_buffer[0] = '\0';
    keywords_len = 0;
    keywords_count = 0;
    keyphrases_buffer[0] = '\0';
    keyphrases_len = 0;
    keyphrases_count = 0;
#ifdef LANG_ENABLED
    lang_classes_buffer[0] = '\0';
    lang_classes_len = 0;
    lang_classes_count = 0;
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
    text_classes_buffer[0] = '\0';
    text_classes_len = 0;
    text_classes_count = 0;
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef  LOCATION_ENABLED
    location_classes_buffer[0] = '\0';
    location_classes_len = 0;
    location_classes_count = 0;
#endif // LOCATION_ENABLED
    intent_valence = 0;
    sentiment_valence = 0;
  }
} Gist;

class Profiler {

 public:

  Profiler();
  ~Profiler();

  int Init(const char* gist_maker_config);

  int SetDebugLevel(unsigned int debug_level);

  int GetProfile(const std::string& twitter_handle,
              std::set<std::string>& locations
#ifdef LANG_ENABLED
              , std::set<std::string>& self_languages
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              , std::set<std::string>& self_text_classes
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
              , std::set<std::string>& self_location_classes
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              // std::map<std::string, std::string>& self_text_class_contributors_map,
              , std::set<std::string>& self_text_class_contributors
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
              , std::set<std::string>& others_languages
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              , std::set<std::string>& others_text_classes
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
              , std::set<std::string>& others_location_classes
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              // std::map<std::string, std::string>& others_text_class_contributors_map,
              , std::set<std::string>& others_text_class_contributors
#endif // TEXT_CLASSIFICATION_ENABLED
              , std::set<std::string>& features
              , const std::string& profile_name);

  int GetProfile(const char* twitter_handle, unsigned int twitter_handle_len,
              unsigned char* locations_buffer, const unsigned int locations_buffer_len,
              unsigned int& locations_len, unsigned int& locations_count
#ifdef LANG_ENABLED
              , char* self_lang_classes_buffer, const unsigned int self_lang_classes_buffer_len,
              unsigned int& self_lang_classes_len, unsigned int& self_lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              , char* self_text_classes_buffer, const unsigned int self_text_classes_buffer_len,
              unsigned int& self_text_classes_len, unsigned int& self_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
              , char* self_location_classes_buffer, const unsigned int self_location_classes_buffer_len,
              unsigned int& self_location_classes_len, unsigned int& self_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              , unsigned char* self_text_class_contributors_buffer,
              const unsigned int self_text_class_contributors_buffer_len,
              unsigned int& self_text_class_contributors_len,
              unsigned int& self_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LANG_ENABLED
              , char* others_lang_classes_buffer, const unsigned int others_lang_classes_buffer_len,
              unsigned int& others_lang_classes_len, unsigned int& others_lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              , char* others_text_classes_buffer, const unsigned int others_text_classes_buffer_len,
              unsigned int& others_text_classes_len, unsigned int& others_text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
              , char* others_location_classes_buffer, const unsigned int others_location_classes_buffer_len,
              unsigned int& others_location_classes_len, unsigned int& others_location_classes_count
#endif // LOCATION_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
              , unsigned char* others_text_class_contributors_buffer,
              const unsigned int others_text_class_contributors_buffer_len,
              unsigned int& others_text_class_contributors_len,
              unsigned int& others_text_class_contributors_count
#endif // TEXT_CLASSIFICATION_ENABLED
              , unsigned char* features_buffer, const unsigned int features_buffer_len,
              unsigned int& features_len, unsigned int& features_count
              , const char* profile_name);

  int CreateProfile(Profile* &profile); // this merely creates an instance of Profile class object

  int AddTextsToProfile(std::set<std::string>& texts, Profile* &profile);

  int AddTextToProfile(const unsigned char* text_buffer,
                       const unsigned int text_buffer_len,
                       const unsigned int text_len,
                       Profile* &profile,
                       const double& dynamic_score=0);

  int GetProfile(Profile* &profile
#ifdef LANG_ENABLED
                 , char* lang_classes_buffer,
                 const unsigned int& lang_classes_buffer_len,
                 unsigned int& lang_classes_len,
                 unsigned int& lang_classes_count
#endif // LANG_ENABLED
#ifdef TEXT_CLASSIFICATION_ENABLED
                 , char* text_classes_buffer,
                 const unsigned int& text_classes_buffer_len,
                 unsigned int& text_classes_len,
                 unsigned int& text_classes_count
#endif // TEXT_CLASSIFICATION_ENABLED
#ifdef LOCATION_ENABLED
                 , char* location_classes_buffer,
                 const unsigned int& location_classes_buffer_len,
                 unsigned int& location_classes_len,
                 unsigned int& location_classes_count
#endif // LOCATION_ENABLED
                 , unsigned char* features_buffer,
                 const unsigned int& features_buffer_len,
                 unsigned int& features_len,
                 unsigned int& features_count);

  int DeleteProfile(Profile* &profile);

  int MultiMapToPipeList(std::multimap<double, std::string>& map,
                  unsigned char* buffer, unsigned int buffer_len,
                  unsigned int& list_len, unsigned int& list_count);

 private:
  inagist::GistMaker m_gist_maker;
  Gist m_gist;
  unsigned int m_debug_level;

  // DISALLOW_COPY_AND_ASSIGN(Profiler); 
};

} // inagist_dashboard

#endif // _INAGIST_DASHBOARD_PROFILER_H_
