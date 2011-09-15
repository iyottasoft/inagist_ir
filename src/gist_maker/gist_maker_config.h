#ifndef _INAGIST_CLASSIFIERS_GISTMAKER_CONFIG_H_
#define _INAGIST_CLASSIFIERS_GISTMAKER_CONFIG_H_

#include <string>
#include <set>

namespace inagist_trends {

typedef struct _gm_config_struct {
  std::string stopwords_file;
  std::string dictionary_file;
  std::string language_dictionary_file;
  std::string language_prior_freqs_file;
  std::string classifier_dictionary_file;
  std::string location_dictionary_file;
  std::string intent_words_file;
  std::string sentiment_words_file;
  std::string unsafe_dictionary_file;
  std::string stemmer_dictionary_file;
} Config;

class GistMakerConfig {
 public:
  // functions
  GistMakerConfig();
  ~GistMakerConfig();
  static int Read(const char* config_file_name, Config& config);
  static int Clear(Config& config);
};

} // namespace inagist_trends

#endif // _INAGIST_CLASSIFIERS_GISTMAKER_CONFIG_H_
