#include "profiler.h"
#include <cstring>
#include <cstdlib>
#include "corpus_manager.h"

#ifdef DEBUG
#if DEBUG>0
#define PROFILE_DEBUG DEBUG
#endif
#endif
//#define PROFILE_DEBUG 3

namespace inagist_dashboard {

Profiler::Profiler() {
}

Profiler::~Profiler() {
}

int Profiler::Init(const std::string &keytuples_extracter_config,
                   const std::string &text_classifier_config,
                   const std::string &language_detection_config) {

  int my_argc = 1;
  char* my_argv[1];
  char* temp_location = (char*) malloc(255);
  my_argv[0] = temp_location;
  memset(temp_location, '\0', 255);
  strcpy(temp_location, keytuples_extracter_config.c_str());
  if (m_text_classifier.InitDependencies(my_argc, (char**) my_argv) < 0) {
    std::cerr << "ERROR: could not init keytuples extracter" \
              << " for training. config_file: " << keytuples_extracter_config << std::endl;
    return -1;
  }
  free(temp_location);

  return 0;
}

int Profiler::GenerateProfile(const std::string& twitter_handle,
                              inagist_classifiers::Corpus& corpus) {

  unsigned int num_docs = 0;
  unsigned int corpus_size = 0;
  bool get_user_info = true;
  if ((m_text_classifier.GetTrainingData(twitter_handle,
                                     num_docs,
                                     corpus,
                                     corpus_size,
                                     get_user_info=true)) < 0) {
    std::cerr << "ERROR: could not get user profile info from text classifier\n";
    return -1;
  }
#ifdef PROFILE_DEBUG
  if (PROFILE_DEBUG > 1) {
    std::cout << "INFO: corpus of size " << corpus_size \
              << " generated for: " << twitter_handle << std::endl;
  }
#endif

  return corpus_size;
}

// calls the Classify function in text classifier.
// assumes that the classifier is already intialized
int Profiler::ClassifyProfile(inagist_classifiers::Corpus& corpus,
                              std::string& dominant_class) {

  int ret_value = 0;

  if ((ret_value = m_text_classifier.Classify(corpus, dominant_class)) < 0) {
    std::cerr << "ERROR: could not classify given corpus\n";
    return -1;
  }

  return ret_value;
}

// writes profile to given file name
// calls UpdateCorpusFile which reads the corpus from file and then updates it.
int Profiler::WriteProfile(inagist_classifiers::Corpus& corpus,
                           const std::string& profile_name) {

  if (!corpus.empty()) {
    if ((inagist_classifiers::CorpusManager::UpdateCorpusFile(corpus, profile_name)) < 0) {
      std::cerr << "ERROR: could not update corpus file: " << profile_name << std::endl;
      return -1;
    }
  }

  return 0;
}

// given a twitter handle this calls other function to produce a corpus, classify and
// then write the corpus to the output file.
// the text classifier must have already been initialized before this is called
int Profiler::Profile(const std::string& twitter_handle,
                      std::string& dominant_class,
                      const std::string& profile_name) {

  inagist_classifiers::Corpus corpus;
  // genrate profile
  if (GenerateProfile(twitter_handle, corpus) < 0) {
    std::cerr << "ERROR: could not generate profile\n";
    return -1;
  }

  // classify
  if (ClassifyProfile(corpus, dominant_class) < 0) {
    std::cerr << "ERROR: could not classify profile\n";
    return -1;
  }

  // write corpus to file
  if (corpus.size() > 0) {
    if (WriteProfile(corpus, profile_name) < 0) {
      std::cerr << "ERROR: could not write corpus to file: " << profile_name << std::endl;
      return -1;
    }
  }

  return 0;
}

} // namespace inagist_dashboard
