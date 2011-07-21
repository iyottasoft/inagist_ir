#include "intent_finder.h"
#include <iostream>

namespace inagist_classifiers {

IntentFinder::IntentFinder() {
}

IntentFinder::~IntentFinder() {
}

int IntentFinder::Init(const char* keytuples_extracter_config_file) {

  // initialize keytuples extracter
  bool load_classifier_dictionary=false;
  if (m_keytuples_extracter.Init(keytuples_extracter_config_file,
                                 load_classifier_dictionary=false) < 0) {
    std::cerr << "ERROR: could not initialize KeyTuplesExtracter\n";
    return -1;
  }

  return 0;
}

int IntentFinder::FindIntent(unsigned char* text_buffer, const unsigned int& text_buffer_len,
                             char* intent_buffer, const unsigned int& intent_buffer_len) {

  if (!text_buffer || !intent_buffer) {
    std::cerr << "ERROR: invalid buffers\n";
    return -1;
  }

  intent_buffer[0] = '\0';
  char safe_status_buffer[10];
  unsigned int safe_status_buffer_len = 10;
  char script_buffer[4];
  unsigned int script_buffer_len = 4;

  if (m_keytuples_extracter.GetKeyTuples(text_buffer, text_buffer_len,
                                         safe_status_buffer, safe_status_buffer_len,
                                         script_buffer, script_buffer_len,
                                         intent_buffer, intent_buffer_len) < 0) {
    std::cerr << "ERROR: could not get sentiment\n";
    return -1;
  }

  return 0;
}

int IntentFinder::Clear() {
  return 0;
}

}
