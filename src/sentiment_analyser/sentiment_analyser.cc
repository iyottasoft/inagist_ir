#include "sentiment_analyser.h"
#include <iostream>

namespace inagist_classifiers {

SentimentAnalyser::SentimentAnalyser() {
}

SentimentAnalyser::~SentimentAnalyser() {
}

int SentimentAnalyser::Init(const char* keytuples_extracter_config_file) {

  // initialize keytuples extracter
  bool load_classifier_dictionary=false;
  if (m_keytuples_extracter.Init(keytuples_extracter_config_file,
                                 load_classifier_dictionary=false) < 0) {
    std::cerr << "ERROR: could not initialize KeyTuplesExtracter\n";
    return -1;
  }

  return 0;
}

int SentimentAnalyser::AnalyseSentiment(unsigned char* text_buffer, const unsigned int& text_buffer_len,
                                        char* sentiment_buffer, const unsigned int& sentiment_buffer_len) {

  if (!text_buffer || !sentiment_buffer) {
    std::cerr << "ERROR: invalid buffers\n";
    return -1;
  }

  sentiment_buffer[0] = '\0';
  char safe_status_buffer[10];
  unsigned int safe_status_buffer_len = 10;
  char script_buffer[4];
  unsigned int script_buffer_len = 4;

  if (m_keytuples_extracter.GetKeyTuples(text_buffer, text_buffer_len,
                                         safe_status_buffer, safe_status_buffer_len,
                                         script_buffer, script_buffer_len,
                                         sentiment_buffer, sentiment_buffer_len) < 0) {
    std::cerr << "ERROR: could not get sentiment\n";
    return -1;
  }

  return 0;
}

int SentimentAnalyser::Clear() {
  return 0;
}

}
