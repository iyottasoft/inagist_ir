#ifndef _INAGIST_CLASSIFIERS_INTENT_ANALYSER_H_
#define _INAGIST_CLASSIFIERS_INTENT_ANALYSER_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <map>
#include <string>
#include <set>
#include <fstream>
#include "keytuples_extracter.h"

namespace inagist_classifiers {

class IntentFinder {
 public:
  IntentFinder();
  ~IntentFinder();
  int Init(const char* keytuples_config_file);
  int FindIntent(unsigned char* text_buffer, const unsigned int& text_buffer_len,
                 const unsigned int& text_len,
                 char* intent_buffer, const unsigned int& intent_buffer_len);
  int Clear();

 private:
  inagist_trends::KeyTuplesExtracter m_keytuples_extracter;
  DISALLOW_COPY_AND_ASSIGN(IntentFinder); 

};

} // inagist_classifiers

#endif // _INAGIST_CLASSIFIERS_INTENT_ANALYSER_H_
