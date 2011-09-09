/* language_classifier_cppi.h */

#ifndef _INAGIST_SEARCH_LANGUAGE_CLASSIFIER_ERL_INTERFACE_H_
#define _INAGIST_SEARCH_LANGUAGE_CLASSIFIER_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitLanguageClassifier(const char* config_file);
int CallClassify(const char* tweet, const unsigned int tweet_len,
                 char* lang_buffer, const unsigned int lang_buffer_len);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_SEARCH_LANGUAGE_CLASSIFIER_ERL_INTERFACE_H_
