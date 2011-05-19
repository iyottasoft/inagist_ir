/* langd.h */

#ifndef _INAGIST_SEARCH_TEXT_CLASSIFIER_ERL_INTERFACE_H_
#define _INAGIST_SEARCH_TEXT_CLASSIFIER_ERL_INTERFACE_H_

#ifdef _CPLUSPLUS
#include <cstdlib>
#endif

#ifdef _CPLUSPLUS
extern "C" {
#endif
int InitTextClassifier(const char* config_file);
int ClassifyText(const char* text, const unsigned int text_len,
                 char* text_class_buffer, const unsigned int text_class_buffer_len);
#ifdef _CPLUSPLUS
}
#endif

#endif // _INAGIST_SEARCH_TEXT_CLASSIFIER_ERL_INTERFACE_H_
