#ifndef _INAGIST_UTILS_TEST_UTILS_H_
#define _INAGIST_UTILS_TEST_UTILS_H_

#include <string>
#include <set>

namespace inagist_utils {

int GetInputText(const unsigned int &input_type,
                 const char* input_value,
                 std::set<std::string>& docs);

} // namespace inagist_utils

#endif // _INAGIST_UTILS_TEST_UTILS_H_
