#ifndef _INAGIST_CLASSIFIERS_SCRIPT_DETECTOR_UTILS_
#define _INAGIST_CLASSIFIERS_SCRIPT_DETECTOR_UTILS_

#include <string>

namespace inagist_classifiers {

int DetectScript(int code_point, std::string &script);
int ExtendedAsciiText(int code_point);

} // namespace

#endif // _INAGIST_CLASSIFIERS_SCRIPT_DETECTOR_UTILS_
