#ifndef _INAGIST_UTILS_STRING_UTILS_H_
#define _INAGIST_UTILS_STRING_UTILS_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>

namespace inagist_utils {

class StringUtils {
 public:
  StringUtils();
  ~StringUtils();
  bool IsPunct(char *ptr, char *prev=NULL, char *next=NULL);
  bool IsIgnore(char *&ptr);
  int TestUtils(const std::string& text, unsigned int text_len);

 private:
  DISALLOW_COPY_AND_ASSIGN(StringUtils);
};

} // namespace inagist_utils

#endif // _INAGIST_UTILS_STRING_UTILS_H_
