#ifndef _INAGIST_UTILS_TEST_INPUT_HANDLER_
#define _INAGIST_UTILS_TEST_INPUT_HANDLER_

#include <string>

namespace inagist_test_utils {

typedef struct _test_input {
  std::string config;
  std::string keytuples_config;
  unsigned int input_type;
  unsigned int output_type;
  unsigned int debug_level;
  std::string input_type_value;
} TestInput;

class TestInputHandler {
 public:
  TestInputHandler();
  ~TestInputHandler();
  int ReadArgs(int argc, char* argv[], TestInput& test_input);
  int PrintArgs(const TestInput& test_input);
  virtual int TestFunction(std::string text)=0;
 private:
};

} // namespace

#endif // _INAGIST_UTILS_TEST_INPUT_HANDLER_
