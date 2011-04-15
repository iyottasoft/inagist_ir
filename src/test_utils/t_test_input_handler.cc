#include "test_input_handler.h"
#include <iostream>

using namespace inagist_test_utils;

class SampleTestHandler : public TestInputHandler {
 public:
  int TestFunction(std::string &text);
 private:
};

int SampleTestHandler::TestFunction(std::string &text) {
  std::cout << text << std::endl;
  return 0;
}

int main(int argc, char* argv[]) {

  SampleTestHandler* sth;
  TestInput test_input;
  sth->ReadArgs(argc, argv, test_input);

  return 0;
}
