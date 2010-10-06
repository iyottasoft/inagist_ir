#include <iostream>
#include <fstream>
#include <cassert>
#include "utf8.h"

using namespace std;

int main(int argc, char *argv[]) {

/*
  ifstream ifs("ta.txt", ios::in | ios::binary);
  if (ifs.is_open()) {
    cout << "error\n";
  }
  int cp = 0;
  string line;
  string::iterator it;
  while (getline(ifs, line)) {
    it = line.begin();
    while (it != line.end()) {
      string::iterator end = line.end();
      cp = utf8::next(line.begin(), *end);
      cout << cp << endl;
      it += cp;
    }
  }
*/

  //char twochars[] = "\xe6\x97\xa5\xd1\x88";
  char twochars[] = "abcde";
  //char twochars[] = "\x61";
  char* w = twochars;
  int cp = utf8::next(w, twochars + 6);
  std::cout << cp << std::endl;
  assert (cp == 0x65e5);
  assert (w == twochars + 3);
  cp = utf8::next(w, twochars + 6);
  std::cout << cp << std::endl;

  return 0;
}
