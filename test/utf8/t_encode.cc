#include <iostream>
#include <cstring>

using namespace std;

int main() {

/*
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
*/

/*
  unsigned int value = 0xFD9B;
  int count = 0;
  int byte_count = 0;
  while (value) {
    int r = value % 2;
    value/=2;
    cout << r;
    count++;
    if (count == 6) {
      cout << "01" << endl;
      count = 0;
      byte_count++;
    }
  }
  byte_count++;
  for (int i=count; i < (8-byte_count); i++)
    cout << "0";
  for (int i=0; i < byte_count; i++)
    cout << "1";
  cout << endl;
*/

  unsigned int value = 0;
  value = 0xFD9B;
  //value = 0x00A9;
  //value = 0x3D;
  value = 0x0B95;
  cout << "value: " << value << endl;
  int byte_count = 0;
  unsigned char byte = 0;
  int r = 0;
  int total_count = 0;
  int count = 0;
  while (value) {
    r = value % 2;
    if (r)
      byte |= 0x80;
    byte >>= 1;
    value/= 2;
    count++;
    total_count++;
    if (count == 6) {
      count = 0;
      //if (init_value > 255) {
        byte >>= 1;
        byte |= 0x80;
        total_count += 2;
        cout << byte << endl;
        std::cout << ((byte & 0xF0) >> 4) << (byte & 0x0F) << std::endl;
        byte = 0;
      //}
    }
  } 
  byte_count = (total_count / 8) + 1;

  for (int i=(total_count % 8); i < (8-byte_count); i++) {
    byte >>= 1;
  }
  for (int i=0; i < (byte_count-1); i++) {
    byte |= 0x80;
    byte >>= 1;
  }
  if (byte_count > 1)
    byte |= 0x80;
  cout << byte << endl;
  std::cout << ((byte & 0xF0) >> 4) << (byte & 0x0F) << std::endl;
  return 0;
}
