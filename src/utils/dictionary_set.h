#ifndef _INAGIST_UTILS_DICTIONARY_SET_H_
#define _INAGIST_UTILS_DICTIONARY_SET_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

//#include <ext/hash_set>
#include <string>
#include <set>

/*
namespace __gnu_cxx
{
  template<> struct hash< std::string > {
    size_t operator()( const std::string& x ) const {
      return hash< const char* >()( x.c_str() );
    }
  };
}
*/

namespace inagist_utils {

//typedef __gnu_cxx::hash_set<std::string, __gnu_cxx::hash<std::string> > string_hash_set;

class DictionarySet {
 public:
  DictionarySet();
  ~DictionarySet();
  int Load(const char* dictionary_file_name);
  int Find(const unsigned char* key);
  int Find(std::string &key);
  int Print();
  int Clear();

 private:
  std::set<std::string> m_dictionary_set;

  DISALLOW_COPY_AND_ASSIGN(DictionarySet); 
};

} // inagist_test

#endif // _INAGIST_UTILS_DICTIONARY_SET_H_
