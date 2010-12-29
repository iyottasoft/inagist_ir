#ifndef _INAGIST_UTILS_DICTIONARY_H_
#define _INAGIST_UTILS_DICTIONARY_H_

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

class Dictionary {
 public:
  Dictionary();
  ~Dictionary();
  int Load(const char* dictionary_file_name);
  int Find(const char* key);
  int Print();

 private:
  //string_hash_set m_dictionary;
  std::set<std::string> m_dictionary;

  DISALLOW_COPY_AND_ASSIGN(Dictionary); 
};

} // inagist_test

#endif // _INAGIST_UTILS_DICTIONARY_H_
