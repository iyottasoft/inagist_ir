#ifndef _INAGIST_UTILS_DICTIONARY_MAP_H_
#define _INAGIST_UTILS_DICTIONARY_MAP_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

//#include <ext/hash_set>
#include <string>
#include <map>

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

class DictionaryMap {
 public:
  DictionaryMap();
  ~DictionaryMap();
  int Load(const char* dictionary_file_name);
  int Find(const std::string& key, std::string& value);
  int Find(const unsigned char *key, const unsigned int &key_len, std::string& value);
  //int Find(const unsigned char *key, std::string& value);
  int FindPart(const unsigned char* key, std::string& value);
  int Print();
  int Clear();

 private:
  //string_hash_set m_dictionary_map;
  std::map<std::string, std::string> m_dictionary_map;
  std::map<std::string, std::string>::iterator m_dict_map_iter;

  DISALLOW_COPY_AND_ASSIGN(DictionaryMap); 
};

} // namespace inagist_utils

#endif // _INAGIST_UTILS_DICTIONARY_MAP_H_
