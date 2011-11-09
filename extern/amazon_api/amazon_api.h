#ifndef _INAGIST_AMAZON_API_TEMPLATE_H_
#define _INAGIST_AMAZON_API_TEMPLATE_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <python2.6/Python.h>

namespace inagist_api {

class AmazonAPI {
 public:
  AmazonAPI(const char* program_name);
  ~AmazonAPI();
  int Init(const char* module_name,
           const char* class_name);
  int ItemSearch();
  int Clear();

 private:
  PyObject* m_pyModuleName;
  PyObject* m_pyModule;
  PyObject* m_pyDict;
  PyObject* m_pyClass;
  PyObject* m_pyInstance;
  PyObject* m_pyValue;

  DISALLOW_COPY_AND_ASSIGN(AmazonAPI); 
};

} // inagist_api

#endif // _INAGIST_AMAZON_API_TEMPLATE_H_
