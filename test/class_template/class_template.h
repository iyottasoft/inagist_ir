#ifndef _INAGIST_TEST_CLASS_TEMPLATE_H_
#define _INAGIST_TEST_CLASS_TEMPLATE_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

namespace inagist_test {

class ClassTemplate {
 public:
  ClassTemplate();
  ~ClassTemplate();

 private:

  DISALLOW_COPY_AND_ASSIGN(ClassTemplate); 
};

} // inagist_test

#endif // _INAGIST_TEST_CLASS_TEMPLATE_H_
