#ifndef _INAGIST_UTILS_FUNC_UTILS_H_
#define _INAGIST_UTILS_FUNC_UTILS_H_

#ifndef DISALLOW_COPY_AND_ASSIGN
#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
  TypeName(const TypeName&);               \
  void operator=(const TypeName&)
#endif

#include <string>
#include <map>
#include <set>

namespace inagist_utils {

int Heapify(double& top1, std::string& top1_class,
            double& top2, std::string& top2_class,
            double& top3, std::string& top3_class);

int FindTopN(std::map<std::string, double>& string_double_map,
             unsigned int& n,
             char* buffer, const unsigned int buffer_len,
             unsigned int& output_len, unsigned int& output_count
            );

} // namespace inagist_utils

#endif // _INAGIST_UTILS_FUNC_UTILS_H_
