#include "func_utils.h"
#include <cstring>
#include <cmath>

#ifdef DEBUG
#if DEBUG>0
#define UTILS_DEBUG DEBUG
#endif
#endif
//#define UTILS_DEBUG 0

#define MAX_BUFFER_LEN 1024

namespace inagist_utils {
  // unless otherwise specified functions return 0 or NULL or false as default
  // return values less than 0 are likely error codes

int Heapify(double& top1, std::string& top1_class,
            double& top2, std::string& top2_class,
            double& top3, std::string& top3_class) {

  double freq = top3;
  std::string temp_class = top3_class;
  if (freq > top2) {
    if (freq > top1) {
      top3_class = top2_class;
      top3 = top2;
      top2_class = top1_class;
      top2 = top1;
      top1_class = temp_class;
      top1 = freq;
      return 2;
    } else {
      top3_class = top2_class;
      top3 = top2;
      top2_class = temp_class;
      top2 = freq;
      return 1;
    }
  }

  return 0;
}

inline int InsertIntoBuffer(std::string& insert_str,
                            unsigned char* buffer, const unsigned int& buffer_len,
                            unsigned int& current_len, unsigned int& content_count) { 

  if (!buffer) {
    return -1;
  }

  unsigned int len = insert_str.length();
  if (len < 1) {
    return 0;
  }

  if (current_len + len >= buffer_len) {
    return 0;
  }

  if (strstr((char *) buffer, insert_str.c_str()) == NULL) {
    strncpy((char *) buffer + current_len, insert_str.c_str(), len);
    current_len += len;
    strcpy((char *) buffer + current_len, "|");
    current_len += 1;
    content_count++;
  }

  return 1;
}

int FindTopN(std::map<std::string, double>& string_double_map,
             unsigned int& n,
             char* buffer, const unsigned int buffer_len,
             unsigned int& output_len, unsigned int& output_count
            ) {

  // currently disregarding input n. n is assumed to be 3

  if (string_double_map.empty()) {
    return 0;
  }

  std::map<std::string, double>::iterator map_iter;

  double sum = 0;
  double top1 = 0;
  std::string top1_element;
  double top2 = 0;
  std::string top2_element;
  double top3 = 0;
  std::string top3_element;

  if (string_double_map.size() > 1) {
    map_iter = string_double_map.begin();
    top1 = map_iter->second;
    top1_element = map_iter->first;
    sum += map_iter->second;
    map_iter++;
    if (top1 < map_iter->second) {
      top2 = top1;
      top2_element = top1_element;
      top1 = map_iter->second;
      top1_element = map_iter->first;
    } else {
      top2 = map_iter->second;
      top2_element = map_iter->first;
    }
    sum += map_iter->second;
    map_iter++;
  }

  if (string_double_map.size() > 2) {
    top3 = map_iter->second;
    top3_element = map_iter->first;
    Heapify(top1, top1_element, top2, top2_element, top3, top3_element);
    sum += map_iter->second;
    map_iter++;
  }

  double freq = 0;
  for (string_double_map.size() > 3;
       map_iter != string_double_map.end();
       map_iter++) {
    freq = map_iter->second;
    sum += freq;
    if (freq > top3) {
      top3 = freq;
      top3_element = map_iter->first;
      Heapify(top1, top1_element, top2, top2_element, top3, top3_element);
    }
  }

  InsertIntoBuffer(top1_element,
                   (unsigned char*) buffer, buffer_len,
                   output_len, output_count);

  double mean = sum/(double) string_double_map.size();

  if (string_double_map.size() > 1) {
    if (exp(top2) > mean) {
      InsertIntoBuffer(top2_element,
                       (unsigned char*) buffer, buffer_len,
                       output_len, output_count);
    }
  }

  if (string_double_map.size() > 2) {
    if (exp(top3) > mean) {
      InsertIntoBuffer(top3_element,
                       (unsigned char*) buffer, buffer_len,
                       output_len, output_count);
    }
  }

  return output_count;
}

} // namespace inagist_utils
