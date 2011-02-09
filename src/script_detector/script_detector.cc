#include "script_detector.h"
#include <iostream>
#include <cstring>
#include "utf8.h"
#include "script_detector_utils.h"

#ifdef DEBUG
#if DEBUG>0
#define SD_DEBUG DEBUG
#endif
#endif
//#define SD_DEBUG 1

namespace inagist_classifiers {

ScriptDetector::ScriptDetector() {
  if (m_buffer)
    memset(m_buffer, '\0', SD_MAX_BUFFER_LEN);
}

ScriptDetector::~ScriptDetector() {
  Clear();
  if (m_buffer)
    memset(m_buffer, '\0', SD_MAX_BUFFER_LEN);
}

int ScriptDetector::Init() {
  return Clear();
}

int ScriptDetector::Clear() {

  if (!m_script_map.empty())
    m_script_map.clear();

  if (m_buffer)
    m_buffer[0] = '\0';
  else
    return -1;

  return 0;
}

int ScriptDetector::DetectScript(const std::string& text, std::set<std::string>& scripts) {

  if (text.length() <= 1 || text.length() >= SD_MAX_BUFFER_LEN) {
#ifdef SD_DEBUG
    std::cout << "ERROR: invalid length\n";
#endif
    scripts.insert(std::string("rr"));
    return -1;
  }

  strcpy((char *) m_buffer, text.c_str());
  unsigned char* ptr = m_buffer;
  unsigned char* end = (unsigned char*) strchr((char *) m_buffer, '\0');
  if (!ptr || !end || (end-ptr) >= SD_MAX_BUFFER_LEN) {
#ifdef SD_DEBUG
    std::cout << "ERROR: invalid pointers\n";
#endif
    scripts.insert(std::string("rr"));
    return -1;
  }

  int code_point = 0;
  std::string script;

  while (ptr && ptr < end) {
    try {
      code_point = utf8::next(ptr, end);
      if (inagist_classifiers::DetectScript(code_point, script) >= 0) {
        scripts.insert(script); 
        TrackScripts(script);
      }
    } catch (...) {
      ptr = NULL;
      end = NULL;
      scripts.insert(std::string("rr"));
      return -1;
    }
  }
  ptr = NULL;
  end = NULL;

  return scripts.size();
}

int ScriptDetector::TrackScripts(const std::string& script) {
  if (m_script_map.find(script) != m_script_map.end()) {
    m_script_map[script] += 1;
  } else {
    m_script_map[script] = 1; 
  }
  return 0;
}

int ScriptDetector::GetMaxScript(std::string& script) {
  // currently sending the first script with more than 9 characters
  // not necessarily the max
  if (m_script_map.empty()) {
    script = "xx";
    return -1;
  }

  int ret_value = 0;
  for (m_script_map_iter = m_script_map.begin(); m_script_map_iter != m_script_map.end(); m_script_map_iter++) {
    if ((ret_value = m_script_map_iter->second) > 9) {
      script = m_script_map_iter->first;
      break;
    }
  }

  if (ret_value <= 9) {
    script = "uu";
    ret_value = 0;
  }

  return ret_value;
}

int ScriptDetector::GetScripts(std::set<std::string>& scripts) {

  if (m_script_map.empty())
    return 0;

  for (m_script_map_iter = m_script_map.begin(); m_script_map_iter != m_script_map.end(); m_script_map_iter++)
    scripts.insert(m_script_map_iter->first);

  return scripts.size();
}

int ScriptDetector::PrintScripts() {

  if (m_script_map.empty())
    return 0;

  for (m_script_map_iter = m_script_map.begin(); m_script_map_iter != m_script_map.end(); m_script_map_iter++)
    std::cout << m_script_map_iter->first << " " << m_script_map_iter->second << std::endl;

  return m_script_map.size();
}

}
