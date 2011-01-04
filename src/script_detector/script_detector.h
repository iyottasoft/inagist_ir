#ifndef _INAGIST_CLASSIFIERS_SCRIPT_DETECTOR_
#define _INAGIST_CLASSIFIERS_SCRIPT_DETECTOR_

#include <string>
#include <set>
#include <map>

#ifndef SD_MAX_BUFFER_LEN
#define SD_MAX_BUFFER_LEN 560
#endif

namespace inagist_classifiers {

class ScriptDetector {
 public:
  ScriptDetector();
  ~ScriptDetector();
  int Init();
  int Clear();
  int DetectScript(const std::string& text, std::set<std::string>& scripts);
  int DetectScript(int code_point, std::string& script);
  int GetMaxScript(std::string& script);
  int GetScripts(std::set<std::string>& scripts);
  int PrintScripts();
 private:
  std::map<std::string, int> m_script_map;
  std::map<std::string, int>::iterator m_script_map_iter;
  unsigned char m_buffer[SD_MAX_BUFFER_LEN];
};

}

#endif // _INAGIST_CLASSIFIERS_SCRIPT_DETECTOR_
