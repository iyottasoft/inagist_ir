#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cstdlib>
#include <cstring>

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cout << argv[0] << " <language_model_file_name>\n";
    return -1;
  }

  std::string lang_model_file_name = std::string(argv[1]);
  std::ifstream ifs(lang_model_file_name.c_str());
  if (!ifs.is_open()) {
    std::cout << "ERROR: could not open file " << lang_model_file_name << std::endl;
    return -1;
  }

  std::map<std::string, std::string> lang_corpus;
  std::string line;
  std::string key;
  std::string value;
  std::string::size_type loc;
  char* ptr = NULL;
  bool all_caps = true;
  char text[1024];
  memset(text, '\0', 1024);
  while (getline(ifs, line)) {
    //std::cout << line << std::endl;
    if ((loc = line.find("=")) != std::string::npos) {
      key.assign(line.c_str(), loc);
      value.assign(line, loc+1, line.length()-loc-1);

      all_caps = true;
      strcpy(text, key.c_str());
      ptr = text;
      while (ptr != NULL && *ptr != '\0') {
        if (*ptr > 96 && *ptr < 123) {
          all_caps = false;
        }
        ptr++;
      }
      if (!all_caps) {
        lang_corpus.insert(std::pair<std::string, std::string> (key, value));
      } else {
        std::cout << key << ", " << value << " is invalid. ignoring." << std::endl;
      }
    }
  }
  ifs.close();

  std::ofstream ofs(lang_model_file_name.c_str());
  if (ofs) {
    std::map<std::string, std::string>::iterator map_iter;
    for (map_iter = lang_corpus.begin(); map_iter != lang_corpus.end(); map_iter++) {
      ofs << map_iter->first << map_iter->second << std::endl;
    }
  }
  ofs.close();

  return 0;

}
