#include "language_detector.h"
#include <iostream>

int main(int argc, char* argv[]) {
  if (argc > 2) {
    std::cout << "Usage: " << argv[0] << " [text]\n";
    return -1;
  }

  inagist_classifiers::LanguageDetector ld;
  std::map<std::string, int> features_map;
  std::map<std::string, int>::iterator map_iter;
  std::string text;
  if (argc == 1) {
    while (1) {
      getline(std::cin, text);
      if (ld.GetNgrams(text.c_str(), text.length(), features_map) <= 0) {
        std::cout << "Error: could not find ngrams" << std::endl;
      } else {
        for (map_iter = features_map.begin(); map_iter != features_map.end(); map_iter++)
          std::cout << (*map_iter).first << " " << (*map_iter).second << std::endl;
      }
      features_map.clear();
    }
  }

  return 0;
}
