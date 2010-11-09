#include "sentiment_analyser.h"
#include "twitter_searcher.h"
#include <iostream>

int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cout << "Usage: " << argv[0] << " <evidence_map_file>\n";
    return -1;
  }

  inagist_classifiers::SentimentAnalyser sa;
  if (sa.Init(argv[1]) < 0) {
    std::cout << "Error: could not initialize sentiment analyser\n";
    return -1;
  }

  inagist_api::TwitterSearcher ts;

  return 0;
}

