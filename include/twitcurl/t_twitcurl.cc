#include "twitcurl.h"
#include <iostream>
#include <string>

int main() {
  twitCurl tc;
  std::string user_name = "balajinx";
  std::string password = "*isbalaji";
  tc.setTwitterUsername(user_name);
  tc.setTwitterPassword(password);

  std::string query = "http://search.twitter.com/search.json?q=from:baljainix";
  if (!tc.search(query)) {
   std::cout << "Error: could not search for tweets\n";
  } else {
   std::string reply_message;
   tc.getLastWebResponse(reply_message);
   std::cout << reply_message << std::endl;
  }

  return 0;
}
