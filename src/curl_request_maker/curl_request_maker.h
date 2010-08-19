#ifndef _INAGIST_API_CURL_REQUEST_MAKER_H_
#define _INAGIST_API_CURL_REQUEST_MAKER_H_

#define DISALLOW_COPY_AND_ASSIGN(TypeName) \
    TypeName(const TypeName&);    \
    void operator=(const TypeName)

#include <curl/curl.h>
#include <cstring>
#include <string>

namespace inagist_api {

const int DEFAULT_BUFFER_SIZE = 1024;
const std::string INAGIST_TOP_TWITTER_TWEETS_URL = "http://inagist.com/api/v1/get_top_tweets?userid=jebui&limit=5&ham=24";


class CurlRequestMaker {

 public:
  CurlRequestMaker();
  ~CurlRequestMaker();
  bool IsCurlInit();
  void GetLastWebResponse(std::string& web_response);
  void GetLastCurlError(std::string& curl_error);
  bool GetTopTweets();
  bool GetTweets(const char* url);

 private:
  CURL *m_curl_handle;
  char m_error_buffer[DEFAULT_BUFFER_SIZE];
  std::string m_call_back_data;

  bool m_curl_call_back_params_set;

  // methods
  void ClearCurlCallBackBuffers();
  void PrepareCurlCallBack();

  void PrepareStandardParams();
  static int CurlCallBack(char *data, size_t size, size_t nmemb, CurlRequestMaker *curl_request_maker);
  int SaveLastWebResponse(char *&data, size_t size);
  bool PerformGet(const std::string& get_url);
};

}

#endif // _INAGIST_API_CURL_REQUEST_MAKER_H_
