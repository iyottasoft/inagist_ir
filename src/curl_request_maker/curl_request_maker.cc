#include "curl_request_maker.h"

namespace inagist_api {

CurlRequestMaker::CurlRequestMaker() {

  ClearCurlCallBackBuffers();

  // curl_easy_init calls curl_global_init internally
  // but its recommended that we call curl_global_init ourselves
  curl_global_init(CURL_GLOBAL_NOTHING);
  m_curl_handle = curl_easy_init();
  if (NULL == m_curl_handle) {
    std::string str;
    GetLastCurlError(str);
  }
}

CurlRequestMaker::~CurlRequestMaker() {
  if (m_curl_handle) {
    curl_easy_cleanup(m_curl_handle);
    m_curl_handle = NULL;
  }
  curl_global_cleanup();
}

bool CurlRequestMaker::IsCurlInit() {
  return (NULL != m_curl_handle) ? true : false;
}

void CurlRequestMaker::GetLastWebResponse(std::string &web_response) {
  if (m_call_back_data.length())
    web_response = m_call_back_data;
}

void CurlRequestMaker::GetLastCurlError(std::string& curl_error) {
  m_error_buffer[DEFAULT_BUFFER_SIZE-1] = '\0';
  curl_error.assign(m_error_buffer);
}

bool CurlRequestMaker::GetTopTweets() {
  bool ret_val;
  if (IsCurlInit()) {
    PrepareStandardParams();
    ret_val = PerformGet(INAGIST_TOP_TWITTER_TWEETS_URL);
  }
  return ret_val;
}

bool CurlRequestMaker::GetArchievedTweets() {
  return GetTweets(INAGIST_ARCHIEVED_TWITTER_TWEETS_URL.c_str());
}

bool CurlRequestMaker::GetTweets(const char* url) {
  bool ret_val;
  if (IsCurlInit()) {
    PrepareStandardParams();
    ret_val = PerformGet(url);
  }
  return ret_val;
}

void CurlRequestMaker::PrepareStandardParams() {
  // clear call back and error buffers
  ClearCurlCallBackBuffers();

  // prepare cURL call back data and error buffer
  PrepareCurlCallBack();
}

void CurlRequestMaker::ClearCurlCallBackBuffers() {
  m_call_back_data = "";
  memset(m_error_buffer, 0, DEFAULT_BUFFER_SIZE);
}

void CurlRequestMaker::PrepareCurlCallBack() {
  curl_easy_reset(m_curl_handle);
  //if (!m_curl_call_back_params_set) {
    // set buffer to get error
    curl_easy_setopt(m_curl_handle, CURLOPT_ERRORBUFFER, m_error_buffer);

    // set call back function to get response
    curl_easy_setopt(m_curl_handle, CURLOPT_WRITEFUNCTION, CurlCallBack);
    curl_easy_setopt(m_curl_handle, CURLOPT_WRITEDATA, this);

    m_curl_call_back_params_set = true;
  //}
}

int CurlRequestMaker::CurlCallBack(char *data, size_t size, size_t nmemb, CurlRequestMaker *curl_request_maker) {
  int written_size = 0;
  if (NULL != curl_request_maker && NULL != data) {
    // save http response in CurlRequestMaker object's buffer
    written_size = curl_request_maker->SaveLastWebResponse(data, (size*nmemb)); // no idea whats happening here
  }
  return written_size;
}

int CurlRequestMaker::SaveLastWebResponse(char *&data, size_t size) {
  int bytes_written = 0;
  if (data && size) {
    m_call_back_data.append(data, size);
    bytes_written =( int) size;
  }
  return bytes_written;
}

bool CurlRequestMaker::PerformGet(const std::string& get_url) {
  // set http request and url
  curl_easy_setopt(m_curl_handle, CURLOPT_HTTPGET, 1);
  curl_easy_setopt(m_curl_handle, CURLOPT_URL, get_url.c_str());

  // send http request
  if (CURLE_OK == curl_easy_perform(m_curl_handle))
    return true;

  return false;
}

}
