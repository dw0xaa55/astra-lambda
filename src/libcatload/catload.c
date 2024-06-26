#include "catload.h"

void downloadCatalogFromURL(const char* url, const char* filename){
  CURL *curl = curl_easy_init();
  if(curl){
    curl_easy_setopt(curl, CURLOPT_URL, url);
    FILE* file = fopen(filename, "w");
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, file);
    curl_easy_perform(curl);
  }
  curl_easy_cleanup(curl);
}
