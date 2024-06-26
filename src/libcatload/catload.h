#ifndef _catload_h_
#define _catload_h_

#include <stdio.h>
#include <curl/curl.h>

void downloadCatalogFromURL(const char* url, const char* filename);

#endif
