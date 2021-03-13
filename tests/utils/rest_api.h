#ifndef JSON_API_H_
#define JSON_API_H_ 



#include <cstdio>
#include <memory>
#include <stdexcept>
#include <string>
#include <array>
#include <regex>
#include <nlohmann/json.hpp>
#include <httplib.h>



using namespace std;
using json = nlohmann::json;



namespace rest_api {
  json create_game(json data = "{}"_json);
}



#endif
