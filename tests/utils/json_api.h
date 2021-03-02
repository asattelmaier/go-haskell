#ifndef JSON_API_H_
#define JSON_API_H_ 



#include <cstdio>
#include <memory>
#include <stdexcept>
#include <string>
#include <array>
#include <regex>
#include <nlohmann/json.hpp>



using namespace std;
using json = nlohmann::json;



namespace json_api {
  json execute(json payload);
}



#endif
