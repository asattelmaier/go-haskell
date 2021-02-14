#ifndef GO_JSON_API_H_
#define GO_JSON_API_H_ 



#include <cstdio>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <array>
#include <regex>
#include <nlohmann/json.hpp>



using namespace std;
using json = nlohmann::json;



json play(json game);



#endif
