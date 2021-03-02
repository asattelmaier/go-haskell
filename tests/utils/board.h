#ifndef BOARD_H_
#define BOARD_H_ 



#include <nlohmann/json.hpp>



using namespace std;
using json = nlohmann::json;



namespace board {
  bool has_state(json board, string state);
}



#endif
