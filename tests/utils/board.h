#ifndef BOARD_H_
#define BOARD_H_ 



#include <nlohmann/json.hpp>



using namespace std;
using json = nlohmann::json;



namespace board {

  json get_board(json game, int position = 0);

  string get_state(json board, tuple<int, int> location);
  
  bool has_state(json board, string state);

}



#endif
