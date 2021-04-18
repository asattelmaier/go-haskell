#include "board.h"



namespace board {
  json get_board(json game, int position) {
    return game["positions"][position];
  }
 


  string get_state(json board, tuple<int, int> location) {
    return board[get<1>(location)][get<0>(location)]["state"];
  }



  bool has_state(json board, string state) {
    bool hasState = false;
  
    for (const json & line : board) {
      if (hasState) {
        break;
      }
  
      for (const json & intersection : line) {
        if (hasState) {
          break;
        }
  
  
        if (intersection["state"] == state) {
          hasState = true;
        }
      }
    }
  
    return hasState;
  }
}
