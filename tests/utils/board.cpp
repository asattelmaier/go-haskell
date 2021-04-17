#include "board.h"



namespace board {
  json get_board(json game, int position) {
    return game["positions"][position];
  }
 


  string get_state(json board, int x, int y) {
    return board[y][x]["state"];
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
