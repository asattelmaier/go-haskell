#include "board.h"



namespace board {
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
