#include <gtest/gtest.h>
#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 9
 *
 * The game ends when both players have passed consecutively. The final position
 * is the position on the board at the time the players pass consecutively.
 *
 */

TEST(Rule9, Territory) {
  json game = socket_api::create_game(9);
  
  /*
   *  +--+--X--+--X--+--+--+--+
   *  |  |  |  |  |  |  |  |  |
   *  +--X--+--X--O--+--+--O--+
   *  |  |  |  |  |  |  |  |  |
   *  +--X--X--O--O--+--+--+--O
   *  |  |  |  |  |  |  |  |  |
   *  +--+--X--X--O--+--O--O--X
   *  |  |  |  |  |  |  |  |  |
   *  +--+--X--O--+--+--O--X--X
   *  |  |  |  |  |  |  |  |  |
   *  +--X--+--+--O--O--X--+--X
   *  |  |  |  |  |  |  |  |  |
   *  +--+--X--+--O--X--X--X--X
   *  |  |  |  |  |  |  |  |  |
   *  +--+--X--O--O--O--X--+--O
   *  |  |  |  |  |  |  |  |  |
   *  +--+--+--X--+--O--X--O--+
   */

  game["positions"][0][0][2]["state"] = "Black";
  game["positions"][0][0][4]["state"] = "Black";
  game["positions"][0][1][1]["state"] = "Black";
  game["positions"][0][1][3]["state"] = "Black";
  game["positions"][0][1][4]["state"] = "White";
  game["positions"][0][1][7]["state"] = "White";
  game["positions"][0][2][1]["state"] = "Black";
  game["positions"][0][2][2]["state"] = "Black";
  game["positions"][0][2][3]["state"] = "White";
  game["positions"][0][2][4]["state"] = "White";
  game["positions"][0][2][8]["state"] = "White";
  game["positions"][0][3][2]["state"] = "Black";
  game["positions"][0][3][3]["state"] = "Black";
  game["positions"][0][3][4]["state"] = "White";
  game["positions"][0][3][6]["state"] = "White";
  game["positions"][0][3][7]["state"] = "White";
  game["positions"][0][3][8]["state"] = "Black";
  game["positions"][0][4][2]["state"] = "Black";
  game["positions"][0][4][3]["state"] = "White";
  game["positions"][0][4][6]["state"] = "White";
  game["positions"][0][4][7]["state"] = "Black";
  game["positions"][0][4][8]["state"] = "Black";
  game["positions"][0][5][2]["state"] = "Black";
  game["positions"][0][5][4]["state"] = "White";
  game["positions"][0][5][5]["state"] = "White";
  game["positions"][0][5][6]["state"] = "Black";
  game["positions"][0][5][8]["state"] = "Black";
  game["positions"][0][6][2]["state"] = "Black";
  game["positions"][0][6][4]["state"] = "White";
  game["positions"][0][6][5]["state"] = "Black";
  game["positions"][0][6][6]["state"] = "Black";
  game["positions"][0][6][7]["state"] = "Black";
  game["positions"][0][6][8]["state"] = "Black";
  game["positions"][0][7][2]["state"] = "Black";
  game["positions"][0][7][3]["state"] = "White";
  game["positions"][0][7][4]["state"] = "White";
  game["positions"][0][7][5]["state"] = "White";
  game["positions"][0][7][6]["state"] = "Black";
  game["positions"][0][7][8]["state"] = "White";
  game["positions"][0][8][3]["state"] = "Black";
  game["positions"][0][8][5]["state"] = "White";
  game["positions"][0][8][6]["state"] = "Black";
  game["positions"][0][8][7]["state"] = "White";
  
  game = socket_api::pass(socket_api::pass(game));

  ASSERT_EQ(game["score"], 44);
}

