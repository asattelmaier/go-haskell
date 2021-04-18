#include <gtest/gtest.h>
#include <iostream>
#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 8
 *
 * A play is illegal if it would have the effect (after all steps of the play have been completed)
 * of creating a position that has occurred previously in the game.
 *
 */

TEST(Rule8, ProhibitionOfRepetition) {
  json game = socket_api::create_game(5);

  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--+--+--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  game["positions"][0][0][1]["state"] = "Black";
  game["positions"][0][1][0]["state"] = "Black";
  game["positions"][0][2][1]["state"] = "Black";
  game["positions"][0][0][2]["state"] = "White";
  game["positions"][0][1][3]["state"] = "White";
  game["positions"][0][2][2]["state"] = "White";
  
  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--O--+--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  
  tuple<int, int> location = make_tuple(1, 1);
  game["activePlayer"] = "White";
  game["passivePlayer"] = "Black";
  game = socket_api::play_stone(game, location);
  
  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--+--X--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  location = make_tuple(2, 1);
  game = socket_api::play_stone(game, location);

  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--O--+--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  location = make_tuple(1, 1);
  
  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--+--X--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
 
  game = socket_api::play_stone(game, location);


  ASSERT_EQ(game["positions"][0][1][1]["state"], "Empty");
  ASSERT_EQ(game["activePlayer"], "White");
  ASSERT_EQ(game["passivePlayer"], "Black");
}

TEST(Rule8, LegalRecapture) {
  json game = socket_api::create_game(5);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--+--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  +--X--X--+--+
   *  |  |  |  |  |
   *  O--O--X--+--+
   */

  game["positions"][0][3][1]["state"] = "Black";
  game["positions"][0][3][2]["state"] = "Black";
  game["positions"][0][4][2]["state"] = "Black";
  game["positions"][0][1][0]["state"] = "White";
  game["positions"][0][2][1]["state"] = "White";
  game["positions"][0][4][0]["state"] = "White";
  game["positions"][0][4][1]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--+--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--X--X--+--+
   *  |  |  |  |  |
   *  O--O--X--+--+
   */
  
  tuple<int, int> location = make_tuple(0, 3);
  game["activePlayer"] = "White";
  game["passivePlayer"] = "Black";
  game = socket_api::play_stone(game, location);
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--+--+--+--+
   *  |  |  |  |  |
   *  X--O--+--+--+
   *  |  |  |  |  |
   *  +--X--X--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   */

  location = make_tuple(0, 2);
  game = socket_api::play_stone(game, location);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--+--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--X--X--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   */

  location = make_tuple(0, 3);
  game = socket_api::play_stone(game, location);


  ASSERT_EQ(game["positions"][0][2][0]["state"], "Empty");
  ASSERT_EQ(game["positions"][0][3][0]["state"], "White");
  ASSERT_EQ(game["activePlayer"], "Black");
  ASSERT_EQ(game["passivePlayer"], "White");
}
