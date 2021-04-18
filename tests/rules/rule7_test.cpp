#include <gtest/gtest.h>
#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 7
 *
 * When it is their turn, a player may either pass (by announcing "pass" and performing no action) or play.
 * A play consists of the following steps (performed in the prescribed order):
 *
 */

// Step 1. (Playing a stone)
// Placing a stone of their color on an empty intersection.
// It can never be moved to another intersection after being played.

TEST(Rule7, PlaceStoneOnEmptyIntersection) {
  json game = socket_api::create_game();

  tuple<int, int> location = make_tuple(0, 0);
  json board = board::get_board(socket_api::play_stone(game, location));
  string state = board::get_state(board, location);

  ASSERT_EQ(state, "Black");
}

TEST(Rule7, PlaceStoneOnOccupiedIntersection) {
  json game = socket_api::create_game();

  tuple<int, int> location = make_tuple(0, 0);
  game = socket_api::play_stone(game, location);
  json board = board::get_board(socket_api::play_stone(game, location));
  string state = board::get_state(board, location);
  bool hasAlternated = state == "White";

  ASSERT_EQ(state, "Black");
  ASSERT_FALSE(hasAlternated);
}


// Step 2. (Capture)
// Removing from the board any stones of their opponent's color that have no liberties.

TEST(Rule7, CaptureWhiteStones) {
  json game = socket_api::create_game(5);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  game["positions"][0][1][2]["state"] = "Black";
  game["positions"][0][2][1]["state"] = "Black";
  game["positions"][0][3][2]["state"] = "Black";
  game["positions"][0][2][2]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--X--O--X--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  

  tuple<int, int> location = make_tuple(3, 2);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--X--+--X--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  
  json board = board::get_board(socket_api::play_stone(game, location));
  json state = board::get_state(board, make_tuple(2, 2));
 
  ASSERT_EQ(state, "Empty");
}

TEST(Rule7, CaptureOnEdge) {
  json game = socket_api::create_game(5);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  O--X--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  game["positions"][0][1][0]["state"] = "Black";
  game["positions"][0][2][1]["state"] = "Black";
  game["positions"][0][2][0]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  O--X--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  
  tuple<int, int> location = make_tuple(0, 3);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  +--X--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  
  json board = board::get_board(socket_api::play_stone(game, location));
  json state = board::get_state(board, make_tuple(0, 2));
 
  ASSERT_EQ(state, "Empty");
}

TEST(Rule7, CaptureBlackStones) {
  json game = socket_api::create_game(5);

  /*
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--X--+--+--+
   *  |  |  |  |  |
   *  O--X--X--O--+
   *  |  |  |  |  |
   *  +--O--X--O--+
   *  |  |  |  |  |
   *  +--+--O--+--+
   */

  game["positions"][0][1][1]["state"] = "Black";
  game["positions"][0][2][1]["state"] = "Black";
  game["positions"][0][2][2]["state"] = "Black";
  game["positions"][0][3][2]["state"] = "Black";
  game["positions"][0][0][1]["state"] = "White";
  game["positions"][0][1][0]["state"] = "White";
  game["positions"][0][2][0]["state"] = "White";
  game["positions"][0][2][3]["state"] = "White";
  game["positions"][0][3][1]["state"] = "White";
  game["positions"][0][3][3]["state"] = "White";
  game["positions"][0][4][2]["state"] = "White";
  
  /*
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--X--O--+--+
   *  |  |  |  |  |
   *  O--X--X--O--+
   *  |  |  |  |  |
   *  +--O--X--O--+
   *  |  |  |  |  |
   *  +--+--O--+--+
   */
  
  tuple<int, int> location = make_tuple(2, 1);
  game["activePlayer"] = "White";
  game["passivePlayer"] = "Black";

  /*
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--+--O--+--+
   *  |  |  |  |  |
   *  O--+--+--O--+
   *  |  |  |  |  |
   *  +--O--+--O--+
   *  |  |  |  |  |
   *  +--+--O--+--+
   */

  json board = board::get_board(socket_api::play_stone(game, location));

  ASSERT_EQ(board[1][1]["state"], "Empty");
  ASSERT_EQ(board[2][1]["state"], "Empty");
  ASSERT_EQ(board[2][2]["state"], "Empty");
  ASSERT_EQ(board[3][2]["state"], "Empty");
}

TEST(Rule7, CapturePassivePlayerBeforeActivePlayer) {
  json game = socket_api::create_game(5);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--X--X
   *  |  |  |  |  |
   *  +--+--X--O--O
   *  |  |  |  |  |
   *  +--+--X--O--+
   */

  game["positions"][0][2][3]["state"] = "Black";
  game["positions"][0][2][4]["state"] = "Black";
  game["positions"][0][3][2]["state"] = "Black";
  game["positions"][0][4][2]["state"] = "Black";
  game["positions"][0][3][3]["state"] = "White";
  game["positions"][0][3][4]["state"] = "White";
  game["positions"][0][4][3]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--X--X
   *  |  |  |  |  |
   *  +--+--X--O--O
   *  |  |  |  |  |
   *  +--+--X--O--X
   */
  
  
  tuple<int, int> location = make_tuple(4, 4);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--X--X
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--+--X--+--X
   */

  json board = board::get_board(socket_api::play_stone(game, location));

  ASSERT_EQ(board[3][3]["state"], "Empty");
  ASSERT_EQ(board[3][4]["state"], "Empty");
  ASSERT_EQ(board[4][3]["state"], "Empty");
}

TEST(Rule7, CaptureAfterCapture) {
  json game = socket_api::create_game(5);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--O--O--+
   */

  game["positions"][0][3][2]["state"] = "Black";
  game["positions"][0][3][3]["state"] = "Black";
  game["positions"][0][4][1]["state"] = "Black";
  game["positions"][0][3][4]["state"] = "White";
  game["positions"][0][4][2]["state"] = "White";
  game["positions"][0][4][3]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--O--O--X
   */
  
  
  tuple<int, int> location = make_tuple(4, 4);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--+--+--X
   */

  game = socket_api::play_stone(game, location);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--+--O--X
   */

  location = make_tuple(3, 4);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--+--O--+
   */

  json board = board::get_board(socket_api::play_stone(game, location));

  ASSERT_EQ(board[4][2]["state"], "Empty");
  ASSERT_EQ(board[4][3]["state"], "White");
  ASSERT_EQ(board[4][4]["state"], "Empty");
}

TEST(Rule7, CaptureMultipleChains) {
  json game = socket_api::create_game(5);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--O--+
   *  |  |  |  |  |
   *  O--X--O--X--O
   *  |  |  |  |  |
   *  X--+--X--X--O
   *  |  |  |  |  |
   *  X--X--O--O--+
   */

  game["positions"][0][2][1]["state"] = "Black";
  game["positions"][0][2][3]["state"] = "Black";
  game["positions"][0][3][0]["state"] = "Black";
  game["positions"][0][3][2]["state"] = "Black";
  game["positions"][0][3][3]["state"] = "Black";
  game["positions"][0][4][0]["state"] = "Black";
  game["positions"][0][4][1]["state"] = "Black";
  game["positions"][0][1][0]["state"] = "White";
  game["positions"][0][1][1]["state"] = "White";
  game["positions"][0][1][3]["state"] = "White";
  game["positions"][0][2][0]["state"] = "White";
  game["positions"][0][2][2]["state"] = "White";
  game["positions"][0][2][4]["state"] = "White";
  game["positions"][0][3][4]["state"] = "White";
  game["positions"][0][4][2]["state"] = "White";
  game["positions"][0][4][3]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--O--+
   *  |  |  |  |  |
   *  O--X--O--X--O
   *  |  |  |  |  |
   *  X--O--X--X--O
   *  |  |  |  |  |
   *  X--X--O--O--+
   */
  
  tuple<int, int> location = make_tuple(1, 3);
  game["activePlayer"] = "White";
  game["passivePlayer"] = "Black";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--O--+
   *  |  |  |  |  |
   *  O--+--O--+--O
   *  |  |  |  |  |
   *  +--O--+--+--O
   *  |  |  |  |  |
   *  +--+--O--O--+
   */

  json board = board::get_board(socket_api::play_stone(game, location));

  ASSERT_EQ(board[2][1]["state"], "Empty");
  ASSERT_EQ(board[2][3]["state"], "Empty");
  ASSERT_EQ(board[3][0]["state"], "Empty");
  ASSERT_EQ(board[3][1]["state"], "White");
  ASSERT_EQ(board[3][2]["state"], "Empty");
  ASSERT_EQ(board[3][3]["state"], "Empty");
  ASSERT_EQ(board[4][0]["state"], "Empty");
  ASSERT_EQ(board[4][1]["state"], "Empty");
}

// Step 3. (Self-capture)
// Removing from the board any stones of their own color that have no liberties.

TEST(Rule7, ShouldNotSelfCapture) {
  json game = socket_api::create_game(5);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--X--X--X--X
   *  |  |  |  |  |
   *  X--O--O--O--O
   *  |  |  |  |  |
   *  X--O--+--X--X
   */

  game["positions"][0][2][0]["state"] = "Black";
  game["positions"][0][2][1]["state"] = "Black";
  game["positions"][0][2][2]["state"] = "Black";
  game["positions"][0][2][3]["state"] = "Black";
  game["positions"][0][2][4]["state"] = "Black";
  game["positions"][0][3][0]["state"] = "Black";
  game["positions"][0][4][0]["state"] = "Black";
  game["positions"][0][4][3]["state"] = "Black";
  game["positions"][0][4][4]["state"] = "Black";
  game["positions"][0][3][1]["state"] = "White";
  game["positions"][0][3][2]["state"] = "White";
  game["positions"][0][3][3]["state"] = "White";
  game["positions"][0][3][4]["state"] = "White";
  game["positions"][0][4][1]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--X--X--X--X
   *  |  |  |  |  |
   *  X--O--O--O--O
   *  |  |  |  |  |
   *  X--O--X--X--X
   */
  
  tuple<int, int> location = make_tuple(2, 4);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--X--X--X--X
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  X--+--X--X--X
   */

  json board = board::get_board(socket_api::play_stone(game, location));

  ASSERT_EQ(board[3][1]["state"], "Empty");
  ASSERT_EQ(board[3][2]["state"], "Empty");
  ASSERT_EQ(board[3][3]["state"], "Empty");
  ASSERT_EQ(board[3][4]["state"], "Empty");
  ASSERT_EQ(board[4][1]["state"], "Empty");
  ASSERT_EQ(board[4][2]["state"], "Black");
}

TEST(Rule7, ShouldSelfCaptureStone) {
  json game = socket_api::create_game(5);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   */

  game["positions"][0][3][0]["state"] = "White";
  game["positions"][0][3][1]["state"] = "White";
  game["positions"][0][4][1]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--+--+
   *  |  |  |  |  |
   *  X--O--+--+--+
   */
  
  tuple<int, int> location = make_tuple(0, 4);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   */

  json board = board::get_board(socket_api::play_stone(game, location));

  ASSERT_EQ(board[4][0]["state"], "Empty");
}

TEST(Rule7, ShouldSelfCaptureChain) {
  json game = socket_api::create_game(5);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--O--O--O
   *  |  |  |  |  |
   *  +--O--X--+--O
   *  |  |  |  |  |
   *  +--+--O--X--X
   */

  game["positions"][0][3][2]["state"] = "Black";
  game["positions"][0][4][3]["state"] = "Black";
  game["positions"][0][4][4]["state"] = "Black";
  game["positions"][0][2][2]["state"] = "White";
  game["positions"][0][2][3]["state"] = "White";
  game["positions"][0][2][4]["state"] = "White";
  game["positions"][0][3][1]["state"] = "White";
  game["positions"][0][3][4]["state"] = "White";
  game["positions"][0][4][2]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--O--O--O
   *  |  |  |  |  |
   *  +--O--X--+--O
   *  |  |  |  |  |
   *  +--+--O--X--X
   */
  
  tuple<int, int> location = make_tuple(3, 3);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--O--O--O
   *  |  |  |  |  |
   *  +--O--+--+--O
   *  |  |  |  |  |
   *  +--+--O--+--+
   */

  json board = board::get_board(socket_api::play_stone(game, location));

  ASSERT_EQ(board[3][2]["state"], "Empty");
  ASSERT_EQ(board[3][3]["state"], "Empty");
  ASSERT_EQ(board[4][3]["state"], "Empty");
  ASSERT_EQ(board[4][4]["state"], "Empty");
}

