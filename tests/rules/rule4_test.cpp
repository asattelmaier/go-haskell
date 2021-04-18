#include <gtest/gtest.h>
#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 4
 *
 * At any time in the game, each intersection on the board is in one and only one of the following three states:
 *
 */

// 1) empty;

TEST(Rule4, EmptyIntersection) {
  json game = socket_api::create_game();

  string state = board::get_state(board::get_board(game), make_tuple(0, 0));

  ASSERT_EQ(state, "Empty");
}


// 2) occupied by a black stone; or

TEST(Rule4, OccupiedByBlack) {
  json game = socket_api::create_game();

  tuple<int, int> location = make_tuple(0, 0);
  json board = board::get_board(socket_api::play_stone(game, location));
  string state = board::get_state(board, location);

  ASSERT_EQ(state, "Black");
}


// 3) occupied by a white stone. A position consists of an indication of the state of each intersection.

TEST(Rule4, OccupiedByWhite) {
  json game = socket_api::create_game();

  json firstPlay = socket_api::play_stone(game, make_tuple(0, 0));
  
  json secondPlay = socket_api::play_stone(firstPlay, make_tuple(1, 0));
  
  json board = board::get_board(secondPlay);
  string state = board::get_state(board, make_tuple(1, 0));

  ASSERT_EQ(state, "White");
}
