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
  json game = socket_api::new_game();

  string state = board::get_state(board::get_board(game), 0, 0);

  ASSERT_EQ(state, "Empty");
}


// 2) occupied by a black stone; or

TEST(Rule4, OccupiedByBlack) {
  json location = json::object({ {"x", 0}, {"y", 0} });
  json game = socket_api::new_game();

  json board = board::get_board(socket_api::play_stone(game, location));
  string state = board::get_state(board, 0, 0);

  ASSERT_EQ(state, "Black");
}


// 3) occupied by a white stone. A position consists of an indication of the state of each intersection.

TEST(Rule4, OccupiedByWhite) {
  json game = socket_api::new_game();

  json firstPlayLocation = json::object({ {"x", 0}, {"y", 0} });
  json firstPlay = socket_api::play_stone(game, firstPlayLocation);
  
  json secondPlayLocation = json::object({ {"x", 1}, {"y", 0} });
  json secondPlay = socket_api::play_stone(firstPlay, secondPlayLocation);
  
  string state = board::get_state(board::get_board(secondPlay), 1, 0);

  ASSERT_EQ(state, "White");
}
