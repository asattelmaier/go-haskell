#include <gtest/gtest.h>
#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 3
 *
 * Go is played with playing tokens known as stones.
 * 
 */

TEST(Rule3, PlayedWithStones) {
  json game = socket_api::create_game();

  tuple<int, int> location = make_tuple(0, 0);
  json board = board::get_board(socket_api::play_stone(game, location));
  bool hasStonePlayed = board::get_state(board, location) == "Black";

  ASSERT_TRUE(hasStonePlayed);
}

