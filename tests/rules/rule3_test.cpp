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
  json game = socket_api::new_game();
  json location = json::object({ {"x", 0}, {"y", 0} });

  json board = board::get_board(socket_api::play_stone(game, location));
  bool hasStonePlayed = board::get_state(board, 0, 0) == "Black";

  ASSERT_TRUE(hasStonePlayed);
}

