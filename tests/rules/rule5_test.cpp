#include <gtest/gtest.h>
#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 5
 *
 * Initial position: At the beginning of the game, the board is empty.
 *
 */

TEST(Rule5, InitialPosition) {
  json game = socket_api::create_game();
  
  json board = board::get_board(game);
  bool hasBlackState = board::has_state(board, "Black");
  bool hasWhiteState = board::has_state(board, "White");
  bool isBoardEmpty = !hasBlackState && !hasWhiteState;

  ASSERT_TRUE(isBoardEmpty);
}
