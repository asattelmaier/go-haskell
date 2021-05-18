#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 2
 *
 * Go is played on a plain grid of 19 horizontal and 19 vertical lines, called a board.
 * 
 */

TEST(Rule2, BoardSize) {
  json game = socket_api::create_game();
  json board = board::get_board(game);

  ASSERT_EQ(board.size(), 19);
  ASSERT_EQ(board.front().size(), 19);
}
