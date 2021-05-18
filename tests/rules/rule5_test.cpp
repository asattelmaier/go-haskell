#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 5
 *
 * Initial position: At the beginning of the game, the board is empty.
 *
 */

TEST(Rule5, InitialPosition) {
  json game = socket_api::create_game(5);

  socket_api::assert_eq(game, R"(
    Active: Black
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");
}
