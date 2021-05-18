#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 6
 *
 * Black moves first. The players alternate thereafter.
 *
 */

TEST(Rule6, Alternate) {
  json game = socket_api::create_game(5);

  game = socket_api::pass(game);

  socket_api::assert_eq(game, R"(
    Active: White
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
