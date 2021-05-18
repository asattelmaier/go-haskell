#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 9
 *
 * The game ends when both players have passed consecutively. The final position
 * is the position on the board at the time the players pass consecutively.
 *
 */

TEST(Rule9, Territory) {
  json game = socket_api::create_game(R"(
    +--+--X--+--X--+--+--+--+
    |  |  |  |  |  |  |  |  |
    +--X--+--X--O--+--+--O--+
    |  |  |  |  |  |  |  |  |
    +--X--X--O--O--+--+--+--O
    |  |  |  |  |  |  |  |  |
    +--+--X--X--O--+--O--O--X
    |  |  |  |  |  |  |  |  |
    +--+--X--O--+--+--O--X--X
    |  |  |  |  |  |  |  |  |
    +--X--+--+--O--O--X--+--X
    |  |  |  |  |  |  |  |  |
    +--+--X--+--O--X--X--X--X
    |  |  |  |  |  |  |  |  |
    +--+--X--O--O--O--X--+--O
    |  |  |  |  |  |  |  |  |
    +--+--+--X--+--O--X--O--+
  )");
  
  game = socket_api::pass(socket_api::pass(game));

  ASSERT_EQ(game["score"], 43);
}

