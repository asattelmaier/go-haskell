#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 10
 *
 * Winner: If one player has a higher score than the other, then that player
 * wins. Otherwise, the game is a draw.
 *
 */

TEST(Rule10, Winner) {
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

  ASSERT_EQ(game["winner"].size(), 1);
  ASSERT_EQ(game["winner"].front(), "Black");
}

