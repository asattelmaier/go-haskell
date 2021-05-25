#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Optional Rule 7a
 *
 * Prohibition of suicide: A play is illegal if one or more stones of that
 * player's color would be removed in Step 3 of that play.
 *
 */

TEST(Rule7a, ProhibitionOfSuicide) {
  bool isSucideAllowed = false;

  json game = socket_api::create_game(R"(
    +--O--+
    |  |  |
    O--+--+
    |  |  |
    +--+--+
  )", isSucideAllowed);

  game = socket_api::play_stone(game, R"(
    X--O--+
    |  |  |
    O--+--+
    |  |  |
    +--+--+
  )");

  socket_api::assert_eq(game, R"(
    Active: Black
    +--O--+
    |  |  |
    O--+--+
    |  |  |
    +--+--+
  )");
}

