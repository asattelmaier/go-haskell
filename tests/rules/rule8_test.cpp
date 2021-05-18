#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 8
 *
 * A play is illegal if it would have the effect (after all steps of the play
 * have been completed) of creating a position that has occurred previously in
 * the game.
 *
 */

TEST(Rule8, ProhibitionOfRepetition) {
  json game = socket_api::create_game(R"(
    Active: White
    +--X--O--+--+
    |  |  |  |  |
    X--+--+--O--+
    |  |  |  |  |
    +--X--O--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");

  game = socket_api::play_stone(game, R"(
    +--X--O--+--+
    |  |  |  |  |
    X--O--+--O--+
    |  |  |  |  |
    +--X--O--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--X--O--+--+
    |  |  |  |  |
    X--O--X--O--+
    |  |  |  |  |
    +--X--O--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--X--O--+--+
    |  |  |  |  |
    X--O--X--O--+
    |  |  |  |  |
    +--X--O--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");
  
  socket_api::assert_eq(game, R"(
    Active: White
    +--X--O--+--+
    |  |  |  |  |
    X--+--X--O--+
    |  |  |  |  |
    +--X--O--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");
}

TEST(Rule8, LegalRecapture) {
  json game = socket_api::create_game(R"(
    Active: White
    +--+--+--+--+
    |  |  |  |  |
    O--+--+--+--+
    |  |  |  |  |
    +--O--+--+--+
    |  |  |  |  |
    +--X--X--+--+
    |  |  |  |  |
    O--O--X--+--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    O--+--+--+--+
    |  |  |  |  |
    +--O--+--+--+
    |  |  |  |  |
    O--X--X--+--+
    |  |  |  |  |
    O--O--X--+--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    O--+--+--+--+
    |  |  |  |  |
    X--O--+--+--+
    |  |  |  |  |
    O--X--X--+--+
    |  |  |  |  |
    O--O--X--+--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    O--+--+--+--+
    |  |  |  |  |
    X--O--+--+--+
    |  |  |  |  |
    O--X--X--+--+
    |  |  |  |  |
    +--+--X--+--+
  )");
  
  socket_api::assert_eq(game, R"(
    Active: Black
    +--+--+--+--+
    |  |  |  |  |
    O--+--+--+--+
    |  |  |  |  |
    +--O--+--+--+
    |  |  |  |  |
    O--X--X--+--+
    |  |  |  |  |
    +--+--X--+--+
  )");
}

