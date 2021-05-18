#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 7
 *
 * When it is their turn, a player may either pass (by announcing "pass" and
 * performing no action) or play.
 * A play consists of the following steps (performed in the prescribed order):
 *
 */

// Step 1. (Playing a stone)
// Placing a stone of their color on an empty intersection.
// It can never be moved to another intersection after being played.

TEST(Rule7, PlaceStoneOnEmptyIntersection) {
  json game = socket_api::create_game(R"(
    +--+
    |  |
    +--+
  )");

  game = socket_api::play_stone(game, R"(
    X--+
    |  |
    +--+
  )");

  socket_api::assert_eq(game, R"(
    Active: White
    X--+
    |  |
    +--+
  )");
}

TEST(Rule7, PlaceStoneOnOccupiedIntersection) {
  json game = socket_api::create_game(R"(
    O--+
    |  |
    +--+
  )");

  game = socket_api::play_stone(game, R"(
    X--+
    |  |
    +--+
  )");

  socket_api::assert_eq(game, R"(
    Active: Black
    O--+
    |  |
    +--+
  )");
}


// Step 2. (Capture)
// Removing from the board any stones of their opponent's color that have no liberties.

TEST(Rule7, CaptureWhiteStones) {
  json game = socket_api::create_game(R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--X--+--+
    |  |  |  |  |
    +--X--O--+--+
    |  |  |  |  |
    +--+--X--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");

  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--X--+--+
    |  |  |  |  |
    +--X--O--X--+
    |  |  |  |  |
    +--+--X--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");

  socket_api::assert_eq(game, R"(
    Active: White
    +--+--+--+--+
    |  |  |  |  |
    +--+--X--+--+
    |  |  |  |  |
    +--X--+--X--+
    |  |  |  |  |
    +--+--X--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");
}

TEST(Rule7, CaptureOnEdge) {
  json game = socket_api::create_game(R"(
    +--+--+--+--+
    |  |  |  |  |
    X--+--+--+--+
    |  |  |  |  |
    O--X--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    X--+--+--+--+
    |  |  |  |  |
    O--X--+--+--+
    |  |  |  |  |
    X--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");
  
  socket_api::assert_eq(game, R"(
    Active: White
    +--+--+--+--+
    |  |  |  |  |
    X--+--+--+--+
    |  |  |  |  |
    +--X--+--+--+
    |  |  |  |  |
    X--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
  )");
}

TEST(Rule7, CaptureBlackStones) {
  json game = socket_api::create_game(R"(
    Active: White
    +--O--+--+--+
    |  |  |  |  |
    O--X--+--+--+
    |  |  |  |  |
    O--X--X--O--+
    |  |  |  |  |
    +--O--X--O--+
    |  |  |  |  |
    +--+--O--+--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--O--+--+--+
    |  |  |  |  |
    O--X--O--+--+
    |  |  |  |  |
    O--X--X--O--+
    |  |  |  |  |
    +--O--X--O--+
    |  |  |  |  |
    +--+--O--+--+
  )");
  
  socket_api::assert_eq(game, R"(
    Active: Black
    +--O--+--+--+
    |  |  |  |  |
    O--+--O--+--+
    |  |  |  |  |
    O--+--+--O--+
    |  |  |  |  |
    +--O--+--O--+
    |  |  |  |  |
    +--+--O--+--+
  )");
}

TEST(Rule7, CapturePassivePlayerBeforeActivePlayer) {
  json game = socket_api::create_game(R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--X--X
    |  |  |  |  |
    +--+--X--O--O
    |  |  |  |  |
    +--+--X--O--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--X--X
    |  |  |  |  |
    +--+--X--O--O
    |  |  |  |  |
    +--+--X--O--X
  )");

  socket_api::assert_eq(game, R"(
    Active: White
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--X--X
    |  |  |  |  |
    +--+--X--+--+
    |  |  |  |  |
    +--+--X--+--X
  )");
}

TEST(Rule7, CaptureAfterCapture) {
  json game = socket_api::create_game(R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--X--X--O
    |  |  |  |  |
    +--X--O--O--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--X--X--O
    |  |  |  |  |
    +--X--O--O--X
  )");

  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--X--X--O
    |  |  |  |  |
    +--X--+--O--X
  )");

  socket_api::assert_eq(game, R"(
    Active: Black
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--X--X--O
    |  |  |  |  |
    +--X--+--O--+
  )");
}

TEST(Rule7, CaptureMultipleChains) {
  json game = socket_api::create_game(R"(
    Active: White
    +--+--+--+--+
    |  |  |  |  |
    O--O--+--O--+
    |  |  |  |  |
    O--X--O--X--O
    |  |  |  |  |
    X--+--X--X--O
    |  |  |  |  |
    X--X--O--O--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    O--O--+--O--+
    |  |  |  |  |
    O--X--O--X--O
    |  |  |  |  |
    X--O--X--X--O
    |  |  |  |  |
    X--X--O--O--+
  )");
  

  socket_api::assert_eq(game, R"(
    Active: Black
    +--+--+--+--+
    |  |  |  |  |
    O--O--+--O--+
    |  |  |  |  |
    O--+--O--+--O
    |  |  |  |  |
    +--O--+--+--O
    |  |  |  |  |
    +--+--O--O--+
  )");
}

// Step 3. (Self-capture)
// Removing from the board any stones of their own color that have no liberties.

TEST(Rule7, ShouldNotSelfCapture) {
  json game = socket_api::create_game(R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    X--X--X--X--X
    |  |  |  |  |
    X--O--O--O--O
    |  |  |  |  |
    X--O--+--X--X
  )");

  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    X--X--X--X--X
    |  |  |  |  |
    X--O--O--O--O
    |  |  |  |  |
    X--O--X--X--X
  )");
  
  socket_api::assert_eq(game, R"(
    Active: White
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    X--X--X--X--X
    |  |  |  |  |
    X--+--+--+--+
    |  |  |  |  |
    X--+--X--X--X
  )");
}

TEST(Rule7, ShouldSelfCaptureStone) {
  json game = socket_api::create_game(R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    O--O--+--+--+
    |  |  |  |  |
    +--O--+--+--+
  )");
  
  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    O--O--+--+--+
    |  |  |  |  |
    X--O--+--+--+
  )");
  
  socket_api::assert_eq(game, R"(
    Active: White
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    O--O--+--+--+
    |  |  |  |  |
    +--O--+--+--+
  )");
}

TEST(Rule7, ShouldSelfCaptureChain) {
  json game = socket_api::create_game(R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--O--O--O
    |  |  |  |  |
    +--O--X--+--O
    |  |  |  |  |
    +--+--O--X--X
  )");
  
  game = socket_api::play_stone(game, R"(
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--O--O--O
    |  |  |  |  |
    +--O--X--X--O
    |  |  |  |  |
    +--+--O--X--X
  )");

  socket_api::assert_eq(game, R"(
    Active: White
    +--+--+--+--+
    |  |  |  |  |
    +--+--+--+--+
    |  |  |  |  |
    +--+--O--O--O
    |  |  |  |  |
    +--O--+--+--O
    |  |  |  |  |
    +--+--O--+--+
  )");
}

