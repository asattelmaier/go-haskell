#include <gtest/gtest.h>
#include "../utils/socket_api.h"
#include "../utils/board.h"



/*
 * Rule 6
 *
 * Black moves first. The players alternate thereafter.
 *
 */

TEST(Rule6, Alternate) {
  json game = socket_api::create_game();

  string firstPlayer = game["activePlayer"];
  string secondPlayer = socket_api::pass(game)["activePlayer"];
  bool hasAlternated = firstPlayer == "Black" && secondPlayer == "White";

  ASSERT_TRUE(hasAlternated);
}
