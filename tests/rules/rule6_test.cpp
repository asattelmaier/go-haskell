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
  json game = socket_api::new_game();

  string firstPlayer = goData["game"]["activePlayer"];
  string secondPlayer = socket_api::pass(game)["activePlayer"];
  bool hasAlternated = firstPlayer == "Black" && secondPlayer == "White";

  ASSERT_TRUE(hasAlternated);
}
