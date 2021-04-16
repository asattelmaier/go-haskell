#include <gtest/gtest.h>
#include "../utils/socket_api.h"



/*
 * Rule 2
 *
 * Go is played on a plain grid of 19 horizontal and 19 vertical lines, called a board.
 * 
 */

TEST(Rule2, BoardSize) {
  json game = socket_api::send(R"({ "command": { "name": "NewGame" } })"_json);
  json board = game["positions"].front();

  ASSERT_EQ(board.size(), 19);
  ASSERT_EQ(board.front().size(), 19);
}
