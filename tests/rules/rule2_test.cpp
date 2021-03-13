#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/rest_api.h"



using namespace std;
using json = nlohmann::json;



/*
 * Rule 2
 *
 * Go is played on a plain grid of 19 horizontal and 19 vertical lines, called a board.
 * 
 */

TEST(Rule2, BoardSize) {
  json board = rest_api::create_game().front();

  ASSERT_EQ(board.size(), 19);
  ASSERT_EQ(board.front().size(), 19);
}
