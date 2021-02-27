#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/go_json_api.h"



using namespace std;
using json = nlohmann::json;



/*
 * Rule 5
 *
 * Initial position: At the beginning of the game, the board is empty.
 *
 */

TEST(Rule5, InitialPosition) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  
  json board = go(createNewGame)["game"]["positions"].front();
  bool isBoardEmpty = !has_state(board, "Black") && !has_state(board, "White");

  ASSERT_TRUE(isBoardEmpty);
}
