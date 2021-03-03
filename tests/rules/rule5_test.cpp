#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/json_api.h"
#include "../utils/board.h"



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
  json goData = json::object({ {"game", json_api::execute(createNewGame)} });
  
  json board = goData["game"]["positions"].front();
  bool isBoardEmpty = !board::has_state(board, "Black") && !board::has_state(board, "White");

  ASSERT_TRUE(isBoardEmpty);
}
