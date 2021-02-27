#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/go_json_api.h"



using namespace std;
using json = nlohmann::json;



/*
 * Rule 1
 *
 * Go is a game between two players, called Black and White.
 * 
 */

TEST(Rule1, GameBetweenBlackAndWhite) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  
  json game = go(createNewGame)["game"];

  ASSERT_EQ(game["activePlayer"].get<string>(),"Black");
  ASSERT_EQ(game["passivePlayer"].get<string>(), "White");
}
