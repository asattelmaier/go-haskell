#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/go_json_api.h"



using namespace std;
using json = nlohmann::json;



/*
 * Rule 6
 *
 * Black moves first. The players alternate thereafter.
 *
 */

TEST(Rule6, Alternate) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json goData = go(createNewGame);

  string firstPlayer = goData["game"]["activePlayer"];
  goData["command"] = "Pass";
  string secondPlayer = go(goData)["game"]["activePlayer"];
  bool hasAlternated = firstPlayer == "Black" && secondPlayer == "White";

  ASSERT_TRUE(hasAlternated);
}
