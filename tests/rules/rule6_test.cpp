#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/json_api.h"



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
  json goData = json::object({ {"game", json_api::execute(createNewGame)} });

  string firstPlayer = goData["game"]["activePlayer"];
  goData["command"] = "Pass";
  string secondPlayer = json_api::execute(goData)["activePlayer"];
  bool hasAlternated = firstPlayer == "Black" && secondPlayer == "White";

  ASSERT_TRUE(hasAlternated);
}
