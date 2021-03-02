#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/json_api.h"



using namespace std;
using json = nlohmann::json;



/*
 * Rule 3
 *
 * Go is played with playing tokens known as stones.
 * 
 */

TEST(Rule3, PlayedWithStones) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json goData = json_api::execute(createNewGame);

  goData["location"] = json::object({ {"x", 0}, {"y", 0} });
  goData["command"] = "PlayStone";

  ASSERT_EQ(json_api::execute(goData)["command"], "PlayStone");
}
