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
  json goData = json::object({ {"game", json_api::execute(createNewGame)} });

  goData["location"] = json::object({ {"x", 0}, {"y", 0} });
  goData["command"] = "PlayStone";
  bool hasStonePlayed = json_api::execute(goData)["positions"][0][0][0]["state"] == "Black";

  ASSERT_TRUE(hasStonePlayed);
}

