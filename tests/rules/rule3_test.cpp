#include <gtest/gtest.h>
#include "../utils/socket_api.h"



/*
 * Rule 3
 *
 * Go is played with playing tokens known as stones.
 * 
 */

TEST(Rule3, PlayedWithStones) {
  json goData = json::object({ {"game", socket_api::send(R"({ "command": { "name": "NewGame" } })"_json)} });

  goData["command"] = json::object({ {"name", "PlayStone"} }); 
  goData["command"]["location"] = json::object({ {"x", 0}, {"y", 0} });
  bool hasStonePlayed = socket_api::send(goData)["positions"][0][0][0]["state"] == "Black";

  ASSERT_TRUE(hasStonePlayed);
}

