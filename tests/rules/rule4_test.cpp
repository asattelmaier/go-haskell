#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/json_api.h"



using namespace std;
using json = nlohmann::json;



/*
 * Rule 4
 *
 * At any time in the game, each intersection on the board is in one and only one of the following three states:
 *
 */

// 1) empty;

TEST(Rule4, EmptyIntersection) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json goData = json_api::execute(createNewGame);

  json interception = goData["game"]["positions"].front().front().front();

  ASSERT_EQ(interception["state"], "Empty");
}


// 2) occupied by a black stone; or

TEST(Rule4, OccupiedByBlack) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json goData = json_api::execute(createNewGame);

  goData["location"] = json::object({ {"x", 0}, {"y", 0} });
  goData["command"] = "PlayStone";
  json interception = json_api::execute(goData)["game"]["positions"].front().front().front();

  ASSERT_EQ(interception["state"], "Black");
}


// 3) occupied by a white stone. A position consists of an indication of the state of each intersection.

TEST(Rule4, OccupiedByWhite) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json goData = json_api::execute(createNewGame);

  goData["location"] = json::object({ {"x", 0}, {"y", 0} });
  goData["command"] = "PlayStone";
  json played = json_api::execute(goData);
  played["location"] = json::object({ {"x", 1}, {"y", 0} });
  played["command"] = "PlayStone";
  json interception = json_api::execute(played)["game"]["positions"].front().front()[1];

  ASSERT_EQ(interception["state"], "White");
}
