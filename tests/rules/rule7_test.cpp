#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/go_json_api.h"



using namespace std;
using json = nlohmann::json;



/*
 * Rule 7
 *
 * When it is their turn, a player may either pass (by announcing "pass" and performing no action) or play.
 * A play consists of the following steps (performed in the prescribed order):
 *
 */

// Step 1. (Playing a stone)
// Placing a stone of their color on an empty intersection.
// It can never be moved to another intersection after being played.

TEST(Rule7, PlaceStoneOnEmptyIntersection) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json goData = go(createNewGame);

  goData["location"] = json::object({ {"x", 0}, {"y", 0} });
  goData["command"] = "PlayStone";
  json interception = go(goData)["game"]["positions"].front().front().front();

  ASSERT_EQ(interception["state"], "Black");
}

TEST(Rule7, PlaceStoneOnOccupiedIntersection) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json goData = go(createNewGame);

  goData["location"] = json::object({ {"x", 0}, {"y", 0} });
  goData["command"] = "PlayStone";
  json playedGame = go(goData);
  playedGame["location"] = json::object({ {"x", 0}, {"y", 0} });
  playedGame["command"] = "PlayStone";
  json game = go(playedGame)["game"];
  json interception = game["positions"].front().front().front();
  bool hasAlternated = game["activePlayer"] == "Black";

  ASSERT_EQ(interception["state"], "Black");
  ASSERT_FALSE(hasAlternated);
}


// Step 2. (Capture)
// Removing from the board any stones of their opponent's color that have no liberties.

TEST(Rule7, CaptureWhiteStones) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  goData["game"]["positions"][0][1][2]["state"] = "Black";
  goData["game"]["positions"][0][2][1]["state"] = "Black";
  goData["game"]["positions"][0][3][2]["state"] = "Black";
  goData["game"]["positions"][0][2][2]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--X--O--X--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  
  goData["location"] = json::object({ {"x", 3 }, {"y", 2} });
  goData["command"] = "PlayStone";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--X--+--X--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  
  ASSERT_EQ(go(goData)["game"]["positions"][0][2][2]["state"], "Empty");
}

TEST(Rule7, CaptureOnEdge) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  O--X--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  goData["game"]["positions"][0][1][0]["state"] = "Black";
  goData["game"]["positions"][0][2][1]["state"] = "Black";
  goData["game"]["positions"][0][2][0]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  O--X--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  
  goData["location"] = json::object({ {"x", 0 }, {"y", 3} });
  goData["command"] = "PlayStone";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  +--X--+--+--+
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  
  ASSERT_EQ(go(goData)["game"]["positions"][0][2][0]["state"], "Empty");
}

TEST(Rule7, CaptureBlackStones) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--X--+--+--+
   *  |  |  |  |  |
   *  O--X--X--O--+
   *  |  |  |  |  |
   *  +--O--X--O--+
   *  |  |  |  |  |
   *  +--+--O--+--+
   */

  goData["game"]["positions"][0][1][1]["state"] = "Black";
  goData["game"]["positions"][0][2][1]["state"] = "Black";
  goData["game"]["positions"][0][2][2]["state"] = "Black";
  goData["game"]["positions"][0][3][2]["state"] = "Black";
  goData["game"]["positions"][0][0][1]["state"] = "White";
  goData["game"]["positions"][0][1][0]["state"] = "White";
  goData["game"]["positions"][0][2][0]["state"] = "White";
  goData["game"]["positions"][0][2][3]["state"] = "White";
  goData["game"]["positions"][0][3][1]["state"] = "White";
  goData["game"]["positions"][0][3][3]["state"] = "White";
  goData["game"]["positions"][0][4][2]["state"] = "White";
  
  /*
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--X--O--+--+
   *  |  |  |  |  |
   *  O--X--X--O--+
   *  |  |  |  |  |
   *  +--O--X--O--+
   *  |  |  |  |  |
   *  +--+--O--+--+
   */
  
  goData["location"] = json::object({ {"x", 2}, {"y", 1} });
  goData["game"]["activePlayer"] = "White";
  goData["game"]["passivePlayer"] = "Black";
  goData["command"] = "PlayStone";

  /*
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--+--O--+--+
   *  |  |  |  |  |
   *  O--+--+--O--+
   *  |  |  |  |  |
   *  +--O--+--O--+
   *  |  |  |  |  |
   *  +--+--O--+--+
   */

  json afterRemoval = go(goData)["game"]["positions"][0];

  ASSERT_EQ(afterRemoval[1][1]["state"], "Empty");
  ASSERT_EQ(afterRemoval[2][1]["state"], "Empty");
  ASSERT_EQ(afterRemoval[2][2]["state"], "Empty");
  ASSERT_EQ(afterRemoval[3][2]["state"], "Empty");
}

TEST(Rule7, CapturePassivePlayerBeforeActivePlayer) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--X--X
   *  |  |  |  |  |
   *  +--+--X--O--O
   *  |  |  |  |  |
   *  +--+--X--O--+
   */

  goData["game"]["positions"][0][2][3]["state"] = "Black";
  goData["game"]["positions"][0][2][4]["state"] = "Black";
  goData["game"]["positions"][0][3][2]["state"] = "Black";
  goData["game"]["positions"][0][4][2]["state"] = "Black";
  goData["game"]["positions"][0][3][3]["state"] = "White";
  goData["game"]["positions"][0][3][4]["state"] = "White";
  goData["game"]["positions"][0][4][3]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--X--X
   *  |  |  |  |  |
   *  +--+--X--O--O
   *  |  |  |  |  |
   *  +--+--X--O--X
   */
  
  goData["location"] = json::object({ {"x", 4}, {"y", 4} });
  goData["command"] = "PlayStone";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--X--X
   *  |  |  |  |  |
   *  +--+--X--+--+
   *  |  |  |  |  |
   *  +--+--X--+--X
   */

  json afterRemoval = go(goData)["game"]["positions"][0];

  ASSERT_EQ(afterRemoval[3][3]["state"], "Empty");
  ASSERT_EQ(afterRemoval[3][4]["state"], "Empty");
  ASSERT_EQ(afterRemoval[4][3]["state"], "Empty");
}

TEST(Rule7, CaptureAfterCapture) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--O--O--+
   */

  goData["game"]["positions"][0][3][2]["state"] = "Black";
  goData["game"]["positions"][0][3][3]["state"] = "Black";
  goData["game"]["positions"][0][4][1]["state"] = "Black";
  goData["game"]["positions"][0][3][4]["state"] = "White";
  goData["game"]["positions"][0][4][2]["state"] = "White";
  goData["game"]["positions"][0][4][3]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--O--O--X
   */
  
  goData["location"] = json::object({ {"x", 4}, {"y", 4} });
  goData["command"] = "PlayStone";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--+--+--X
   */

  json afterFirstCapture = go(goData);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--+--O--X
   */

  afterFirstCapture["location"] = json::object({ {"x", 3}, {"y", 4} });
  afterFirstCapture["command"] = "PlayStone";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--X--X--O
   *  |  |  |  |  |
   *  +--X--+--O--+
   */

  json afterSecondCapture = go(afterFirstCapture)["game"]["positions"][0];

  ASSERT_EQ(afterSecondCapture[4][2]["state"], "Empty");
  ASSERT_EQ(afterSecondCapture[4][3]["state"], "White");
  ASSERT_EQ(afterSecondCapture[4][4]["state"], "Empty");
}

TEST(Rule7, CaptureMultipleChains) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--O--+
   *  |  |  |  |  |
   *  O--X--O--X--O
   *  |  |  |  |  |
   *  X--+--X--X--O
   *  |  |  |  |  |
   *  X--X--O--O--+
   */

  goData["game"]["positions"][0][2][1]["state"] = "Black";
  goData["game"]["positions"][0][2][3]["state"] = "Black";
  goData["game"]["positions"][0][3][0]["state"] = "Black";
  goData["game"]["positions"][0][3][2]["state"] = "Black";
  goData["game"]["positions"][0][3][3]["state"] = "Black";
  goData["game"]["positions"][0][4][0]["state"] = "Black";
  goData["game"]["positions"][0][4][1]["state"] = "Black";
  goData["game"]["positions"][0][1][0]["state"] = "White";
  goData["game"]["positions"][0][1][1]["state"] = "White";
  goData["game"]["positions"][0][1][3]["state"] = "White";
  goData["game"]["positions"][0][2][0]["state"] = "White";
  goData["game"]["positions"][0][2][2]["state"] = "White";
  goData["game"]["positions"][0][2][4]["state"] = "White";
  goData["game"]["positions"][0][3][4]["state"] = "White";
  goData["game"]["positions"][0][4][2]["state"] = "White";
  goData["game"]["positions"][0][4][3]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--O--+
   *  |  |  |  |  |
   *  O--X--O--X--O
   *  |  |  |  |  |
   *  X--O--X--X--O
   *  |  |  |  |  |
   *  X--X--O--O--+
   */
  
  goData["location"] = json::object({ {"x", 1}, {"y", 3} });
  goData["game"]["activePlayer"] = "White";
  goData["game"]["passivePlayer"] = "Black";
  goData["command"] = "PlayStone";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--O--+
   *  |  |  |  |  |
   *  O--+--O--+--O
   *  |  |  |  |  |
   *  +--O--+--+--O
   *  |  |  |  |  |
   *  +--+--O--O--+
   */

  json afterCapture = go(goData)["game"]["positions"][0];

  ASSERT_EQ(afterCapture[2][1]["state"], "Empty");
  ASSERT_EQ(afterCapture[2][3]["state"], "Empty");
  ASSERT_EQ(afterCapture[3][0]["state"], "Empty");
  ASSERT_EQ(afterCapture[3][1]["state"], "White");
  ASSERT_EQ(afterCapture[3][2]["state"], "Empty");
  ASSERT_EQ(afterCapture[3][3]["state"], "Empty");
  ASSERT_EQ(afterCapture[4][0]["state"], "Empty");
  ASSERT_EQ(afterCapture[4][1]["state"], "Empty");
}

// Step 3. (Self-capture)
// Removing from the board any stones of their own color that have no liberties.

TEST(Rule7, ShouldNotSelfCapture) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--X--X--X--X
   *  |  |  |  |  |
   *  X--O--O--O--O
   *  |  |  |  |  |
   *  X--O--+--X--X
   */

  goData["game"]["positions"][0][2][0]["state"] = "Black";
  goData["game"]["positions"][0][2][1]["state"] = "Black";
  goData["game"]["positions"][0][2][2]["state"] = "Black";
  goData["game"]["positions"][0][2][3]["state"] = "Black";
  goData["game"]["positions"][0][2][4]["state"] = "Black";
  goData["game"]["positions"][0][3][0]["state"] = "Black";
  goData["game"]["positions"][0][4][0]["state"] = "Black";
  goData["game"]["positions"][0][4][3]["state"] = "Black";
  goData["game"]["positions"][0][4][4]["state"] = "Black";
  goData["game"]["positions"][0][3][1]["state"] = "White";
  goData["game"]["positions"][0][3][2]["state"] = "White";
  goData["game"]["positions"][0][3][3]["state"] = "White";
  goData["game"]["positions"][0][3][4]["state"] = "White";
  goData["game"]["positions"][0][4][1]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--X--X--X--X
   *  |  |  |  |  |
   *  X--O--O--O--O
   *  |  |  |  |  |
   *  X--O--X--X--X
   */
  
  goData["location"] = json::object({ {"x", 2}, {"y", 4} });
  goData["command"] = "PlayStone";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  X--X--X--X--X
   *  |  |  |  |  |
   *  X--+--+--+--+
   *  |  |  |  |  |
   *  X--+--X--X--X
   */

  json afterCapture = go(goData)["game"]["positions"][0];

  ASSERT_EQ(afterCapture[3][1]["state"], "Empty");
  ASSERT_EQ(afterCapture[3][2]["state"], "Empty");
  ASSERT_EQ(afterCapture[3][3]["state"], "Empty");
  ASSERT_EQ(afterCapture[3][4]["state"], "Empty");
  ASSERT_EQ(afterCapture[4][1]["state"], "Empty");
  ASSERT_EQ(afterCapture[4][2]["state"], "Black");
}

TEST(Rule7, ShouldSelfCaptureStone) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   */

  goData["game"]["positions"][0][3][0]["state"] = "White";
  goData["game"]["positions"][0][3][1]["state"] = "White";
  goData["game"]["positions"][0][4][1]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--+--+
   *  |  |  |  |  |
   *  X--O--+--+--+
   */
  
  goData["location"] = json::object({ {"x", 0}, {"y", 4} });
  goData["command"] = "PlayStone";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--O--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   */

  json afterCapture = go(goData)["game"]["positions"][0];

  ASSERT_EQ(afterCapture[4][0]["state"], "Empty");
}

TEST(Rule7, ShouldSelfCaptureChain) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--O--O--O
   *  |  |  |  |  |
   *  +--O--X--+--O
   *  |  |  |  |  |
   *  +--+--O--X--X
   */

  goData["game"]["positions"][0][3][2]["state"] = "Black";
  goData["game"]["positions"][0][4][3]["state"] = "Black";
  goData["game"]["positions"][0][4][4]["state"] = "Black";
  goData["game"]["positions"][0][2][2]["state"] = "White";
  goData["game"]["positions"][0][2][3]["state"] = "White";
  goData["game"]["positions"][0][2][4]["state"] = "White";
  goData["game"]["positions"][0][3][1]["state"] = "White";
  goData["game"]["positions"][0][3][4]["state"] = "White";
  goData["game"]["positions"][0][4][2]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--O--O--O
   *  |  |  |  |  |
   *  +--O--X--+--O
   *  |  |  |  |  |
   *  +--+--O--X--X
   */
  
  goData["location"] = json::object({ {"x", 3}, {"y", 3} });
  goData["command"] = "PlayStone";

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--O--O--O
   *  |  |  |  |  |
   *  +--O--+--+--O
   *  |  |  |  |  |
   *  +--+--O--+--+
   */

  json afterCapture = go(goData)["game"]["positions"][0];

  ASSERT_EQ(afterCapture[3][2]["state"], "Empty");
  ASSERT_EQ(afterCapture[3][3]["state"], "Empty");
  ASSERT_EQ(afterCapture[4][3]["state"], "Empty");
  ASSERT_EQ(afterCapture[4][4]["state"], "Empty");
}

