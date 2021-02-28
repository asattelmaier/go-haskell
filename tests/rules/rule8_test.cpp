#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/go_json_api.h"



using namespace std;
using json = nlohmann::json;



/*
 * Rule 8
 *
 * A play is illegal if it would have the effect (after all steps of the play have been completed)
 * of creating a position that has occurred previously in the game.
 *
 */

TEST(Rule8, ProhibitionOfRepetition) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--+--+--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  goData["game"]["positions"][0][0][1]["state"] = "Black";
  goData["game"]["positions"][0][1][0]["state"] = "Black";
  goData["game"]["positions"][0][2][1]["state"] = "Black";
  goData["game"]["positions"][0][0][2]["state"] = "White";
  goData["game"]["positions"][0][1][3]["state"] = "White";
  goData["game"]["positions"][0][2][2]["state"] = "White";
  
  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--O--+--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
  
  goData["location"] = json::object({ {"x", 1}, {"y", 1} });
  goData["game"]["activePlayer"] = "White";
  goData["game"]["passivePlayer"] = "Black";
  goData["command"] = "PlayStone";
  json whitePlayed = go(goData);
  
  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--+--X--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  whitePlayed["location"] = json::object({ {"x", 2}, {"y", 1} });
  whitePlayed["command"] = "PlayStone";
  json blackPlayed = go(whitePlayed);

  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--O--+--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */

  blackPlayed["location"] = json::object({ {"x", 1}, {"y", 1} });
  blackPlayed["command"] = "PlayStone";
  
  /*
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  X--+--X--O--+
   *  |  |  |  |  |
   *  +--X--O--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  +--+--+--+--+
   */
 
  json illegalRecapture = go(blackPlayed)["game"];

  
  ASSERT_EQ(illegalRecapture["positions"][0][1][1]["state"], "Empty");
  ASSERT_EQ(illegalRecapture["activePlayer"], "White");
  ASSERT_EQ(illegalRecapture["passivePlayer"], "Black");
}

TEST(Rule8, LegalRecapture) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = go(createNewGame);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--+--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  +--X--X--+--+
   *  |  |  |  |  |
   *  O--O--X--+--+
   */

  goData["game"]["positions"][0][3][1]["state"] = "Black";
  goData["game"]["positions"][0][3][2]["state"] = "Black";
  goData["game"]["positions"][0][4][2]["state"] = "Black";
  goData["game"]["positions"][0][1][0]["state"] = "White";
  goData["game"]["positions"][0][2][1]["state"] = "White";
  goData["game"]["positions"][0][4][0]["state"] = "White";
  goData["game"]["positions"][0][4][1]["state"] = "White";
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--+--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--X--X--+--+
   *  |  |  |  |  |
   *  O--O--X--+--+
   */
  
  goData["location"] = json::object({ {"x", 0}, {"y", 3} });
  goData["game"]["activePlayer"] = "White";
  goData["game"]["passivePlayer"] = "Black";
  goData["command"] = "PlayStone";
  json whitePlayed = go(goData);
  
  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--+--+--+--+
   *  |  |  |  |  |
   *  X--O--+--+--+
   *  |  |  |  |  |
   *  +--X--X--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   */

  whitePlayed["location"] = json::object({ {"x", 0}, {"y", 2} });
  whitePlayed["command"] = "PlayStone";
  json blackPlayed = go(whitePlayed);

  /*
   *  +--+--+--+--+
   *  |  |  |  |  |
   *  O--+--+--+--+
   *  |  |  |  |  |
   *  +--O--+--+--+
   *  |  |  |  |  |
   *  O--X--X--+--+
   *  |  |  |  |  |
   *  +--+--X--+--+
   */

  blackPlayed["location"] = json::object({ {"x", 0}, {"y", 3} });
  blackPlayed["command"] = "PlayStone";
  json legalRecapture = go(blackPlayed)["game"];


  ASSERT_EQ(legalRecapture["positions"][0][2][0]["state"], "Empty");
  ASSERT_EQ(legalRecapture["positions"][0][3][0]["state"], "White");
  ASSERT_EQ(legalRecapture["activePlayer"], "Black");
  ASSERT_EQ(legalRecapture["passivePlayer"], "White");
}
