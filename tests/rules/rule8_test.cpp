#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "../utils/json_api.h"



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
  json goData = json::object({ {"game", json_api::execute(createNewGame)} });

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
  goData["game"] = json_api::execute(goData);
  
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

  goData["location"] = json::object({ {"x", 2}, {"y", 1} });
  goData["command"] = "PlayStone";
  goData["game"] = json_api::execute(goData);

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
  goData["command"] = "PlayStone";
  
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
 
  json game = json_api::execute(goData);

  
  ASSERT_EQ(game["positions"][0][1][1]["state"], "Empty");
  ASSERT_EQ(game["activePlayer"], "White");
  ASSERT_EQ(game["passivePlayer"], "Black");
}

TEST(Rule8, LegalRecapture) {
  json createNewGame = json::object({ {"command", "NewGame"}, {"size", 5} });
  json goData = json::object({ {"game", json_api::execute(createNewGame)} });

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
  goData["game"] = json_api::execute(goData);
  
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

  goData["location"] = json::object({ {"x", 0}, {"y", 2} });
  goData["command"] = "PlayStone";
  goData["game"] = json_api::execute(goData);

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

  goData["location"] = json::object({ {"x", 0}, {"y", 3} });
  goData["command"] = "PlayStone";
  json game = json_api::execute(goData);


  ASSERT_EQ(game["positions"][0][2][0]["state"], "Empty");
  ASSERT_EQ(game["positions"][0][3][0]["state"], "White");
  ASSERT_EQ(game["activePlayer"], "Black");
  ASSERT_EQ(game["passivePlayer"], "White");
}
