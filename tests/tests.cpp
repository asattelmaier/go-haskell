#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "utils/go_json_api.h"
#include <iostream>



using namespace std;
using json = nlohmann::json;



// Rule 1
// Go is a game between two players, called Black and White

TEST(Tests, Rule1) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  
  json game = play(createNewGame)["game"];

  ASSERT_EQ(game["activePlayer"].get<string>(),"Black");
  ASSERT_EQ(game["passivePlayer"].get<string>(), "White");
}



// Rule 2
// Go is played on a plain grid of 19 horizontal and 19 vertical lines, called a board.

TEST(Tests, Rule2) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  
  json board = play(createNewGame)["game"]["positions"].front();

  ASSERT_EQ(board.size(), 19);
  ASSERT_EQ(board.front().size(), 19);
}



// Rule 3
// Go is played with playing tokens known as stones.

TEST(Tests, Rule3) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

  newGame["location"] = json::object({ {"x", 0}, {"y", 0} });
  newGame["command"] = "PlayStone";
  json command = play(newGame)["command"];

  ASSERT_EQ(command, "PlayStone");
}



// Rule 4
// At any time in the game, each intersection on the board is in one and only one of the following three states:
//   1) empty;

TEST(Tests, Rule4Empty) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

  json interception = newGame["game"]["positions"].front().front().front();

  ASSERT_EQ(interception["state"], "Empty");
}

//   2) occupied by a black stone; or

TEST(Tests, Rule4Black) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

  newGame["location"] = json::object({ {"x", 0}, {"y", 0} });
  newGame["command"] = "PlayStone";
  json interception = play(newGame)["game"]["positions"].front().front().front();

  ASSERT_EQ(interception["state"], "Black");
}

//   3) occupied by a white stone. A position consists of an indication of the state of each intersection.

TEST(Tests, Rule4White) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

  newGame["location"] = json::object({ {"x", 0}, {"y", 0} });
  newGame["command"] = "PlayStone";
  json playedGame = play(newGame);
  playedGame["location"] = json::object({ {"x", 1}, {"y", 0} });
  playedGame["command"] = "PlayStone";
  json interception = play(playedGame)["game"]["positions"].front().front()[1];

  ASSERT_EQ(interception["state"], "White");
}



// Rule 5
// Initial position: At the beginning of the game, the board is empty.

TEST(Tests, Rule5) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  
  json board = play(createNewGame)["game"]["positions"].front();
  bool isBoardEmpty = !has_state(board, "Black") && !has_state(board, "White");

  ASSERT_TRUE(isBoardEmpty);
}



// Rule 6
// Black moves first. The players alternate thereafter.

TEST(Tests, Rule6) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

  string firstPlayer = newGame["game"]["activePlayer"];
  newGame["command"] = "Pass";
  bool hasAlternated = play(newGame)["game"]["activePlayer"] == "White";

  ASSERT_EQ(firstPlayer, "Black");
  ASSERT_TRUE(hasAlternated);
}



// Rule 7
// Moving: When it is their turn, a player may either pass (by announcing "pass" and performing no action) or play.
// A play consists of the following steps (performed in the prescribed order):
//   Step 1. (Playing a stone)
//   Placing a stone of their color on an empty intersection.
//   It can never be moved to another intersection after being played.

TEST(Tests, Rule7Step1Empty) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

  newGame["location"] = json::object({ {"x", 0}, {"y", 0} });
  newGame["command"] = "PlayStone";
  json interception = play(newGame)["game"]["positions"].front().front().front();

  ASSERT_EQ(interception["state"], "Black");
}

TEST(Tests, Rule7Step1Occupied) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

  newGame["location"] = json::object({ {"x", 0}, {"y", 0} });
  newGame["command"] = "PlayStone";
  json playedGame = play(newGame);
  playedGame["location"] = json::object({ {"x", 0}, {"y", 0} });
  playedGame["command"] = "PlayStone";
  json game = play(playedGame)["game"];
  json interception = game["positions"].front().front().front();
  bool hasAlternated = game["activePlayer"] == "Black";

  ASSERT_EQ(interception["state"], "Black");
  ASSERT_FALSE(hasAlternated);
}

//   Step 2. (Capture)
//   Removing from the board any stones of their opponent's color that have no liberties.

TEST(Tests, Rule7Step2BlackCapture) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

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

  newGame["game"]["positions"][0][1][2]["state"] = "Black";
  newGame["game"]["positions"][0][2][1]["state"] = "Black";
  newGame["game"]["positions"][0][3][2]["state"] = "Black";
  newGame["game"]["positions"][0][2][2]["state"] = "White";
  
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
  
  newGame["location"] = json::object({ {"x", 3 }, {"y", 2} });
  newGame["command"] = "PlayStone";

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
  
  ASSERT_EQ(play(newGame)["game"]["positions"][0][2][2]["state"], "Empty");
}

TEST(Tests, Rule7Step2EdgeCapture) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

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

  newGame["game"]["positions"][0][1][0]["state"] = "Black";
  newGame["game"]["positions"][0][2][1]["state"] = "Black";
  newGame["game"]["positions"][0][2][0]["state"] = "White";
  
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
  
  newGame["location"] = json::object({ {"x", 0 }, {"y", 3} });
  newGame["command"] = "PlayStone";

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
  
  ASSERT_EQ(play(newGame)["game"]["positions"][0][2][0]["state"], "Empty");
}

TEST(Tests, Rule7Step2WhiteCapture) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

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

  newGame["game"]["positions"][0][1][1]["state"] = "Black";
  newGame["game"]["positions"][0][2][1]["state"] = "Black";
  newGame["game"]["positions"][0][2][2]["state"] = "Black";
  newGame["game"]["positions"][0][3][2]["state"] = "Black";
  newGame["game"]["positions"][0][0][1]["state"] = "White";
  newGame["game"]["positions"][0][1][0]["state"] = "White";
  newGame["game"]["positions"][0][2][0]["state"] = "White";
  newGame["game"]["positions"][0][2][3]["state"] = "White";
  newGame["game"]["positions"][0][3][1]["state"] = "White";
  newGame["game"]["positions"][0][3][3]["state"] = "White";
  newGame["game"]["positions"][0][4][2]["state"] = "White";
  
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
  
  newGame["location"] = json::object({ {"x", 2}, {"y", 1} });
  newGame["game"]["activePlayer"] = "White";
  newGame["game"]["passivePlayer"] = "Black";
  newGame["command"] = "PlayStone";

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

  json afterRemoval = play(newGame)["game"]["positions"][0];

  ASSERT_EQ(afterRemoval[1][1]["state"], "Empty");
  ASSERT_EQ(afterRemoval[2][1]["state"], "Empty");
  ASSERT_EQ(afterRemoval[2][2]["state"], "Empty");
  ASSERT_EQ(afterRemoval[3][2]["state"], "Empty");
}

TEST(Tests, Rule7Step2WhiteCapturedBeforeBlack) {
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

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

  // TODO: Add API for adjusting board size
  newGame["game"]["positions"][0][2][3]["state"] = "Black";
  newGame["game"]["positions"][0][2][4]["state"] = "Black";
  newGame["game"]["positions"][0][3][2]["state"] = "Black";
  newGame["game"]["positions"][0][4][2]["state"] = "Black";
  newGame["game"]["positions"][0][3][3]["state"] = "White";
  newGame["game"]["positions"][0][3][4]["state"] = "White";
  newGame["game"]["positions"][0][4][3]["state"] = "White";
  
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
  
  newGame["location"] = json::object({ {"x", 4}, {"y", 4} });
  newGame["command"] = "PlayStone";

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

  json afterRemoval = play(newGame)["game"]["positions"][0];

  ASSERT_EQ(afterRemoval[3][3]["state"], "Empty");
  ASSERT_EQ(afterRemoval[3][4]["state"], "Empty");
  ASSERT_EQ(afterRemoval[4][3]["state"], "Empty");
}
