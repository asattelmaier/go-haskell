#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "utils/go_json_api.h"
#include <iostream>



using namespace std;
using json = nlohmann::json;



// Rule 1
// Go is a game between two players, called Black and White

TEST(Tests, Rule1){
  json createNewGame = json::object({ {"command", "NewGame"} });
  
  json game = play(createNewGame)["game"];

  ASSERT_EQ(game["activePlayer"].get<string>(),"Black");
  ASSERT_EQ(game["passivePlayer"].get<string>(), "White");
}



// Rule 2
// Go is played on a plain grid of 19 horizontal and 19 vertical lines, called a board.

TEST(Tests, Rule2){
  json createNewGame = json::object({ {"command", "NewGame"} });
  
  json board = play(createNewGame)["game"]["positions"][0];

  ASSERT_EQ(board.size(), 19);
  ASSERT_EQ(board[0].size(), 19);
}



// Rule 3
// Go is played with playing tokens known as stones.

TEST(Tests, Rule3){
  json createNewGame = json::object({ {"command", "NewGame"} });
  json newGame = play(createNewGame);

  newGame["location"] = json::object({ {"x", 0}, {"y", 0} });
  newGame["command"] = "PlayStone";
  json interception = play(newGame)["game"]["positions"][0][0][0];

  // TODO: Update API, remove "state" and "location"
  ASSERT_EQ(interception["state"], "Black");
}

