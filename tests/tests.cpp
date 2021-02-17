#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "utils/go_json_api.h"



using namespace std;
using json = nlohmann::json;



// Rule 1
// Go is a game between two players, called Black and White

TEST(Tests, Rule1){
  json newGame = json::object({ {"command", "NewGame"} });
  
  json game = play(newGame)["game"];
  
  ASSERT_EQ(game["activePlayer"].get<string>(),"Black");
  ASSERT_EQ(game["passivePlayer"].get<string>(), "White");
}



// Rule 2
// Go is played on a plain grid of 19 horizontal and 19 vertical lines, called a board.

TEST(Tests, Rule2){
  json newGame = json::object({ {"command", "NewGame"} });
  
  json board = play(newGame)["game"]["positions"][0];

  ASSERT_EQ(board.size(), 19);
  ASSERT_EQ(board[0].size(), 19);
}

