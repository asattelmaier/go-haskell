#include <list>
#include <gtest/gtest.h>
#include <nlohmann/json.hpp>
#include "utils/go_json_api.h"



using namespace std;
using json = nlohmann::json;



// Rule 1
// Go is a game between two players, called Black and White

TEST(Tests, Rule1){
  list<string> expectedPlayers = {"Black", "White"};
  json newGame = json::object({ {"command", "NewGame"} });
  
  json game = play(newGame)["game"];
  list<string> actualPlayers = {game["activePlayer"].get<string>(), game["passivePlayer"].get<string>()};
  
  EXPECT_EQ(actualPlayers, expectedPlayers);
}

