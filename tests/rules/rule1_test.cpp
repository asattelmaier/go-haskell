#include <gtest/gtest.h>
#include "../utils/socket_api.h"



/*
 * Rule 1
 *
 * Go is a game between two players, called Black and White.
 * 
 */

TEST(Rule1, GameBetweenBlackAndWhite) {
  // TODO: Create function for new game
  json game = socket_api::send(R"({ "command": { "name": "NewGame" } })"_json);
  
  ASSERT_EQ(game["activePlayer"].get<string>(),"Black");
  ASSERT_EQ(game["passivePlayer"].get<string>(), "White");
}
