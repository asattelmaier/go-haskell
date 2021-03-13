#include "rest_api.h"



httplib::Client client("localhost", 8000);



namespace rest_api {
  json create_game(json data) {
    auto response = client.Post("/game", data.dump(), "application/json");
    return json::parse(response->body);
  }
}

