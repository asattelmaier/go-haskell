#ifndef JSON_API_H_
#define JSON_API_H_ 



#include <gtest/gtest.h>
#include <tuple>
#include <string>
#include <atomic>
#include <nlohmann/json.hpp>
#include <websocketpp/config/asio_no_tls_client.hpp>
#include <websocketpp/client.hpp>
#include <boost/thread/thread.hpp>



typedef websocketpp::client<websocketpp::config::asio_client> client;



using namespace std;
using json = nlohmann::json;
using websocketpp::lib::placeholders::_2;
using websocketpp::lib::bind;



namespace socket_api {

  void init(string uri);
  
  json create_game();
  
  json create_game(int size, bool isSuicideAllowed = true);

  json create_game(string position, bool isSuicideAllowed = true);

  json play_stone(json game, tuple<int, int> location);
  
  json play_stone(json game, string position);

  json pass(json game);

  void assert_eq(json game, string position);

}



#endif
