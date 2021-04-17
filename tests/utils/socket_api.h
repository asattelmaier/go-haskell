#ifndef JSON_API_H_
#define JSON_API_H_ 



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
  
  json new_game(int size);

  json new_game();

  json play_stone(json game, json location);

}



#endif
