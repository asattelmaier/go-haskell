#include "socket_api.h"



websocketpp::connection_hdl connection_handle;
client socket_client;
atomic<bool> has_responded;
atomic<bool> is_open;
string responseData;



void on_open() {
  is_open = true;
}



void on_message(client::message_ptr message) {
  responseData = message->get_payload();
  has_responded = true;
}
  


json send(json data) {
  has_responded = false;

  while(!is_open.load()) {
    boost::this_thread::sleep(boost::posix_time::milliseconds(100));
  }
    
  socket_client.send(
    connection_handle,
    data.dump(),
    websocketpp::frame::opcode::text
  );
    
  while(!has_responded.load()) {
    boost::this_thread::sleep(boost::posix_time::milliseconds(100));
  }

  return json::parse(responseData);
}



json create_location(tuple<int, int> location) {
  return json::object({
    {"x", get<0>(location)},
    {"y", get<1>(location)}
  });
}



namespace socket_api {
  void init(string uri) {
    socket_client.init_asio();
    socket_client.start_perpetual();

    socket_client.clear_access_channels(websocketpp::log::alevel::all);
    
    socket_client.set_open_handler(bind(&on_open));
    socket_client.set_message_handler(bind(&on_message, ::_2));

    websocketpp::lib::error_code ec;
    client::connection_ptr connection = socket_client.get_connection(uri, ec);
    connection_handle = connection->get_handle();
    socket_client.connect(connection);
    
    socket_client.run();
  }

  

  json create_game() {
    return send(R"({ "command": { "name": "CreateGame" } })"_json);
  }



  json create_game(int size) {
    json data = R"({ "command": { "name": "CreateGame" } })"_json;
    data["command"]["size"] = size;
    
    return send(data);
  }



  json play_stone(json game, tuple<int, int> location) {
    json data = R"({ "command": { "name": "PlayStone" } })"_json;
    data["command"]["location"] = create_location(location);
    data["game"] = game;
    
    return send(data);
  }



  json pass(json game) {
    json data = R"({ "command": { "name": "Pass" } })"_json;
    data["game"] = game;
    
    return send(data);
  }
}

