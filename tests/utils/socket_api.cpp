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



namespace socket_api {
  void init(string uri) {
    socket_client.init_asio();
    socket_client.start_perpetual();
    
    socket_client.set_open_handler(bind(&on_open));
    socket_client.set_message_handler(bind(&on_message, ::_2));

    websocketpp::lib::error_code ec;
    client::connection_ptr connection = socket_client.get_connection(uri, ec);
    connection_handle = connection->get_handle();
    socket_client.connect(connection);
    
    socket_client.run();
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
}

