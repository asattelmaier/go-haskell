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



bool isEmpty(char c) {
  return c == '+';
}



bool isBlack(char c) {
  return c == 'X';
}



bool isWhite(char c) {
  return c == 'O';
}



int get_size(string position) {
  istringstream stream(position);
  string line;
  int size = 0;

  while (getline(stream, line)) {
    if (line.find('-') == string::npos) {
      continue;
    }
    
    for(char& c : line) {
      if (isEmpty(c) || isBlack(c) || isWhite(c)) {
        size++;
      }
    }

    break;
  }

  return size;
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
    return send(R"({ "command": { "name": "Create" } })"_json);
  }



  json create_game(int size) {
    json data = R"({ "command": { "name": "Create" } })"_json;
    data["command"]["size"] = size;
    
    return send(data);
  }


  json create_game(string position) {
    json game = create_game(get_size(position));
    istringstream stream(position);
    string line;
    int row = -1;
    int col = -1;

    while (getline(stream, line)) {
      if (line.find("Active") != string::npos) {
        bool isBlackActivePlayer = line.find("Black") != string::npos;
        
        game["activePlayer"] = isBlackActivePlayer ? "Black" : "White";
        game["passivePlayer"] = isBlackActivePlayer ? "White" : "Black";
      }

      if (line.find('-') == string::npos) {
        continue;
      }

      row++;
      col = -1;

      for(char& c : line) {
        if (isEmpty(c) || isBlack(c) || isWhite(c)) {
          col++;
          game["positions"][0][row][col]["location"] = create_location(make_tuple(col, row));
        }

        if (isBlack(c)) {
          game["positions"][0][row][col]["state"] = "Black";
        }
        
        if (isWhite(c)) {
          game["positions"][0][row][col]["state"] = "White";
        }
        
        if (isEmpty(c)) {
          game["positions"][0][row][col]["state"] = "Empty";
        }
      }
    }

    return game;
  }



  json play_stone(json game, tuple<int, int> location) {
    json data = R"({ "command": { "name": "Play" } })"_json;
    data["command"]["location"] = create_location(location);
    data["game"] = game;
    
    return send(data);
  }

  

  json play_stone(json game, string position) {
    json updatedPosition = create_game(position)["positions"].front();
    tuple<int, int> location = make_tuple(-1, -1);

    for (auto const& row : game["positions"].front()) {
      for (auto const& col : row) {
        int x = col["location"]["x"].get<int>();
        int y = col["location"]["y"].get<int>();
        string state = updatedPosition[y][x]["state"].get<string>();

        if (col["state"].get<string>() != state) {
          location = make_tuple(x, y);
          break;
        }
      }

      if (get<0>(location) >= 0) {
        break;
      }
    }

    return play_stone(game, location);
  }



  json pass(json game) {
    json data = R"({ "command": { "name": "Pass" } })"_json;
    data["game"] = game;
    
    return send(data);
  }



  void assert_eq(json game, string position) {
    json expectedGame = create_game(position);
    string actualPosition = game["positions"].front().dump();
    string actualActivePlayer = game["activePlayer"].get<string>();
    string actualPassivePlayer = game["passivePlayer"].get<string>();
    
    string expectedPosition = expectedGame["positions"].front().dump();
    string expectedActivePlayer = expectedGame["activePlayer"].get<string>();
    string expectedPassivePlayer = expectedGame["passivePlayer"].get<string>();
    
    ASSERT_EQ(actualPosition, expectedPosition);
    ASSERT_EQ(actualActivePlayer, expectedActivePlayer);
    ASSERT_EQ(actualPassivePlayer, expectedPassivePlayer);
  }
}

