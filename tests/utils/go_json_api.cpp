#include "go_json_api.h"



string exec(const char* cmd) {
  array<char, 128> buffer;
  string result;
  unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd, "r"), pclose);
         
  if (!pipe) {
    throw runtime_error("popen() failed!");
  }
           
  while (fgets(buffer.data(), buffer.size(), pipe.get()) != nullptr) {
    result += buffer.data();
  }
              
  return result;
}



string replace(string const & in, string const & from, string const & to) {
  return regex_replace(in, regex(from), to);
}



json play(json game) {
  string base = "cd ../../../ && cabal -v0 run -- -m json ";
  string escapedGame = '"' + replace(game.dump(), "\"", "\\\"") + '"';
  string command = base.append(escapedGame);
  
  string result = exec(command.c_str());

  return json::parse(result);
}