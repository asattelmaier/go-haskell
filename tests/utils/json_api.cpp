#include "json_api.h"



string execute_command(const char* cmd) {
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



namespace json_api {
  json execute(json payload) {
    string executeGoHaskellCommand(getenv("EXECUTE_GO_HASKELL_COMMAND"));
    string escapedPayload = '"' + replace(payload.dump(), "\"", "\\\"") + '"';
    string command = executeGoHaskellCommand.append(escapedPayload);
    
    string result = execute_command(command.c_str());
  
    return json::parse(result);
  }
}
