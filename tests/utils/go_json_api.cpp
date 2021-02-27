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



json go(json payload) {
  string base = "cd ../../../ && cabal -v0 run -- -m json ";
  string escapedPayload = '"' + replace(payload.dump(), "\"", "\\\"") + '"';
  string command = base.append(escapedPayload);
  
  string result = exec(command.c_str());

  return json::parse(result);
}



bool has_state(json board, string state) {
  bool hasState = false;

  for (const json & line : board) {
    if (hasState) {
      break;
    }

    for (const json & intersection : line) {
      if (hasState) {
        break;
      }


      if (intersection["state"] == state) {
        hasState = true;
      }
    }
  }

  return hasState;
}

