#include <iostream>
#include <thread>
#include <gtest/gtest.h>
#include "utils/socket_api.h"



int main(int argc, char **argv) {
  std::thread (&socket_api::init, "ws://localhost:9000").detach();
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

