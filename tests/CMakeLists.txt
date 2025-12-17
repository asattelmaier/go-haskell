cmake_minimum_required(VERSION 2.8.12)
project(GoHaskellSocketApiTest)
enable_testing()

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

add_executable(
  go_haskell_socket_api_test

  go_haskell_socket_api_test.cpp

  rules/rule1_test.cpp
  rules/rule2_test.cpp
  rules/rule3_test.cpp
  rules/rule4_test.cpp
  rules/rule5_test.cpp
  rules/rule6_test.cpp
  rules/rule7_test.cpp
  rules/rule7a_test.cpp
  rules/rule8_test.cpp
  rules/rule9_test.cpp
  rules/rule10_test.cpp

  utils/socket_api.cpp
  utils/socket_api.h
  utils/board.cpp
  utils/board.h
)

target_link_libraries(go_haskell_socket_api_test ${CONAN_LIBS})

add_test(NAME go_haskell_socket_api_test WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin COMMAND go_haskell_socket_test)

