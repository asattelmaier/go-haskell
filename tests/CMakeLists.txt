cmake_minimum_required(VERSION 2.8.12)
project(GoHaskellTest)
enable_testing()

add_definitions("-std=c++11")

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

add_executable(
  test_go_json_api

  go_json_api_test.cpp

  rules/rule1_test.cpp
  rules/rule2_test.cpp
  rules/rule3_test.cpp
  rules/rule4_test.cpp
  rules/rule5_test.cpp
  rules/rule6_test.cpp
  rules/rule7_test.cpp
  rules/rule8_test.cpp

  utils/go_json_api.cpp
  utils/go_json_api.h
)

target_link_libraries(test_go_json_api ${CONAN_LIBS})

add_test(NAME test_go_json_api WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin COMMAND test_go_json_api)

