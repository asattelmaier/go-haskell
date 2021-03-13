cmake_minimum_required(VERSION 2.8.12)
project(GoHaskellRestApiTest)
enable_testing()

add_definitions("-std=c++11")

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

add_executable(
  go_haskell_rest_api_test

  go_haskell_rest_api_test.cpp

  rules/rule1_test.cpp
  #rules/rule2_test.cpp
  #rules/rule3_test.cpp
  #rules/rule4_test.cpp
  #rules/rule5_test.cpp
  #rules/rule6_test.cpp
  #rules/rule7_test.cpp
  #rules/rule8_test.cpp

  utils/rest_api.cpp
  utils/rest_api.h
  utils/board.cpp
  utils/board.h
)

target_link_libraries(go_haskell_rest_api_test ${CONAN_LIBS})

add_test(NAME go_haskell_rest_api_test WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin COMMAND go_haskell_rest_api_test)

