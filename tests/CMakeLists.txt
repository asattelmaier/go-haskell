cmake_minimum_required(VERSION 2.8.12)
project(GoHaskellTests)
enable_testing()

add_definitions("-std=c++11")

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

add_executable(tests tests.cpp)
target_link_libraries(tests ${CONAN_LIBS})

add_test(NAME tests WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin COMMAND tests)
