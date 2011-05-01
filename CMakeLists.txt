cmake_minimum_required(VERSION 2.8.0)

set (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake" 
  ${CMAKE_MODULE_PATH})

project(evolver)

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)

add_subdirectory(src)
