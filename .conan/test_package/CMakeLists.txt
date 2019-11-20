cmake_minimum_required(VERSION 2.8.12)
project(test_package CXX)

include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup(TARGETS)

find_package(nanorange REQUIRED CONFIG)

add_executable(test_package test_package.cpp)
target_link_libraries(test_package nanorange::nanorange)
