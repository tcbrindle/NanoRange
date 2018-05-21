
cmake_minimum_required(VERSION 3.1)

project(nanorange CXX)

set(CMAKE_CXX_EXTENSIONS Off)

add_library(nanorange INTERFACE)
target_sources(nanorange INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include/nanorange.hpp)
target_include_directories(nanorange INTERFACE ${CMAKE_CURRENT_SOURCE_DIR}/include)

if (CMAKE_COMPILER_IS_GNUCXX OR CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    target_compile_features(nanorange INTERFACE cxx_std_14)
endif()

if (MSVC)
    target_compile_options(nanorange INTERFACE /permissive- /std:c++latest)
endif()

add_subdirectory(test)
add_subdirectory(tools)
