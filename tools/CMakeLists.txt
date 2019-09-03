
add_executable(make_single_header make_single_header.cpp)
target_compile_features(make_single_header PRIVATE cxx_std_17)
target_link_libraries(make_single_header PRIVATE nanorange)

if (CMAKE_COMPILER_IS_GNUCXX)
  target_link_libraries(make_single_header PRIVATE stdc++fs)
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  target_link_libraries(make_single_header PRIVATE c++fs)
endif()
