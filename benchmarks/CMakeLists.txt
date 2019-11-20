
find_package(benchmark REQUIRED)

if (NOT ${CMAKE_BUILD_TYPE} STREQUAL "Release")
    message(WARNING "Benchmarking enabled, but this is not a release build!")
endif()

function(add_benchmark NAME PATH)
    add_executable(${NAME} ${PATH} benchmark_main.cpp)
    target_link_libraries(${NAME} PRIVATE nanorange benchmark::benchmark)
    if (CMAKE_COMPILER_IS_GNUCXX)
        target_compile_options(${NAME} PRIVATE -march=native)
    endif()
    if (CMAKE_CXX_COMPILER_ID MATCHES "Clang")
        target_compile_options(${NAME} PRIVATE -march=native -mllvm -align-all-blocks=5)
    endif()
endfunction(add_benchmark)

add_benchmark(benchmark_rotate algorithm/rotate.cpp)
