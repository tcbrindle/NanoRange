
if (NOT ${ENABLE_BENCHMARKS})
    return()
endif()

find_package(benchmark REQUIRED)

if (NOT ${CMAKE_BUILD_TYPE} STREQUAL "Release")
    message(WARNING "Benchmarking enabled, but this is not a release build!")
endif()

function(add_benchmark NAME PATH)
    add_executable(${NAME} ${PATH} benchmark_main.cpp)
    target_link_libraries(${NAME} PRIVATE nanorange benchmark::benchmark)
endfunction(add_benchmark)

add_benchmark(benchmark_rotate algorithm/rotate.cpp)
