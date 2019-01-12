
#include <benchmark/benchmark.h>

namespace {

void empty_benchmark(benchmark::State& state) {}
}

BENCHMARK(empty_benchmark);
