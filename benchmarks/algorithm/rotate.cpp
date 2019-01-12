#include <nanorange/algorithm/rotate.hpp>

#include <algorithm>
#include <utility>
#include <vector>

#include <nanorange/algorithm/generate.hpp>

#include <benchmark/benchmark.h>

namespace {

void set_interesting_sizes(benchmark::internal::Benchmark* bench)
{
    static constexpr std::pair<int, int> cases[]{
        {1, 999},           // Special case for std
        {100, 900},         //
        {400, 600},         //
        {500, 500},         // Special case for std
        {700, 300},         //
        {999, 1},           // Special case for std
        {700'000, 350'000}, // Big gcd
        {30'000, 16'127},   // Coprime
    };

    for (auto c : cases) {
        bench->Args({c.first, c.second});
    }
}

template <typename T>
struct type_t {
    using type = T;
};

auto generator(type_t<int>)
{
    return [x = 0]() mutable { return x++; };
}

auto generator(type_t<std::string>)
{
    auto ints = generator(type_t<int>{});
    return [&]() mutable {
        int num = std::abs(ints() % ('z' - 'a'));
        return std::string(static_cast<size_t>(num), 'a' + num);
    };
}

template <typename F, typename T>
void rotate_random_access(benchmark::State& state)
{
    const std::size_t lhs = static_cast<std::size_t>(state.range(0)),
                      rhs = static_cast<std::size_t>(state.range(1));

    std::vector<T> v(lhs + rhs);
    nano::generate(v, generator(type_t<T>{}));

    for (auto _ : state) {
        benchmark::DoNotOptimize(F{}(v.begin(), v.begin() + lhs, v.end()));
    }
}

struct nano_rotate {
    template <typename I>
    I operator()(I f, I m, I l)
    {
        return nano::rotate(f, m, l).begin();
    }
};

struct std_rotate {
    template <typename I>
    I operator()(I f, I m, I l)
    {
        return std::rotate(f, m, l);
    }
};

} // namespace

// int -------------------------------------------------

BENCHMARK_TEMPLATE(rotate_random_access, nano_rotate, int)
    ->Apply(set_interesting_sizes);

BENCHMARK_TEMPLATE(rotate_random_access, std_rotate, int)
    ->Apply(set_interesting_sizes);

// string -------------------------------------------------

BENCHMARK_TEMPLATE(rotate_random_access, nano_rotate, std::string)
    ->Apply(set_interesting_sizes);

BENCHMARK_TEMPLATE(rotate_random_access, std_rotate, std::string)
    ->Apply(set_interesting_sizes);
