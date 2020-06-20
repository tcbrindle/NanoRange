
#include <nanorange/algorithm.hpp>

#include "constexpr_array.hpp"

namespace {

struct sum_fn {
    int sum = 0;
    constexpr void operator() (int e) {
        sum += e;
    }
};

constexpr bool test_for_each()
{
    constexpr carray<int, 3> arr{3, 2, 1};

    {
        auto result = nano::for_each(arr.begin(), arr.end(), sum_fn{});
        if (result.fun.sum != 6) {
            return false;
        }
    }

    {
        auto result = nano::for_each(arr, sum_fn{});
        if (result.fun.sum != 6) {
            return false;
        }
    }

    {
        auto result = nano::for_each(arr, sum_fn{}, [](int e) { return -e; });
        if (result.fun.sum != -6) {
            return false;
        }
    }

    return true;
}
static_assert(test_for_each(), "");

constexpr bool test_for_each_n()
{
    constexpr carray<int, 3> arr{3, 2, 1};

    {
        auto result = nano::for_each_n(arr.begin(), 2, sum_fn{}, [](int e) { return -e; });
        if (result.fun.sum != -5) {
            return false;
        }
    }
    return true;
}
static_assert(test_for_each_n(), "");

constexpr bool test_clamp()
{
    if (nano::clamp(1, 2, 5) != 2) {
        return false;
    }
    if (nano::clamp(3, 2, 5) != 3) {
        return false;
    }
    if (nano::clamp(7, 2, 5) != 5) {
        return false;
    }
    if (nano::clamp(3, 2, 5, nano::greater{}, [](int i) { return -i; }) != 3) {
        return false;
    }
    return true;
}
static_assert(test_clamp(), "");

}
