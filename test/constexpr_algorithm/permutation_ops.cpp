
#include <nanorange/algorithm.hpp>

#include "constexpr_array.hpp"

namespace {

constexpr bool test_next_permutation()
{
    constexpr carray<int, 3> result{1, 2, 3};

    {
        carray<int, 3> arr{3, 2, 1};
        nano::next_permutation(arr.begin(), arr.end());
        if (arr != result) {
            return false;
        }
    }

    {
        carray<int, 3> arr{3, 2, 1};
        nano::next_permutation(arr);
        if (arr != result) {
            return false;
        }
    }

    return true;
}
static_assert(test_next_permutation(), "");

constexpr bool test_prev_permutation()
{
    constexpr carray<int, 3> result{3, 2, 1};

    {
        carray<int, 3> arr{1, 2, 3};
        nano::prev_permutation(arr.begin(), arr.end());
        if (arr != result) {
            return false;
        }
    }

    {
        carray<int, 3> arr{1, 2, 3};
        nano::prev_permutation(arr);
        if (arr != result) {
            return false;
        }
    }

    return true;
}
static_assert(test_prev_permutation(), "");


}