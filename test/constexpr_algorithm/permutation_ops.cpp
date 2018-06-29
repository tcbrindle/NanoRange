
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

// FIXME: MSVC doesn't like this test, complaining about a read of a variable
// outside its lifetime.
// TODO: Work out if this is a real problem or just MSVC being weird
#ifndef _MSC_VER
constexpr bool test_is_permutation()
{
    constexpr carray<int, 3> arr1{1, 2, 3};
    constexpr carray<int, 3> arr2{3, 2, 1};
    constexpr carray<int, 3> arr3{1, 2, 99};
    constexpr carray<int, 1> arr4{0};

    // Four-legged
    {
        static_assert(nano::is_permutation(arr1.begin(), arr1.end(),
                                           arr2.begin(), arr2.end()), "");
        static_assert(!nano::is_permutation(arr1.begin(), arr1.end(),
                                            arr3.begin(), arr3.end()), "");
        static_assert(!nano::is_permutation(arr1.begin(), arr1.end(),
                                            arr4.begin(), arr4.end()), "");
    }

    // Two ranges
    {
        static_assert(nano::is_permutation(arr1, arr2), "");
        static_assert(!nano::is_permutation(arr1, arr3), "");
        static_assert(!nano::is_permutation(arr1, arr4), "");
    }

    // Three-legged
    {
        static_assert(nano::is_permutation(arr1.begin(), arr1.end(),
                                           arr2.begin()), "");
        static_assert(!nano::is_permutation(arr1.begin(), arr1.end(),
                                            arr3.begin()), "");
    }

    // Range and a half
    {
        static_assert(nano::is_permutation(arr1, arr2.begin()), "");
        static_assert(!nano::is_permutation(arr1, arr3.begin()), "");
    }

    return true;
}
static_assert(test_is_permutation(), "");
#endif

}