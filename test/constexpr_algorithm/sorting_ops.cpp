
#include <nanorange/algorithm.hpp>
#include "constexpr_array.hpp"

namespace {

constexpr bool test_sort()
{
    {
        carray<int, 5> arr{5, 4, 3, 2, 1};
        nano::sort(arr);
        if (!nano::is_sorted(arr)) {
            return false;
        }
    }

    {
        // large-ish array to exercise a different sort code path
        constexpr int sz = 1'000;
        carray<int, sz> arr{};
        for (int i = 0; i < sz; i++) {
            arr[(size_t)i] = i;
        }
        nano::sort(arr, nano::greater<>{});
        return nano::is_sorted(arr, nano::greater<>{});
    }
}
static_assert(test_sort(), "");

constexpr bool test_nth_element()
{
    {
        carray<int, 9> arr{5, 8, 4, 7, 2, 1, 0, 6, 3};
        const auto ret = nano::nth_element(arr, arr.begin() + 5);
        if (ret != arr.end()) {
            return false;
        }
        if (arr[5] != 5) {
            return false;
        }
    }

    {
        carray<int, 9> arr{5, 8, 4, 7, 2, 1, 0, 6, 3};
        const auto ret = nano::nth_element(arr.begin(), arr.begin() + 5, arr.end());
        if (ret != arr.end()) {
            return false;
        }
        if (arr[5] != 5) {
            return false;
        }
    }
    return true;
}
static_assert(test_nth_element(), "");

}
