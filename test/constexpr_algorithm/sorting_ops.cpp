
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

}
