
#include <nanorange/algorithm.hpp>

#include "constexpr_array.hpp"

namespace {

constexpr bool test_copy()
{
    constexpr carray<int, 5> src{1, 2, 3, 4, 5};
    {
        int dest[5] = {};
        auto res = nano::copy(src.begin(), src.end(), dest);
        if (res.first != src.end()) {
            return false;
        }
        if (res.second != dest + 5) {
            return false;
        }
    }
    {
        int dest[5] = {};
        auto res = nano::copy(src, dest);
        if (res.first != src.end()) {
            return false;
        }
        if (res.second != dest + 5) {
            return false;
        }
    }

    return true;
}
static_assert(test_copy(), "");


}