
#include <nanorange/algorithm.hpp>

#include "constexpr_array.hpp"

namespace {

constexpr bool test_copy()
{
    constexpr carray<int, 5> src{1, 2, 3, 4, 5};
    {
        int dest[5] = {};
        auto res = nano::copy(src.begin(), src.end(), dest);
        if (res.in != src.end()) {
            return false;
        }
        if (res.out != dest + 5) {
            return false;
        }
    }
    {
        int dest[5] = {};
        auto res = nano::copy(src, dest);
        if (res.in != src.end()) {
            return false;
        }
        if (res.out != dest + 5) {
            return false;
        }
    }

    return true;
}
static_assert(test_copy(), "");

constexpr bool test_unique_copy()
{
    constexpr carray<int, 5> in{1, 2, 2, 2, 3};
    constexpr carray<int, 3> result{1, 2, 3};

    {
        carray<int, 3> out{};

        const auto r = nano::unique_copy(in.begin(), in.end(), out.begin());
        if (r.in != in.end()) {
            return false;
        }
        if (r.out != out.end()) {
            return false;
        }
        if (out != result) {
            return false;
        }
    }

    {
        carray<int, 3> out{};

        const auto r = nano::unique_copy(in, out.begin());
        if (r.in != in.end()) {
            return false;
        }
        if (r.out != out.end()) {
            return false;
        }
        if (out != result) {
            return false;
        }
    }

    return true;
}
static_assert(test_unique_copy(), "");

}