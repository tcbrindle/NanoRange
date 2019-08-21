// Range v3 library
//
//  Copyright Eric Niebler 2014-present
//
//  Use, modification and distribution is subject to the
//  Boost Software License, Version 1.0. (See accompanying
//  file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//
// Project home: https://github.com/ericniebler/range-v3

#include <nanorange/views/istream.hpp>
#include <sstream>
#include "../catch.hpp"
#include "../test_utils.hpp"

using namespace nano;

namespace {

struct moveonly {
    char c;

    moveonly() = default;

    moveonly(moveonly&&) = default;

    moveonly& operator=(moveonly&&)& = default;

    operator char() const
    {
        return c;
    }

    friend std::istream& operator>>(std::istream& is, moveonly& m)
    {
        is.get(m.c);
        return is;
    }
};

}

static_assert(InputRange<ranges::basic_istream_view<int, char>>, "");
static_assert(InputRange<ranges::basic_istream_view<moveonly, char>>, "");

TEST_CASE("views.istream")
{
    {
        constexpr const char test[] = "abcd3210";
        std::istringstream ss{test};
        ::check_equal(ranges::istream_view<moveonly>(ss),
                      ranges::subrange(test, test + sizeof(test) - 1));
    }

    // Default-constructed istream_views are empty
    {
        ranges::basic_istream_view<int, char> isr{};
        CHECK(ranges::distance(isr) == 0);
    }

    // Default-constructed iterators can be compared
    {
        const auto iter = decltype(ranges::basic_istream_view<int, char>{}.begin()){};
        constexpr ranges::default_sentinel_t sent{};
        CHECK(iter == sent);
        CHECK(sent == iter);
        CHECK_FALSE(iter != sent);
        CHECK_FALSE(sent != iter);
    }
}
