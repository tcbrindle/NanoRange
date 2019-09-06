// test/views/iota.cpp
//
// Copyright (c) 2019 Tristan Brindle (tcbrindle at gmail dot com)
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include <nanorange/algorithm/for_each.hpp>
#include <nanorange/views/iota.hpp>
#include <nanorange/views/counted.hpp>
#include <nanorange/views/reverse.hpp>

#include "../catch.hpp"
#include "../test_utils.hpp"

#include <vector>

TEST_CASE("views.iota")
{
    {
        constexpr auto rng = nano::views::iota(0, 10);

        static_assert(nano::random_access_range<decltype(rng)>);

        static_assert(nano::distance(rng) == 10);
        static_assert(nano::size(rng) == 10);
        static_assert(!rng.empty());

        ::check_equal(rng, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
        ::check_equal(rng | nano::views::reverse,
                      {9, 8, 7, 6, 5, 4, 3, 2, 1, 0});


        constexpr auto it1 = nano::begin(rng);
        constexpr auto it2 = nano::end(rng);
        static_assert(it1 == it1);
        static_assert(it1 != it2);
        static_assert(it1 < it2);
        static_assert(it2 > it1);
        static_assert(it1 <= it2);
        static_assert(it2 >= it1);
        static_assert(it1[2] == 2);
        static_assert(*(it1 + 1) == 1);
        static_assert(*(it2 - 1) == 9);

        constexpr auto compound_assignment_test = [rng] {
            auto it = rng.begin();
            it += 3;
            if (*it != 3) {
                return false;
            }
            it -= 3;
            if (it != rng.begin()) {
                return false;
            }
            return true;
        };

        static_assert(compound_assignment_test());
    }

    {
        auto rng = nano::views::iota(0);
        ::check_equal(nano::views::counted(nano::begin(rng), 10),
                      {0, 1, 2, 3, 4, 5, 6, 7, 8, 9});
    }

    {
        // Testing iota_view with something that isn't integral
        std::vector vec{1, 2, 3, 4, 5};
        auto rng = nano::views::iota(vec.begin(), vec.begin() + 3);

        nano::for_each(rng, [](auto i) {
            *i = 0;
        });

        ::check_equal(vec, {0, 0, 0, 4, 5});
    }

    {
        // Check conversion to vector
        auto rng = nano::views::iota(0, 5);
        static_assert(nano::common_range<decltype(rng)>);
        auto vec = std::vector<int>(rng.begin(), rng.end());
        ::check_equal(vec, {0, 1, 2, 3, 4});
    }
}